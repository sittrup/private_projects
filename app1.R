library(shiny)
library(tidyverse)
library(tidymodels)
library(vip)
library(DT)
library(forcats)
library(scales)

# --- GLOBAL SETUP -------------------------------------------------------------
setwd("~/Desktop/Eksamen 2. semester")
source("~/Desktop/Eksamen 2. semester/LogRegTilApp3.R")  # definerer master_final, train_data, test_data, log_final_model, log_pred


# Data preparation
nf <- master_final


# Tilføj markering for event-deltagelse
events_data <- readRDS("~/Desktop/Eksamen 2. semester/events.rds")  # opdater evt. stien
event_cvr <- unique(events_data$CVR)
nf <- nf %>%
  mutate(event_deltagelse = if_else(`CVR-nr.` %in% event_cvr, "Deltaget", "Ikke deltaget"))

members <- nf %>% filter(churnet == "no")
probs <- suppressWarnings(predict(log_final_model, new_data = members, type = "prob"))
pred_probs <- bind_cols(
  members %>% select(CVR = `CVR-nr.`, starts_with("Branche_"), Mindre_Virksomhed:Større_Virksomhed),
  probs
)

# Pre-compute metrics for "Ledelse"
branche_counts <- members %>%
  pivot_longer(starts_with("Branche_"), names_to = "branche_type", values_to = "has_branch") %>%
  filter(has_branch == 1) %>% count(branche_type) %>%
  mutate(branche_type = str_remove(branche_type, "^Branche_"))
str_counts <- members %>%
  pivot_longer(c(Mindre_Virksomhed, Mellem_Str._Virksomhed, Større_Virksomhed),
               names_to = "virksomhedsstr", values_to = "has_str") %>%
  filter(has_str == 1) %>% count(virksomhedsstr)
branche_risk <- pred_probs %>%
  pivot_longer(starts_with("Branche_"), names_to = "branche_type", values_to = "has_branch") %>%
  filter(has_branch == 1) %>%
  group_by(branche_type) %>% summarise(avg_risk = mean(.pred_yes), .groups = "drop") %>%
  mutate(branche_type = str_remove(branche_type, "^Branche_"))

# _______________ Events

# Læs event-data 
events_data <- readRDS("~/Desktop/Eksamen 2. semester/events.rds")  # Ret sti hvis nødvendigt

# Find CVR'er som deltager i events
cvr_event_deltagere <- unique(events_data$`CVR-nr.`)

# Alle brancher (fra master_final)
all_branches <- master_final %>%
  select(starts_with("Branche_")) %>%
  pivot_longer(everything(), names_to = "branche", values_to = "has_branch") %>%
  filter(has_branch == 1) %>%
  distinct(branche) %>%
  mutate(branche = str_remove(branche, "^Branche_"))

# Brancher der deltager i events
branche_event_deltagere <- master_final %>%
  filter(`CVR-nr.` %in% cvr_event_deltagere) %>%
  select(starts_with("Branche_")) %>%
  pivot_longer(everything(), names_to = "branche", values_to = "has_branch") %>%
  filter(has_branch == 1) %>%
  distinct(branche) %>%
  mutate(branche = str_remove(branche, "^Branche_"))

# Brancher uden deltagelse
brancher_uden_deltagelse <- all_branches %>%
  filter(!(branche %in% branche_event_deltagere$branche))

# Plot data
branche_plot_data <- master_final %>%
  mutate(Deltager = ifelse(`CVR-nr.` %in% cvr_event_deltagere, "Ja", "Nej")) %>%
  pivot_longer(starts_with("Branche_"), names_to = "branche", values_to = "has_branch") %>%
  filter(has_branch == 1) %>%
  mutate(branche = str_remove(branche, "^Branche_")) %>%
  count(branche, Deltager)

event_status_df <- master_final %>%
  mutate(deltager = ifelse(`CVR-nr.` %in% events_data$`CVR-nr.`, "Deltager", "Ikke deltager")) %>%
  select(`CVR-nr.`, churnet, deltager)

# Opgør churn vs event-deltagelse
churn_event_summary <- event_status_df %>%
  count(churnet, deltager) %>%
  group_by(deltager) %>%
  mutate(andel = n / sum(n))

# --- UI ----------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Business Viborg Churn Dashboard"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("role", "Vælg rolle:", choices = c("Medarbejder", "Ledelse")),
      conditionalPanel(
        condition = "input.role == 'Medarbejder'",
        numericInput("topN", "Antal højrisiko virksomheder:", value = 10, min = 1)
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Churn-risiko",
                 conditionalPanel(
                   condition = "input.role == 'Medarbejder'",
                   h4("Medlemmer med højrisiko for churn"),
                   DTOutput("topRiskDT"),
                   h5("Klik på en række for detaljer:"),
                   DTOutput("memberDetails"),
                   h4("Top 20 faktorer churn vægtes efter"),
                   plotOutput("vipPlotMedarbejder")
                 ),
                 conditionalPanel(
                   condition = "input.role == 'Ledelse'",
                   fluidRow(
                     column(6, h4("Antal medlemmer"), textOutput("totalMembers")),
                     column(6, h4("Gns. churn-risiko"), textOutput("avgRisk"))
                   )
                 )
        ),
        # _______________ Events

tabPanel("Events",
         
uiOutput("forklaring"),
         
  
  DTOutput("brancherUdenEvent"),
  
  h4("Sammenhæng: Event-deltagelse og churn"),
  plotOutput("churnEventPlot"),
  
  h4("Event-deltagelse per branche"),
  plotOutput("brancheEventCountPlot")                  
                
        ),
        tabPanel("Møder",
                 h4("Møde-data her")
        )
      )
    )
  )
)

# --- SERVER ------------------------------------------------------------------
server <- function(input, output, session) {
  # Medarbejder: reactive top N
  top_dt <- reactive({
    pred_probs %>%
      arrange(desc(.pred_yes)) %>%
      slice_head(n = input$topN) %>%
      select(CVR, starts_with("Branche_"), Mindre_Virksomhed:Større_Virksomhed, risk = .pred_yes) %>%
      pivot_longer(starts_with("Branche_"), names_to = "branche_type", values_to = "has_branch") %>%
      filter(has_branch == 1) %>%
      pivot_longer(c(Mindre_Virksomhed, Mellem_Str._Virksomhed, Større_Virksomhed), names_to = "virksomhedsstr", values_to = "has_str") %>%
      filter(has_str == 1) %>%
      mutate(
        branche_type = str_remove(branche_type, "^Branche_"),
        `Risk (%)` = paste0(round(risk * 100), "%")
      ) %>%
      select(CVR, branche_type, virksomhedsstr, `Risk (%)`)
  })
  output$topRiskDT <- renderDT({ top_dt() }, selection = 'single', options = list(pageLength = input$topN))
  output$memberDetails <- renderDT({
    sel <- input$topRiskDT_rows_selected
    if (length(sel) == 0) return(NULL)
    top_dt()[sel, , drop = FALSE]
  }, options = list(dom = 't', paging = FALSE))
  output$vipPlotMedarbejder <- renderPlot({
    glm_fit <- extract_fit_parsnip(log_final_model)$fit
    vip(glm_fit, num_features = 20, geom = "col") + ggtitle("Top 20 faktorer churn vægtes efter") + theme_minimal()
  })
  
  
  # Ledelse: KPI'er
  output$totalMembers <- renderText({ format(nrow(members), big.mark = ".", decimal.mark = ",") })
  output$avgRisk <- renderText({ sprintf("%.1f%%", mean(probs$.pred_yes) * 100) })
  
  # Forklaring på plots
  output$forklaring <- renderUI({
    div(style = "background-color: #1b5a59; padding: 15px; border-radius: 10px; color: white;",
        HTML("
        <h4 style='color: #7fdd9e;'>📊 Hvordan aflæses graferne?</h4>
        <p><b>1. Sammenhæng mellem churn og event-deltagelse:</b><br>
        Grafen viser, hvordan eventdeltagelse hænger sammen med churn. 
        Hver søjle viser, hvor mange virksomheder der har deltaget eller ikke deltaget i events – opdelt efter om de er churnet eller ej.
        Den bruges til at vurdere, om eventdeltagelse er forbundet med lavere churn.</p>

        <p><b>2. Brancher og churn-risiko:</b><br>
        Denne graf viser, hvor mange virksomheder i hver branche der deltager i events. Inde i hver søjle står, hvor stor en andel af virksomhederne 
        i branchen der er i risiko for churn (fx '25% i risiko'). Brug den til at identificere brancher med høj churn-risiko – især hvis de ikke deltager i events.</p>
      ")
    )
  })
  
  
    
  output$churnEventPlot <- renderPlot({
    nf %>%
      pivot_longer(
        cols = c("Ingen_Events_Deltagelse", "Højst_To_Events_Deltagelse", "Højst_Fem_Events_Deltagelse", "Flere_End_Fem_Events_Deltagelse"),
        names_to = "event_kategori", 
        values_to = "har_deltaget"
      ) %>%
      filter(har_deltaget == 1) %>%
      count(event_kategori, churnet) %>%
      mutate(
        event_kategori = factor(event_kategori, levels = c("Ingen_Events_Deltagelse", "Højst_To_Events_Deltagelse", "Højst_Fem_Events_Deltagelse", "Flere_End_Fem_Events_Deltagelse"),
                                labels = c("Ingen", "Højst 2", "Højst 5", "Flere end 5")),
        churnet = factor(churnet, levels = c("no", "yes"), labels = c("Ikke churnet", "Churnet"))
      ) %>%
      ggplot(aes(x = event_kategori, y = n, fill = churnet)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      geom_text(aes(label = n), 
                position = position_dodge(width = 0.8), 
                vjust = -0.4, 
                color = "black", size = 4) +
      scale_fill_manual(
        values = c("Ikke churnet" = "#1b5a59", "Churnet" = "#7fdd9e")
      ) +
      labs(
        title = "Churn i forhold til eventdeltagelse",
        x = "Event-deltagelse (antal events)",
        y = "Antal virksomheder",
        fill = "Status"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold"),
        legend.position = "top"
      )
  })
  
  output$brancheEventCountPlot <- renderPlot({
    # Opret data for event-deltagelse per branche og churn-status
    branche_event_data <- nf %>%
      mutate(event_deltagelse = ifelse(`CVR-nr.` %in% cvr_event_deltagere, "Deltaget", "Ikke deltager")) %>%
      pivot_longer(starts_with("Branche_"), names_to = "branche", values_to = "has_branch") %>%
      filter(has_branch == 1) %>%
      count(branche, event_deltagelse, churnet) %>%
      mutate(branche = str_remove(branche, "^Branche_"))  # Ryd op i branche-navnene
    
    # Beregn procentdelen af virksomheder med churn-risiko i hver branche
    branche_event_data <- branche_event_data %>%
      group_by(branche, event_deltagelse) %>%
      mutate(total = sum(n)) %>%
      ungroup() %>%
      mutate(procent = (n / total) * 100)
    
    # Returnér plot
    ggplot(branche_event_data, aes(x = fct_reorder(branche, n), y = n, fill = interaction(event_deltagelse, churnet))) +
      geom_bar(stat = "identity", position = "stack") +
      coord_flip() +
      labs(
        title = "Procent af virksomheder med churn-risiko per branche",
        x = "Branche",
        y = "Antal virksomheder",
        fill = "Event-deltagelse og Churn"
      ) +
      scale_fill_manual(
        values = c(
          "Deltaget.no" = "#405f63",      # Grøn for deltager og ikke churnet
          "Deltaget.yes" = "#1b5a59",     # Mørk grå for deltager og churnet
          "Ikke deltager.no" = "#7fdd9e", # Lys grøn for ikke deltager og ikke churnet
          "Ikke deltager.yes" = "#cbd726"  # Gul for ikke deltager og churnet
        ),
        labels = c(
          "Deltaget og ikke churnet",
          "Deltaget og churnet",
          "Ikke deltager og ikke churnet",
          "Ikke deltager og churnet"
        )
      ) +  # Tildel specifikke farver til de kombinerede kategorier
      geom_text(aes(label = paste0(round(procent, 1), "%")), position = position_stack(vjust = 0.5), color = "white") + # Procentdel i bjælkerne
      theme_minimal(base_family = "sans") +
      theme(
        plot.background = element_rect(fill = "#1b5a59", color = NA),
        panel.background = element_rect(fill = "#1b5a59", color = NA),
        legend.background = element_rect(fill = "#1b5a59"),
        legend.key = element_rect(fill = "#1b5a59"),
        legend.text = element_text(color = "#ffffff"),
        legend.title = element_text(color = "#ffffff"),
        axis.text.x = element_text(color = "#ffffff"),
        axis.text.y = element_text(color = "#ffffff"),
        axis.title = element_text(color = "#ffffff"),
        plot.title = element_text(color = "#ffffff", face = "bold"),
        legend.position = "top"
      )
  })
  
  # Forklaring der kan inkluderes i appen:
  output$explanation <- renderText({
    "Plottet viser, hvordan risikoen for churn fordeler sig i de enkelte brancher baseret på event-deltagelse.
   - Hver bjælke repræsenterer en branche og viser antallet af virksomheder, der har deltager i events (Deltaget) eller ikke deltager i events (Ikke deltager).
   - Farverne angiver, om virksomheden er churnet (gul for churnet, grøn for ikke churnet).
   - Procentdelen af churn-risiko i hver branche vises i bjælkerne, så du kan hurtigt se, hvor stor en del af virksomhederne i en given branche, der er i risiko for at churner."
  })
  
  
  # Evt. andre server-funktioner...
  
  
}

# Run the app
shinyApp(ui, server)

