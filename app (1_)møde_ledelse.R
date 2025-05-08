required_packages <- c("shiny", "tidyverse", "tidymodels", "DT", "vip", "gridExtra", "grid", "shadowtext", "gt", "markdown")

for(pkg in required_packages) {
    if (!require(pkg, character.only = TRUE)) {
        install.packages(pkg)
        library(pkg, character.only = TRUE)
    }
}

# Stop eventuelle kørende Shiny apps
if (!is.null(shiny::getDefaultReactiveDomain())) {
    shiny::stopApp()
}

# Ryd environment
rm(list = ls())

# --- Libraries --- 
library(shiny)
library(tidyverse)
library(tidymodels)
library(tibble)
library(DT)
library(vip)
library(gridExtra)
library(grid) 
library(shadowtext)
library(gt)
library(markdown)
library(ggplot2)
library(dplyr)


# --- Data setup ---
setwd("C:/Users/ander/OneDrive/Desktop/2_semester_projekt")
source("LogRegTilApp3.R")

nf <- master_final
members <- nf %>% filter(churnet == "no")
probs <- suppressWarnings(predict(log_final_model, new_data = members, type = "prob"))
pred_probs <- bind_cols(
  members %>% select(CVR = `CVR-nr.`, starts_with("Branche_"), Mindre_Virksomhed:Større_Virksomhed),
  probs
)


# Beregn møde antal og mødelængde
members <- members %>%
  mutate(
    antal_møder = case_when(
      Flere_End_Fem_Møder == 1 ~ "Flere end 5 møder",
      Højst_Fem_Møder == 1 ~ "1-5 møder",
      Et_Møde == 1 ~ "1 møde",
      Ingen_Møder == 1 ~ "0 møder",
      TRUE ~ "Ukendt"
    ),
    mødelængde_tid = case_when(  # Ændret til 'mødelængde_tid'
      Møde_Længde_Under60min == 1 ~ "Under 60 min",
      Møde_Længde_60min == 1 ~ "60 min",
      Møde_Længde_75min == 1 ~ "75 min",
      Møde_Længde_90min == 1 ~ "90 min",
      Møde_Længde_Over90min == 1 ~ "Over 90 min",
      Ingen_Mødelængde == 1 ~ "Ingen mødelængde",
      TRUE ~ "Ukendt"
    )
  )

ui <- fluidPage(
  titlePanel("Business Viborg Churn Dashboard"),

  radioButtons("role", "Vælg rolle:", choices = c("Medarbejder", "Ledelse"), selected = "Ledelse"),

  sidebarLayout(
    sidebarPanel(
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
        tabPanel("Events",
          h4("Event-data her")
        ),
        tabPanel("Møder",
          conditionalPanel(
            condition = "input.role == 'Ledelse'",
            tagList(
              tags$style(HTML("
                .kpi-container {
                    display: flex;
                    justify-content: space-between;
                    gap: 20px;
                    margin-bottom: 20px;
                    min-height: 120px;  /* Increased height */
                }
                .kpi-box {
                    background-color: #ffffff;
                    border-radius: 12px;
                    padding: 25px;  /* Increased padding */
                    box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
                    text-align: center;
                    flex: 1;
                    overflow: hidden;
                    display: flex;
                    flex-direction: column;
                    justify-content: center;
                }
                .kpi-title {
                    font-size: 18px;  /* Increased font size */
                    color: #1b5a59;
                    margin-bottom: 15px;  /* Added space between title and value */
                    font-weight: bold;
                }
                .kpi-value {
                    font-size: 24px;  /* Increased font size */
                    color: #1b5a59;
                    font-weight: bold;
                }
                .chart-container {
                    background-color: #f8f8f8;
                    border-radius: 12px;
                    padding: 20px;
                    margin-bottom: 20px;
                }
                .chart-title {
                    color: #1b5a59;
                    font-size: 24px;
                    font-weight: bold;
                    text-align: center;
                    margin-bottom: 20px;
                }
              ")),
              div(class = "kpi-container",
                div(class = "kpi-box",
                  div(class = "kpi-title", "Gns. antal møder pr. virksomhed"),
                  div(class = "kpi-value", textOutput("avgMeetings"))
                ),
                div(class = "kpi-box",
                  div(class = "kpi-title", "Andel uden møder"),
                  div(class = "kpi-value", textOutput("noMeetingsPct"))
                ),
                div(class = "kpi-box",
                  div(class = "kpi-title", "Antal uden møde"),
                  div(class = "kpi-value", textOutput("antalUdenMoeder"))
                )
              ),
              br(),
              div(class = "chart-container",
                div(class = "chart-title", "Antal møder"),

                div(class = "kpi-container",
                  div(class = "kpi-box",
                    style = "flex: 1;",
                    plotOutput("moedeVsChurnBar", height = "400px")
                  ),
                  div(class = "kpi-box",
                    style = "flex: 1;",
                    plotOutput("moedeVsChurnDonut", height = "400px")
                  )
                ),
                div(class = "kpi-container",
                div(class = "kpi-box",
                style = "flex: 1;",
                plotOutput("churnVsMeetingCount", height = "400px")
                 )
                )
              ),
              div(class = "chart-container",
                div(class = "chart-title", "Mødelængde"),
                div(class = "kpi-container",
                  div(class = "kpi-box",
                    style = "flex: 1;",
                    plotOutput("moedeFordeling", height = "400px")
                  ),
                  div(class = "kpi-box",
                    style = "flex: 1;",
                    gt_output("mødelængdeTabel")
                  )
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.role == 'Medarbejder'",
            h4("Denne visning er kun tilgængelig for ledelsen.")
          )
        )
      )
    )
  )
)

# --- Server --- 
server <- function(input, output, session) {
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
    vip(glm_fit, num_features = 20, geom = "col") +
      ggtitle("Top 20 faktorer churn vægtes efter") +
      theme_minimal()
  })

  output$avgMeetings <- renderText({
    avg <- with(members,
      mean(
        Et_Møde * 1 +
        Højst_Fem_Møder * 3 +
        Flere_End_Fem_Møder * 6,
        na.rm = TRUE
      )
    )
    paste0(round(avg, 1), " møder")
  })

  output$noMeetingsPct <- renderText({
    pct <- mean(members$Ingen_Møder == 1, na.rm = TRUE)
    paste0(round(pct * 100, 1), "%")
  })

  output$antalUdenMoeder <- renderText({
    sum(members$Ingen_Møder == 1, na.rm = TRUE)
  })

  output$totalMembers <- renderText({
    nrow(members)
  })

  output$moedeVsChurnDonut <- renderPlot({
    meet_summary <- pred_probs %>%
        left_join(members %>% select(CVR = `CVR-nr.`, antal_møder), by = "CVR") %>%
        filter(!is.na(antal_møder)) %>%
        group_by(antal_møder) %>%
        summarise(
            churn = mean(.pred_yes, na.rm = TRUE),
            count = n(),
            .groups = "drop"
        ) %>%
        mutate(
            pct = count / sum(count),
            ymin = lag(cumsum(pct), default = 0),
            ymax = cumsum(pct),
            ymid = (ymin + ymax) / 2,
            xmid = 3.5,  # Adjusted to move closer to the center
            label_inside = scales::percent(churn, accuracy = 1),
            fill_color = case_when(
                antal_møder == "0 møder" ~ "#1b5a59",
                antal_møder == "1 møde" ~ "#4d867a",
                antal_møder == "1-5 møder" ~ "#7fdd9e",
                antal_møder == "Flere end 5 møder" ~ "#b3c9c1",
                TRUE ~ "gray"
            )
        )

    tekst_forklaring <- tibble(
  y = seq(0.7, 0.7 + -0.15 * 3, by = -0.15),
  tekst = c(
    "49% churn-risiko 0 møder",
    "24% churn-risiko 1 møde",
    "14% churn-risiko 1-5 møder",
    "9% churn-risiko >5 møder"
  ),
  farve = c("#1b5a59", "#4d867a", "#7fdd9e", "#b3c9c1"),
  prik = "●"
)

p3 <- ggplot(tekst_forklaring, aes(y = y)) +
  # Prik med farve fra `farve`
  geom_text(aes(x = 1.3, label = prik, color = farve), size = 6) +
  
  # Teksten adskilt og med ensfarvet grågrøn tekst
  geom_text(aes(x = 1.5, label = tekst), hjust = 0, size = 6, color = "#1b5a59") +
  
  scale_color_identity() +
  xlim(1.3, 4) +
  ylim(0.1, 0.9) +
  theme_void()


    p1 <- ggplot(meet_summary) +
        ggtitle("Churn-risiko fordelt på antal møder") +
        geom_rect(aes(xmin = 2.8, xmax = 6.0, ymin = ymin, ymax = ymax,    
                    fill = fill_color), color = "white", size = 0.5) +      
        scale_fill_identity() +
        coord_polar(theta = "y") +
        xlim(-2.0, 6.5) +                                                   
        theme_void() +
        geom_text(aes(x = 4, y = ymid, label = label_inside),           
                  color = "white", fontface = "bold", size = 6) +          
        theme(
            plot.margin = margin(t = -50, r = 100, b = 20, l = 5),         # Added negative left margin
            plot.title = element_text(
                hjust = 0.01,                                                 
                size = 20,
                face = "bold",
                color = "#1b5a59",
                margin = margin(b = 50)
            )
        )

    gridExtra::grid.arrange(p1, p3, ncol = 2, widths = c(0.8, 1.2))         # Adjusted width ratio
}, height = 400, width = 900)


# Opret plot med churn og mødeaktivitet
output$churnVsMeetingCount <- renderPlot({
  nf %>%
    mutate(
      antal_møder = case_when(
        Flere_End_Fem_Møder == 1 ~ ">5 møder",
        Højst_Fem_Møder == 1 ~ "4 møder",
        Et_Møde == 1 ~ "1 møde",
        Ingen_Møder == 1 ~ "0 møder",
        TRUE ~ NA_character_
      ),
      churn_label = recode(churnet, "yes" = "Churn", "no" = "Aktive")
    ) %>%
    filter(!is.na(antal_møder)) %>%
    count(antal_møder, churn_label) %>%
    group_by(antal_møder) %>%
    mutate(
      pct = n / sum(n),
      label_text = paste0(round(pct * 100, 1), "%")
    ) %>%
    ungroup() %>%
    mutate(antal_møder = factor(antal_møder, levels = c("0 møder", "1 møde", "4 møder", ">5 møder"))) %>%
    ggplot(aes(x = antal_møder, y = pct, fill = churn_label)) +
    geom_col(position = "dodge", width = 0.6) +
    geom_text(
      aes(label = label_text),
      position = position_dodge(width = 0.6),
      vjust = -0.5,
      size = 6,
      color = "#1b5a59"
    ) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, 1.1),
      expand = expansion(mult = c(0, 0.05))
    ) +
    scale_fill_manual(values = c("Churn" = "#1b5a59", "Aktive" = "#7fdd9e")) +
    labs(
      title = "Andel churn og aktive pr. antal møder",
      x = "Antal møder",
      y = "Andel (%)",
      fill = "Status"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(
        hjust = 0,
        face = "bold",
        color = "#1b5a59",
        size = 20,
        margin = margin(b = 10)
      ),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      axis.title = element_text(size = 14, face = "bold"),
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 12),
      panel.grid.major.x = element_blank(),
      plot.margin = margin(10, 10, 10, 10)
    )
})

  output$moedeVsChurnBar <- renderPlot({
    meet_summary <- pred_probs %>%
      left_join(members %>% select(CVR = `CVR-nr.`, antal_møder), by = "CVR") %>%
      filter(!is.na(antal_møder)) %>%
      group_by(antal_møder) %>%
      summarise(
        churn = mean(.pred_yes, na.rm = TRUE),
        count = n(),
        .groups = "drop"
      ) %>%
      mutate(
        pct = count / sum(count),
        label_text = paste0(scales::percent(pct, accuracy = 1)),
        fill_color = case_when(
          antal_møder == "0 møder" ~ "#1b5a59",
          antal_møder == "1 møde" ~ "#4d867a",
          antal_møder == "1-5 møder" ~ "#7fdd9e",
          antal_møder == "Flere end 5 møder" ~ "#b3c9c1",
          TRUE ~ "gray"
        )
      )

    ggplot(meet_summary, aes(x = reorder(antal_møder, -pct), y = pct, fill = antal_møder)) +
      geom_col(width = 0.6, show.legend = FALSE) +
      geom_text(aes(label = label_text), hjust = -0.05, size = 6, color = "#1b5a59") +
      scale_fill_manual(values = meet_summary$fill_color) +
      coord_flip() +
      theme_minimal() +
      labs(
        title = "Fordeling af medlemmer efter mødeantal",
        x = NULL,
        y = "Andel"
      ) +
      theme(
      plot.title = element_text(
      hjust = 0,                  
      face = "bold",
      color = "#1b5a59",
      size = 20,
      margin = margin(b = 10)
    ),
    plot.title.position = "plot",
    plot.margin = margin(10, 10, 10, 10),
    axis.text.y = element_text(size = 14),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank()
    ) +
    ylim(0, max(meet_summary$pct) * 1.1)
  })

  # Opret distribution af mødelængde
  output$moedeFordeling <- renderPlot({
  members_summary <- members %>%
    mutate(mødelængde_tid = factor(
      mødelængde_tid,
      levels = c(
        "Ingen mødelængde",
        "Under 60 min",
        "75 min",
        "90 min",
        "Over 90 min"
      )
    )) %>%
    group_by(mødelængde_tid) %>%
    summarise(antal_virksomheder = n_distinct(`CVR-nr.`), .groups = "drop")

  fill_colors <- c("#1b5a59", "#4d867a", "#7fdd9e", "#b3c9c1", "#a3c9d1")

  ggplot(members_summary, aes(x = mødelængde_tid, y = antal_virksomheder)) +
    geom_bar(
      stat = "identity",
      fill = fill_colors[1:nrow(members_summary)],
      color = "white",
      alpha = 0.8,
      width = 0.7
    ) +
    labs(
      title = "Fordeling af medlemmer pr. mødelængde",
      y = "Antal virksomheder"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
      axis.text.y = element_text(size = 14),    
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 14, face = "bold"),
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = "#1b5a59"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "#f8f8f8", color = "white"),
      legend.position = "none"
    )
})

# Opret tabel med mødelængde og churn-risiko
  output$mødelængdeTabel <- render_gt({
    tibble(
      Risiko = c("🔴", "🟡", "🟡", "🟢", "🟢"),
      Mødelængde = c("Ingen møder", "Under 60 min", "90 min", "75 min", "Over 90 min"),
      `Churn_risiko` = c("49%", "27%", "20%", "14%", "13%")  # Changed hyphen to underscore
    ) %>%
      gt() %>%
      tab_header(
        title = md("**Churn-risiko baseret på mødelængde**")
      ) %>%
      cols_label(
        Risiko = "",
        Mødelængde = "Mødelængde",
        Churn_risiko = "Churn-risiko"  # Changed to match column name
      ) %>%
      tab_style(
        style = cell_text(color = "#1b5a59", weight = "bold"),
        locations = cells_body()
      ) %>%
      tab_style(
      style = cell_text(
      size = 20,
      weight = "bold",
      color = "#1b5a59",
      align = "center"
     ),
      locations = cells_title(groups = "title")
      ) %>%
      tab_options(
        table.background.color = "#ffffff",
        table.border.top.color = "transparent",
        table.font.size = 16,
        heading.title.font.size = 20,
        heading.title.font.weight = "bold",
        heading.padding = px(0),  # Added padding below title
        column_labels.font.weight = "bold",
        column_labels.font.size = 16,
        column_labels.background.color = "#ffffff",
        data_row.padding = px(4)
      )
  })

  output$avgRisk <- renderText({
    sprintf("%.1f%%", mean(probs$.pred_yes) * 100)
  })
}

# --- Run app with proper cleanup --- 
shinyApp(ui = ui, server = server)