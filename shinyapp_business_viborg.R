# Business Viborg Churn Dashboard - Rhino Design
# Load required packages
library(shiny)
library(tidyverse)
library(tidymodels)
library(vip)
library(DT)
library(forcats)
library(scales)
library(glmnet)
library(shinydashboard)

# --- GLOBAL SETUP -------------------------------------------------------------
setwd("C:/Users/andre/OneDrive - EaDania/eksamen_sem2/2_semesters_eksamensopgave2025_kode")
source("LogRegTilApp3.R")  # definerer master_final, train_data, test_data, log_final_model, log_pred

# Data preparation
nf <- master_final
members <- nf %>% filter(churnet == "no")
probs <- suppressWarnings(predict(log_final_model, new_data = members, type = "prob"))
pred_probs <- bind_cols(
  members %>% select(CVR = `CVR-nr.`, starts_with("Branche_"), Mindre_Virksomhed:Større_Virksomhed),
  probs
)

# Pre-compute metrics for "Ledelse"
branche_counts <- members %>% 
  pivot_longer(starts_with("Branche_"), names_to = "Branchetype", values_to = "has_branch") %>% 
  filter(has_branch == 1) %>% count(Branchetype) %>% 
  mutate(Branchetype = str_remove(Branchetype, "^Branche_"))
str_counts <- members %>% 
  pivot_longer(c(Mindre_Virksomhed, Mellem_Str._Virksomhed, Større_Virksomhed),
               names_to = "Organisationsstørrelse", values_to = "has_str") %>% 
  filter(has_str == 1) %>% count(Organisationsstørrelse)
branche_risk <- pred_probs %>% 
  pivot_longer(starts_with("Branche_"), names_to = "Branchetype", values_to = "has_branch") %>% 
  filter(has_branch == 1) %>% 
  group_by(Branchetype) %>% summarise(avg_risk = mean(.pred_yes), .groups = "drop") %>% 
  mutate(Branchetype = str_remove(Branchetype, "^Branche_"))

# --- UI ----------------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* RHINO DESIGN WITH BUSINESS VIBORG COLORS */
      
      /* Business Viborg color palette */
      :root {
        --bv-dark-green: #083832;
        --bv-light-green: #7AB547;
        --bv-bg-light: #f8f9fa;
        --bv-text-dark: #333333;
        --bv-text-light: #ffffff;
      }
      
      /* Modern container styling */
      .container-fluid {
        padding: 0;
        margin: 0;
        width: 100%;
      }
      
      /* Modern header styling */
      .header {
        background-color: var(--bv-dark-green);
        padding: 8px 25px;
        display: flex;
        align-items: center;
        justify-content: flex-start;
        width: 100%;
        box-shadow: 0 3px 10px rgba(8, 56, 50, 0.15);
        border-bottom: 2px solid var(--bv-light-green);
        position: relative;
        z-index: 1000;
        min-height: 60px;
        backdrop-filter: blur(5px);
      }
      
      /* Logo styling */
      .header img {
        height: 35px;
        margin-right: 15px;
        transition: all 0.4s ease;
        filter: brightness(1.05);
      }
      
      .header img:hover {
        transform: scale(1.08);
        filter: brightness(1.2);
      }
      
      /* Title styling */
      .header h2 {
        color: var(--bv-light-green);
        margin: 0;
        font-family: 'Segoe UI', -apple-system, BlinkMacSystemFont, sans-serif;
        font-weight: 300;
        font-size: 22px;
        letter-spacing: 1.2px;
        text-transform: uppercase;
        transition: all 0.3s ease;
        position: relative;
        padding-left: 10px;
      }
      
      .header h2:before {
        content: '';
        position: absolute;
        left: 0;
        top: 50%;
        transform: translateY(-50%);
        width: 3px;
        height: 70%;
        background-color: var(--bv-light-green);
        border-radius: 2px;
      }
      
      /* Content wrapper */
      .content-wrapper {
        padding: 25px 35px;
        background-color: var(--bv-bg-light);
        margin-top: 1px;
      }
      
      /* Add subtle animation to the header on page load */
      @keyframes headerFadeIn {
        from {
          opacity: 0;
          transform: translateY(-10px);
        }
        to {
          opacity: 1;
          transform: translateY(0);
        }
      }
      
      .header {
        animation: headerFadeIn 0.5s ease-out forwards;
      }
      
      /* Rhino-specific styles */
      .nav-tabs {
        border-bottom: 2px solid var(--bv-light-green);
      }
      
      .nav-tabs > li > a {
        color: var(--bv-dark-green);
        border: none;
        border-bottom: 3px solid transparent;
        margin-right: 2px;
        transition: all 0.3s ease;
        border-radius: 0;
        padding: 12px 20px;
      }
      
      .nav-tabs > li.active > a, 
      .nav-tabs > li.active > a:focus, 
      .nav-tabs > li.active > a:hover {
        border: none;
        border-bottom: 3px solid var(--bv-light-green);
        color: var(--bv-dark-green);
        font-weight: 500;
        background-color: rgba(122, 181, 71, 0.1);
      }
      
      .nav-tabs > li > a:hover {
        background-color: rgba(122, 181, 71, 0.05);
        border-color: transparent;
      }
      
      /* Button styling */
      .btn-primary {
        background-color: var(--bv-light-green);
        border-color: var(--bv-light-green);
        color: white;
        transition: all 0.3s ease;
      }
      
      .btn-primary:hover, .btn-primary:focus {
        background-color: var(--bv-dark-green);
        border-color: var(--bv-dark-green);
        color: white;
      }
      
      /* Sidebar styling */
      .well {
        background-color: white;
        border: none;
        border-radius: 8px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.05);
        padding: 15px 20px;
      }
      
      /* Tables styling */
      .dataTable {
        border-collapse: collapse;
        box-shadow: 0 2px 10px rgba(0,0,0,0.05);
        border-radius: 8px;
        overflow: hidden;
      }
      
      .dataTable thead th {
        background-color: var(--bv-dark-green) !important;
        color: white !important;
        border-bottom: none !important;
        font-weight: 400;
        padding: 12px 15px !important;
      }
      
      .dataTable tbody td {
        padding: 10px 15px !important;
        border-top: 1px solid #f1f1f1 !important;
      }
      
      .dataTable tbody tr:hover {
        background-color: rgba(122, 181, 71, 0.05) !important;
      }
      
      /* Radio buttons styling */
      .radio label {
        padding: 10px 15px;
        background-color: white;
        border-radius: 6px;
        border: 1px solid #e9ecef;
        margin-right: 10px;
        transition: all 0.3s ease;
        display: inline-block;
        margin-bottom: 8px;
      }
      
      .radio input[type='radio']:checked + label {
        background-color: var(--bv-light-green);
        color: white;
        border-color: var(--bv-light-green);
      }
      
      .radio input[type='radio'] {
        position: absolute;
        opacity: 0;
      }
      
      /* Improve text elements */
      h4 {
        color: var(--bv-dark-green);
        font-weight: 500;
        margin-bottom: 20px;
        padding-bottom: 8px;
        border-bottom: 1px solid #e9ecef;
      }
      
      /* Pagination styling */
      .pagination > .active > a, 
      .pagination > .active > a:focus, 
      .pagination > .active > a:hover {
        background-color: var(--bv-light-green);
        border-color: var(--bv-light-green);
      }
      
      .pagination > li > a {
        color: var(--bv-dark-green);
      }
      
      /* Search box styling */
      .dataTables_filter input {
        border: 1px solid #e9ecef;
        border-radius: 4px;
        padding: 6px 12px;
        box-shadow: none;
        transition: border-color 0.3s;
      }
      
      .dataTables_filter input:focus {
        border-color: var(--bv-light-green);
        outline: none;
      }
      
      /* Plot styling */
      .shiny-plot-output {
        border-radius: 8px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.05);
        background-color: white;
        padding: 15px;
      }
      
      /* Card styling for KPIs */
      .kpi-card {
        background-color: white;
        border-radius: 8px;
        padding: 20px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.05);
        transition: all 0.3s ease;
        height: 100%;
      }
      
      .kpi-card:hover {
        transform: translateY(-3px);
        box-shadow: 0 4px 15px rgba(0,0,0,0.1);
      }
      
      .kpi-card h4 {
        color: var(--bv-dark-green);
        font-size: 16px;
        font-weight: 500;
        margin-bottom: 10px;
        border-bottom: none;
        padding-bottom: 0;
      }
      
      .kpi-card .kpi-value {
        color: var(--bv-light-green);
        font-size: 28px;
        font-weight: 600;
      }
    "))
  ),
  
  # Header med logo og titel
  div(class = "header",
      tags$img(src = "www/business-viborg-logo.png"),
      h2("Churn Dashboard")
  ),
  
  # Wrap the rest of the UI in a div with padding
  div(class = "content-wrapper",
      sidebarLayout(
        sidebarPanel(
          div(
            style = "text-align: center; margin-bottom: 20px;",
            h4("Vælg rolle", style = "color: #083832; text-align: center; border-bottom: none; padding-bottom: 0;"),
            div(
              style = "display: flex; justify-content: center;",
              radioButtons("role", "", choices = c("Medarbejder", "Ledelse"), inline = TRUE)
            )
          )
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Churn-risiko",
                     conditionalPanel(
                       condition = "input.role == 'Medarbejder'",
                       div(class = "kpi-card",
                           h4("Medlemmer med højrisiko for churn"),
                           DTOutput("topRiskDT"),
                           h5("Klik på en række for detaljer:", style = "margin-top: 20px;"),
                           DTOutput("memberDetails")
                       )
                     ),
                     conditionalPanel(
                       condition = "input.role == 'Ledelse'",
                       fluidRow(
                         column(6, 
                                div(class = "kpi-card",
                                    h4("Antal medlemmer"),
                                    div(class = "kpi-value", textOutput("totalMembers"))
                                )
                         ),
                         column(6, 
                                div(class = "kpi-card",
                                    h4("Gns. churn-risiko"),
                                    div(class = "kpi-value", textOutput("avgRisk"))
                                )
                         )
                       )
                     )
            ),
            tabPanel("Events",
                     div(class = "kpi-card",
                         h4("Event Deltagelse"),
                         DTOutput("eventsTable")
                     )
            ),
            tabPanel("Møder",
                     div(class = "kpi-card",
                         h4("Møde-data her")
                     )
            ),
            tabPanel("Datavisualisering",
                     div(class = "kpi-card",
                         h4("Top 10 faktorer churn vægtes efter"),
                         plotOutput("vipPlotVisualisering")
                     )
            )
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
      dplyr::arrange(desc(.pred_yes)) %>%
      dplyr::select(CVR, starts_with("Branche_"), Mindre_Virksomhed:Større_Virksomhed, risk = .pred_yes) %>%
      tidyr::pivot_longer(starts_with("Branche_"), names_to = "Branchetype", values_to = "has_branch") %>%
      dplyr::filter(has_branch == 1) %>%
      tidyr::pivot_longer(c(Mindre_Virksomhed, Mellem_Str._Virksomhed, Større_Virksomhed), 
                          names_to = "Organisationsstørrelse", values_to = "has_str") %>%
      dplyr::filter(has_str == 1) %>%
      dplyr::mutate(
        Branchetype = stringr::str_remove(Branchetype, "^Branche_"),
        `Risiko (%)` = paste0(round(risk * 100), "%"),
        Organisationsstørrelse = case_when(
          Organisationsstørrelse == "Mindre_Virksomhed" ~ "1-49",
          Organisationsstørrelse == "Mellem_Str._Virksomhed" ~ "50-250",
          Organisationsstørrelse == "Større_Virksomhed" ~ "250+",
          TRUE ~ Organisationsstørrelse
        )
      ) %>%
      dplyr::select(CVR, Branchetype, Organisationsstørrelse, `Risiko (%)`)
  })
  
  output$topRiskDT <- DT::renderDataTable({
    DT::datatable(
      top_dt(),
      selection = 'single',
      options = list(
        pageLength = 10,
        lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'Alle')),
        language = list(
          lengthMenu = 'Vis _MENU_ medlemmer',
          search = "Søg:",
          info = "Viser _START_ til _END_ af _TOTAL_ medlemmer",
          infoEmpty = "Viser 0 til 0 af 0 medlemmer",
          zeroRecords = "Ingen medlemmer fundet",
          paginate = list(
            first = "Første",
            last = "Sidste",
            `next` = "Næste",
            previous = "Forrige"
          )
        ),
        searching = TRUE,
        ordering = TRUE,
        dom = 'Blfrtip',
        buttons = c('copy', 'csv', 'excel'),
        processing = TRUE,
        serverSide = FALSE
      ),
      class = 'cell-border stripe hover'
    )
  })
  
  output$memberDetails <- DT::renderDataTable({
    sel <- input$topRiskDT_rows_selected
    if (length(sel) == 0) return(NULL)
    DT::datatable(
      top_dt()[sel, , drop = FALSE],
      options = list(dom = 't', paging = FALSE)
    )
  })
  
  # Flyttet VIP plottet til "Datavisualisering" fanen
  output$vipPlotVisualisering <- renderPlot({
    glm_fit <- extract_fit_parsnip(log_final_model)$fit
    vip(glm_fit, num_features = 10, geom = "col") + 
      ggtitle("Top 10 faktorer churn vægtes efter") + 
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#083832", size = 16, face = "bold"),
        axis.title = element_text(color = "#083832"),
        panel.grid.major = element_line(color = "#f1f1f1"),
        panel.grid.minor = element_line(color = "#f8f8f8"),
        plot.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent")
      ) +
      scale_fill_gradient(low = "#7AB547", high = "#083832")
  })
  
  # Ledelse: KPI'er
  output$totalMembers <- renderText({ 
    format(nrow(members), big.mark = ".", decimal.mark = ",") 
  })
  
  output$avgRisk <- renderText({ 
    sprintf("%.1f%%", mean(probs$.pred_yes) * 100) 
  })
  
  # Events table
  output$eventsTable <- DT::renderDataTable({
    members %>%
      select(CVR = `CVR-nr.`,
             `Ingen Events` = Ingen_Events,
             `1-2 Events` = `1-2_Events`,
             `3-5 Events` = `3-5_Events`,
             `Over 5 Events` = Over_5_Events) %>%
      DT::datatable(
        options = list(
          pageLength = 10,
          lengthMenu = c(5, 10, 15, 20),
          language = list(
            lengthMenu = "Vis _MENU_ indtastninger",
            paginate = list(
              first = "Første",
              last = "Sidste",
              previous = "Forrige",
              `next` = "Næste"
            ),
            info = "Viser _START_ til _END_ af _TOTAL_ indtastninger",
            infoEmpty = "Viser 0 til 0 af 0 indtastninger",
            search = "Søg:",
            zeroRecords = "Ingen matchende resultater fundet"
          ),
          dom = 'lfrtp'
        ),
        class = 'cell-border stripe hover',
        rownames = FALSE
      ) %>%
      formatStyle(
        columns = c("Ingen Events", "1-2 Events", "3-5 Events", "Over 5 Events"),
        backgroundColor = styleEqual(c(0, 1), c('white', '#e8f5e9')),
        fontWeight = styleEqual(1, 'bold')
      )
  })
}

# Run the app
shinyApp(ui, server)