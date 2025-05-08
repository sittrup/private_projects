# Business Viborg Churn Dashboard - Comprehensive App

# --- REQUIRED PACKAGES ------------------------------------------------------------
required_packages <- c("shiny", "tidyverse", "tidymodels", "DT", "vip", 
                      "forcats", "scales", "glmnet", "shinydashboard", 
                      "gridExtra", "grid", "shadowtext", "markdown", 
                      "lubridate", "ggplot2", "dplyr")

# Load or install all packages
for(pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Stop any running Shiny apps
if (!is.null(shiny::getDefaultReactiveDomain())) {
  shiny::stopApp()
}

# --- GLOBAL SETUP -------------------------------------------------------------
# Source the model file - using your specific path
setwd("C:/Users/andre/OneDrive - EaDania/eksamen_sem2/2_semesters_eksamensopgave2025_kode")
source("LogRegTilApp3.R") # definerer master_final, train_data, test_data, log_final_model, log_pred

# --- DATA PREPARATION ---------------------------------------------------------
# Basic data prep
nf <- master_final
members <- nf %>% filter(churnet == "no")
probs <- suppressWarnings(predict(log_final_model, new_data = members, type = "prob"))
pred_probs <- bind_cols(
  members %>% select(CVR = `CVR-nr.`, starts_with("Branche_"), Mindre_Virksomhed:Større_Virksomhed),
  probs
)

# Calculate meeting count and duration
members <- members %>%
  mutate(
    antal_møder = case_when(
      Flere_End_Fem_Møder == 1 ~ "Flere end 5 møder",
      Højst_Fem_Møder == 1 ~ "1-5 møder",
      Et_Møde == 1 ~ "1 møde",
      Ingen_Møder == 1 ~ "0 møder",
      TRUE ~ "Ukendt"
    ),
    mødelængde_tid = case_when(
      Møde_Længde_Under60min == 1 ~ "Under 60 min",
      Møde_Længde_60min == 1 ~ "60 min",
      Møde_Længde_75min == 1 ~ "75 min",
      Møde_Længde_90min == 1 ~ "90 min",
      Møde_Længde_Over90min == 1 ~ "Over 90 min",
      Ingen_Mødelængde == 1 ~ "Ingen mødelængde",
      TRUE ~ "Ukendt"
    )
  )

# Calculate heatmap data for management
summary_df <- pred_probs %>%
  pivot_longer(starts_with("Branche_"), names_to = "Branche", values_to = "has_branch") %>%
  filter(has_branch == 1) %>%
  pivot_longer(c(Mindre_Virksomhed, Mellem_Str._Virksomhed, Større_Virksomhed),
               names_to = "SizeCategory", values_to = "has_size") %>%
  filter(has_size == 1) %>%
  mutate(
    Branche = str_remove(Branche, "^Branche_"),
    SizeCategory = case_when(
      SizeCategory == "Mindre_Virksomhed" ~ "Small",
      SizeCategory == "Mellem_Str._Virksomhed" ~ "Medium",
      SizeCategory == "Større_Virksomhed" ~ "Large",
      TRUE ~ SizeCategory
    ) %>% factor(levels = c("Small", "Medium", "Large"))
  ) %>%
  group_by(Branche, SizeCategory) %>%
  summarise(
    n_members = n(),
    churn_prob_sum = sum(.pred_yes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    total_members = sum(n_members),
    total_churn   = sum(churn_prob_sum),
    pct_members   = 100 * n_members / total_members,
    pct_churn     = 100 * churn_prob_sum / total_churn,
    risk_ratio    = pct_churn / pct_members
  )

# Order industries by overall risk_ratio
tot_m <- sum(summary_df$n_members)
tot_c <- sum(summary_df$churn_prob_sum)
branche_ord <- summary_df %>%
  group_by(Branche) %>%
  summarise(
    grp_m = sum(n_members),
    grp_c = sum(churn_prob_sum),
    .groups = "drop"
  ) %>%
  mutate(overall_ratio = (grp_c / tot_c) / (grp_m / tot_m)) %>%
  arrange(desc(overall_ratio)) %>%
  pull(Branche)

summary_df <- summary_df %>% mutate(Branche = factor(Branche, levels = branche_ord))

# Try to load events data if available
try({
  events_path <- file.path(dirname(getwd()), "events.rds")
  if(file.exists(events_path)) {
    events_data <- readRDS(events_path)
    event_cvr <- unique(events_data$CVR)
    nf <- nf %>%
      mutate(event_deltagelse = if_else(`CVR-nr.` %in% event_cvr, "Deltaget", "Ikke deltaget"))
    
    # Event-specific data preparations
    cvr_event_deltagere <- unique(events_data$`CVR-nr.`)
    
    all_branches <- master_final %>%
      select(starts_with("Branche_")) %>%
      pivot_longer(everything(), names_to = "branche", values_to = "has_branch") %>%
      filter(has_branch == 1) %>%
      distinct(branche) %>%
      mutate(branche = str_remove(branche, "^Branche_"))
    
    branche_event_deltagere <- master_final %>%
      filter(`CVR-nr.` %in% cvr_event_deltagere) %>%
      select(starts_with("Branche_")) %>%
      pivot_longer(everything(), names_to = "branche", values_to = "has_branch") %>%
      filter(has_branch == 1) %>%
      distinct(branche) %>%
      mutate(branche = str_remove(branche, "^Branche_"))
    
    brancher_uden_deltagelse <- all_branches %>%
      filter(!(branche %in% branche_event_deltagere$branche))
    
    branche_plot_data <- master_final %>%
      mutate(Deltager = ifelse(`CVR-nr.` %in% cvr_event_deltagere, "Ja", "Nej")) %>%
      pivot_longer(starts_with("Branche_"), names_to = "branche", values_to = "has_branch") %>%
      filter(has_branch == 1) %>%
      mutate(branche = str_remove(branche, "^Branche_")) %>%
      count(branche, Deltager)
    
    event_status_df <- master_final %>%
      mutate(deltager = ifelse(`CVR-nr.` %in% events_data$`CVR-nr.`, "Deltager", "Ikke deltager")) %>%
      select(`CVR-nr.`, churnet, deltager)
    
    churn_event_summary <- event_status_df %>%
      count(churnet, deltager) %>%
      group_by(deltager) %>%
      mutate(andel = n / sum(n))
  }
}, silent = TRUE)

# Add Member Growth data preparation
try({
  # First, create a dataframe with a single "Branche" column
  branche_data <- master_final %>%
    pivot_longer(
      cols = starts_with("Branche_"),
      names_to = "Branche",
      values_to = "has_branch"
    ) %>%
    filter(has_branch == 1) %>%
    mutate(
      Branche = str_remove(Branche, "^Branche_"),
      reg_month = floor_date(MedlemsRegistreringsDato, "month"),
      SizeCategory = case_when(
        Mindre_Virksomhed == 1 ~ "Small",
        Mellem_Str._Virksomhed == 1 ~ "Medium",
        Større_Virksomhed == 1 ~ "Large",
        TRUE ~ NA_character_
      ) %>% factor(levels = c("Small", "Medium", "Large"))
    )
  
  # Count new members per month×segment
  new_df <- branche_data %>%
    group_by(reg_month, Branche, SizeCategory) %>%
    summarise(new = n(), .groups = "drop")
  
  # Calculate active members and renewals
  months <- seq.Date(as.Date(min(branche_data$reg_month)),
                    as.Date(floor_date(Sys.Date(), "month")),
                    by = "month"
  )
  
  segments <- expand_grid(
    reg_month    = months,
    Branche      = unique(branche_data$Branche),
    SizeCategory = levels(branche_data$SizeCategory)
  )
  
  growth_df <- segments %>%
    left_join(new_df, by = c("reg_month", "Branche", "SizeCategory")) %>%
    replace_na(list(new = 0)) %>%
    rowwise() %>%
    mutate(
      active = sum(
        branche_data$reg_month <= reg_month &
          branche_data$churnet == "no" &
          branche_data$Branche == Branche &
          branche_data$SizeCategory == SizeCategory
      ),
      renew = active - new
    ) %>%
    ungroup() %>%
    pivot_longer(c(new, renew), names_to = "type", values_to = "count")
}, silent = TRUE)

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
      
      /* KPI container styling for meeting tab */
      .kpi-container {
        display: flex;
        justify-content: space-between;
        gap: 20px;
        margin-bottom: 20px;
        min-height: 120px;
      }
      
      .kpi-box {
        background-color: #ffffff;
        border-radius: 12px;
        padding: 25px;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
        text-align: center;
        flex: 1;
        overflow: hidden;
        display: flex;
        flex-direction: column;
        justify-content: center;
      }
      
      .kpi-title {
        font-size: 18px;
        color: #1b5a59;
        margin-bottom: 15px;
        font-weight: bold;
      }
      
      .kpi-value {
        font-size: 24px;
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
      
      /* Role switcher container */
      .role-switcher {
        display: flex;
        align-items: center;
        background-color: var(--bv-dark-green);
        padding: 0;
        border-radius: 0;
        margin: 0;
        height: 40px;
        border-bottom: 1px solid var(--bv-light-green);
      }
      
      .role-label {
        color: white;
        font-weight: 500;
        font-size: 14px;
        margin: 0 10px 0 15px;
        flex-shrink: 0;
      }
      
      /* Better integrated role buttons */
      .role-buttons {
        display: flex;
        height: 100%;
        flex-grow: 1;
      }
      
      .role-button {
        background-color: transparent;
        border: none;
        color: white;
        padding: 0 15px;
        font-size: 14px;
        cursor: pointer;
        height: 100%;
        display: flex;
        align-items: center;
        justify-content: center;
        transition: background-color 0.3s;
        margin: 0;
      }
      
      .role-button.active {
        background-color: var(--bv-light-green);
        font-weight: 500;
      }
      
      .role-button:hover:not(.active) {
        background-color: rgba(255, 255, 255, 0.1);
      }
      
      /* Hide radio buttons */
      input[name='role'] {
        position: absolute;
        opacity: 0;
      }
      
      .shiny-options-group {
        display: flex;
        width: 100%;
      }
      
      .shiny-options-group .radio {
        flex-grow: 1;
        margin: 0;
      }
      
      .shiny-options-group label {
        width: 100%;
        height: 100%;
        padding: 10px 0;
        display: flex;
        align-items: center;
        justify-content: center;
        cursor: pointer;
      }
      
      /* Hide control label for radio */
      #role-label {
        display: none;
      }
      
      /* Number input for Medarbejder */
      #topN-container {
        margin: 0;
        padding: 8px 15px;
        display: flex;
        align-items: center;
        background-color: #f5f5f5;
      }
      
      #topN-container .control-label {
        margin-right: 10px;
        font-size: 13px;
        font-weight: normal;
        flex-grow: 1;
        margin-bottom: 0;
      }
      
      #topN {
        width: 70px;
        padding: 4px;
        font-size: 13px;
      }
      
      /* Main area adjustments */
      #dynamicTabs {
        margin-top: 15px;
      }
    "))
  ),
  
  # Header with logo and title
  div(class = "header",
      tags$img(src = "www/business-viborg-logo.png"),
      h2("Churn Dashboard")
  ),
  
  # Role switcher (replacing the sidebar)
  div(class = "role-switcher",
      span(class = NULL),
      div(class = "role-buttons",
          div(id = "roleButtonGroup",
              radioButtons("role", "", choices = c("Medarbejder", "Ledelse"), inline = TRUE),
              tags$script("
                $(document).ready(function() {
                  // Add active class based on the selected value
                  $('input:radio[name=role]').change(function() {
                    $('.role-button').removeClass('active');
                    $('.role-button[data-value=\"' + $(this).val() + '\"]').addClass('active');
                  });
                  
                  // Initialize with the default selected value
                  $('.role-button[data-value=\"' + $('input:radio[name=role]:checked').val() + '\"]').addClass('active');
                });
              ")
          )
      )
  ),
  
  # Conditional control for Medarbejder
  conditionalPanel(
    condition = "false",
    div(id = "topN-container",
        numericInput("topN", "Antal højrisiko virksomheder:", value = 10, min = 1)
    )
  ),
  
  # Main content area
  div(class = "content-wrapper",
      uiOutput("dynamicTabs")
  ),
  
  # JavaScript to enhance the role buttons
  tags$script(HTML("
    $(document).ready(function() {
      // Create custom role buttons
      $('#roleButtonGroup .shiny-options-group').hide();
      
      var buttonGroup = $('<div class=\"role-buttons\"></div>');
      
      // Create Medarbejder button
      var medarbejderBtn = $('<button type=\"button\" class=\"role-button\" data-value=\"Medarbejder\">Medarbejder</button>');
      medarbejderBtn.click(function() {
        $('input:radio[name=role][value=Medarbejder]').prop('checked', true).trigger('change');
      });
      
      // Create Ledelse button
      var ledelseBtn = $('<button type=\"button\" class=\"role-button\" data-value=\"Ledelse\">Ledelse</button>');
      ledelseBtn.click(function() {
        $('input:radio[name=role][value=Ledelse]').prop('checked', true).trigger('change');
      });
      
      // Add buttons to the group
      buttonGroup.append(medarbejderBtn).append(ledelseBtn);
      
      // Initially set the active button
      if ($('input:radio[name=role][value=Medarbejder]').prop('checked')) {
        medarbejderBtn.addClass('active');
      } else {
        ledelseBtn.addClass('active');
      }
      
      // Insert the custom buttons
      $('.role-switcher').append(buttonGroup);
    });
  "))
)

# --- SERVER ------------------------------------------------------------------
server <- function(input, output, session) {
  # --- DYNAMIC TABSET BASED ON ROLE ---
  output$dynamicTabs <- renderUI({
    if(input$role == "Medarbejder") {
      # Only show Churn-risiko tab for Medarbejder
      tabsetPanel(
        # TAB 1: CHURN RISK for Medarbejder
        tabPanel("Churn-risiko",
            div(class = "kpi-card",
                h4("Medlemmer med højrisiko for churn"),
                DTOutput("topRiskDT"),
                h5("Klik på en række for detaljer:", style = "margin-top: 20px;"),
                DTOutput("memberDetails")
            )
        )
      )
    } else {
      # Show all tabs for Ledelse
      tabsetPanel(
        # TAB 1: CHURN RISK for Ledelse
        tabPanel("Churn-risiko",
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
            ),
            div(class = "kpi-card", style = "margin-top: 20px;",
                h4("Relative Churn-risiko per Branche & Størrelse"),
                plotOutput("heatmap", height = "600px")
            )
        ),
        
        # TAB 2: EVENTS
        tabPanel("Events",
            uiOutput("forklaring"),
            div(class = "kpi-card",
                h4("Sammenhæng: Event-deltagelse og churn"),
                plotOutput("churnEventPlot")
            ),
            div(class = "kpi-card", style = "margin-top: 20px;",
                h4("Event-deltagelse per branche"),
                plotOutput("brancheEventCountPlot")
            )
        ),
        
        # TAB 3: MEETINGS
        tabPanel("Møder",
            tagList(
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
                      div(class = "kpi-box", style = "flex: 1;",
                          plotOutput("moedeVsChurnBar", height = "400px")
                      ),
                      div(class = "kpi-box", style = "flex: 1;",
                          plotOutput("moedeVsChurnDonut", height = "400px")
                      )
                  ),
                  div(class = "kpi-container",
                      div(class = "kpi-box", style = "flex: 1;",
                          plotOutput("churnVsMeetingCount", height = "400px")
                      )
                  )
              ),
              div(class = "chart-container",
                  div(class = "chart-title", "Mødelængde"),
                  div(class = "kpi-container",
                      div(class = "kpi-box", style = "flex: 1;",
                          plotOutput("moedeFordeling", height = "400px")
                      ),
                      div(class = "kpi-box", style = "flex: 1;",
                          tableOutput("mødelængdeTabel")
                      )
                  )
              )
            )
        ),
        
        # TAB 4: DATA VISUALIZATION
        tabPanel("Top 10",
            div(class = "kpi-card",
                h4("Top 10 faktorer churn vægtes efter"),
                plotOutput("vipPlotVisualisering")
            )
        ),
        
        # TAB 5: MEDLEMS
        tabPanel("Medlemsvækst ",
            fluidRow(
              column(
                3,
                div(class = "kpi-card",
                    radioButtons("growthGroup", "Group by:",
                                choices = c("Branche", "SizeCategory"),
                                inline = TRUE
                    ),
                    selectInput("timeRangePreset", "Time Range:",
                                choices = c("Last 12 months", "Last 3 years", "Last 5 years", "All time")
                    ),
                    checkboxInput("sameYAxis", "Use same scale for all charts", value = FALSE),
                    checkboxInput("smoothLines", "Show smoothed trends", value = TRUE),
                    selectInput("selectedBranches", "Select Industries:",
                                choices = if(exists("growth_df")) unique(growth_df$Branche) else "",
                                multiple = TRUE,
                                selected = if(exists("growth_df")) {
                                  branches <- unique(growth_df$Branche)
                                  branches[1:min(4, length(branches))]
                                } else ""
                    )
                )
              ),
              column(
                9,
                div(class = "kpi-card",
                    plotOutput("growthPlot", height = "600px")
                ),
                div(class = "kpi-card", style = "margin-top: 20px;",
                    plotOutput("totalGrowthPlot", height = "250px")
                )
              )
            )
        )
      )
    }
  })

  # --- CHURN RISK TAB - EMPLOYEE VIEW ---
  top_dt <- reactive({
    pred_probs %>%
      dplyr::arrange(desc(.pred_yes)) %>%
      slice_head(n = input$topN) %>%
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
        )
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
  
  # VIP plot for visualization tab (only used by Ledelse now)
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

  # --- CHURN RISK TAB - MANAGEMENT VIEW ---
  output$totalMembers <- renderText({
    format(nrow(members), big.mark = ".", decimal.mark = ",") 
  })

  output$avgRisk <- renderText({
    sprintf("%.1f%%", mean(probs$.pred_yes) * 100)
  })

  # Heat map
  output$heatmap <- renderPlot({
    ggplot(summary_df, aes(x = SizeCategory, y = Branche, fill = risk_ratio)) +
      geom_tile(color = "white", linewidth = 0.5) +
      geom_text(aes(label = sprintf("%.2f", risk_ratio)), size = 4) +
      scale_fill_gradient2(
        midpoint = 1, # center at "on-par"
        low      = "#7fdd9e", # light green (under-represented)
        mid      = "#b3c9c1", # light grey-green (neutral)
        high     = "#cbd726", # bright yellow (over-represented)
        limits   = c(0.5, 1.5),
        oob      = scales::squish,
        name     = "Risk Ratio\n(% churn ÷ % members)"
      ) +
      # Add this scale to change x-axis labels with employee counts
      scale_x_discrete(
        labels = c(
          "Small" = "Small\n(< 50 ansatte)",
          "Medium" = "Medium\n(< 250 ansatte)",
          "Large" = "Large\n(> 250 ansatte)"
        )
      ) +
      labs(x = "Virksomheds-størrelse", y = "Branche") +
      theme_minimal(base_size = 14) +
      theme(panel.grid = element_blank())
  })
  
  # Forklaring på plots
  output$forklaring <- renderUI({
    div(style = "background-color: #083832; padding: 15px; border-radius: 10px; color: white; margin-bottom: 20px;",
        HTML("
        <h4 style='color: #7AB547;'>📊 Hvordan aflæses graferne?</h4>
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
  
  # Event plots
  output$churnEventPlot <- renderPlot({
    req(exists("nf"))
    nf %>%
      pivot_longer(
        cols = c("Ingen_Events_Deltagelse", "Højst_To_Events_Deltagelse", "Højst_Fem_Events_Deltagelse", "Flere_End_Fem_Events_Deltagelse"),
        names_to = "event_kategori", 
        values_to = "har_deltaget"
      ) %>%
      filter(har_deltaget == 1) %>%
      count(event_kategori, churnet) %>%
      mutate(
        event_kategori = factor(event_kategori, 
                              levels = c("Ingen_Events_Deltagelse", "Højst_To_Events_Deltagelse", "Højst_Fem_Events_Deltagelse", "Flere_End_Fem_Events_Deltagelse"),
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
        values = c("Ikke churnet" = "#083832", "Churnet" = "#7AB547")
      ) +
      labs(
        title = "Churn i forhold til eventdeltagelse",
        x = "Event-deltagelse (antal events)",
        y = "Antal virksomheder",
        fill = "Status"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", color = "#083832"),
        legend.position = "top"
      )
  })
  
  output$brancheEventCountPlot <- renderPlot({
    req(exists("nf") && exists("cvr_event_deltagere"))
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
          "Deltaget.no" = "#083832",      # Mørkegrøn
          "Deltaget.yes" = "#1b5a59",     # Mørkere grå-grøn
          "Ikke deltager.no" = "#7AB547", # Lysegrøn
          "Ikke deltager.yes" = "#cbd726"  # Gul
        ),
        labels = c(
          "Deltaget og ikke churnet",
          "Deltaget og churnet",
          "Ikke deltager og ikke churnet",
          "Ikke deltager og churnet"
        )
      ) +  
      geom_text(aes(label = paste0(round(procent, 1), "%")), position = position_stack(vjust = 0.5), color = "white") + 
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", color = "#083832"),
        legend.position = "top"
      )
  })
  
  # --- MEETINGS TAB ---
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
            xmid = 3.5,
            label_inside = scales::percent(churn, accuracy = 1),
            fill_color = case_when(
                antal_møder == "0 møder" ~ "#083832",
                antal_møder == "1 møde" ~ "#4d867a",
                antal_møder == "1-5 møder" ~ "#7AB547",
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
      farve = c("#083832", "#4d867a", "#7AB547", "#b3c9c1"),
      prik = "●"
    )

    p3 <- ggplot(tekst_forklaring, aes(y = y)) +
      geom_text(aes(x = 1.3, label = prik, color = farve), size = 6) +
      geom_text(aes(x = 1.5, label = tekst), hjust = 0, size = 6, color = "#083832") +
      scale_color_identity() +
      xlim(1.3, 4) +
      ylim(0.1, 0.9) +
      theme_void()

    p1 <- ggplot(meet_summary) +
        ggtitle("Churn-risiko fordelt på antal møder") +
        geom_rect(aes(xmin = 2.8, xmax = 6.0, ymin = ymin, ymax = ymax, fill = fill_color), 
                  color = "white", size = 0.5) +      
        scale_fill_identity() +
        coord_polar(theta = "y") +
        xlim(-2.0, 6.5) +                                                   
        theme_void() +
        geom_text(aes(x = 4, y = ymid, label = label_inside),           
                  color = "white", fontface = "bold", size = 6) +          
        theme(
            plot.margin = margin(t = -50, r = 100, b = 20, l = 5),
            plot.title = element_text(
                hjust = 0.01,                                                 
                size = 20,
                face = "bold",
                color = "#083832",
                margin = margin(b = 50)
            )
        )

    gridExtra::grid.arrange(p1, p3, ncol = 2, widths = c(0.8, 1.2))
  }, height = 400, width = 900)


  # Plot with churn and meeting activity
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
        color = "#083832"
      ) +
      scale_y_continuous(
        labels = scales::percent_format(accuracy = 1),
        limits = c(0, 1.1),
        expand = expansion(mult = c(0, 0.05))
      ) +
      scale_fill_manual(values = c("Churn" = "#083832", "Aktive" = "#7AB547")) +
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
          color = "#083832",
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
          antal_møder == "0 møder" ~ "#083832",
          antal_møder == "1 møde" ~ "#4d867a",
          antal_møder == "1-5 møder" ~ "#7AB547",
          antal_møder == "Flere end 5 møder" ~ "#b3c9c1",
          TRUE ~ "gray"
        )
      )

    ggplot(meet_summary, aes(x = reorder(antal_møder, -pct), y = pct, fill = antal_møder)) +
      geom_col(width = 0.6, show.legend = FALSE) +
      geom_text(aes(label = label_text), hjust = -0.05, size = 6, color = "#083832") +
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
          color = "#083832",
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

  # Create distribution of meeting length
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

    fill_colors <- c("#083832", "#4d867a", "#7AB547", "#b3c9c1", "#a3c9d1")

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
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = "#083832"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#f8f8f8", color = "white"),
        legend.position = "none"
      )
  })

  # Create table with meeting length and churn risk
  output$mødelængdeTabel <- renderTable({
    tibble(
      Risiko = c("🔴", "🟡", "🟡", "🟢", "🟢"),
      Mødelængde = c("Ingen møder", "Under 60 min", "90 min", "75 min", "Over 90 min"),
      `Churn_risiko` = c("49%", "27%", "20%", "14%", "13%")
    )
  }, striped = TRUE, hover = TRUE, digits = 0)
  
  # --- MEMBER GROWTH TAB ---
  output$growthPlot <- renderPlot({
    req(exists("growth_df"))
    # Filter by selected time range
    end_date <- max(growth_df$reg_month)
    start_date <- case_when(
      input$timeRangePreset == "Last 12 months" ~ end_date - months(12),
      input$timeRangePreset == "Last 3 years" ~ end_date - years(3),
      input$timeRangePreset == "Last 5 years" ~ end_date - years(5),
      TRUE ~ min(growth_df$reg_month)
    )

    # Filter by selected branches/industries
    filtered_df <- growth_df %>%
      filter(reg_month >= start_date) %>%
      filter(if (input$growthGroup == "Branche") Branche %in% input$selectedBranches else TRUE)

    # Base plot
    p <- ggplot(filtered_df, aes(x = reg_month, y = count, color = type)) +
      scale_color_manual(
        values = c(new = "#083832", renew = "#7AB547"), 
        labels = c("New", "Renewing"),
        name = "Member Type"
      ) +
      labs(
        x = "Month",
        y = "Member Count",
        title = "New vs. Renewing Members Over Time"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "top",
        strip.text = element_text(size = 12, face = "bold"),
        strip.background = element_rect(fill = "#f0f0f0", color = NA)
      )

    # Add smoothed trend lines if requested
    if (input$smoothLines) {
      p <- p + geom_smooth(aes(group = type), method = "loess", se = FALSE, linewidth = 1.2) +
        geom_line(alpha = 0.3, linewidth = 0.6) # Original lines with lower alpha
    } else {
      p <- p + geom_line(linewidth = 1)
    }

    # Facet by the chosen grouping with appropriate scales
    if (input$growthGroup == "Branche") {
      p <- p + facet_wrap(~Branche, scales = if (input$sameYAxis) "fixed" else "free_y", ncol = 2)
    } else {
      p <- p + facet_wrap(~SizeCategory, scales = if (input$sameYAxis) "fixed" else "free_y")
    }

    p
  })

  # Add a summary plot showing total growth
  output$totalGrowthPlot <- renderPlot({
    req(exists("growth_df"))
    # Filter by selected time range
    end_date <- max(growth_df$reg_month)
    start_date <- case_when(
      input$timeRangePreset == "Last 12 months" ~ end_date - months(12),
      input$timeRangePreset == "Last 3 years" ~ end_date - years(3),
      input$timeRangePreset == "Last 5 years" ~ end_date - years(5),
      TRUE ~ min(growth_df$reg_month)
    )

    # Calculate net monthly growth across all segments
    summary_growth <- growth_df %>%
      filter(reg_month >= start_date) %>%
      group_by(reg_month, type) %>%
      summarize(total = sum(count), .groups = "drop")

    # Calculate cumulative members
    cumulative_members <- summary_growth %>%
      pivot_wider(names_from = type, values_from = total, values_fill = 0) %>%
      arrange(reg_month) %>%
      mutate(
        net_growth = new - lag(renew, default = 0),
        cumulative = cumsum(net_growth) + first(new + renew)
      )

    # Plot total membership trend
    ggplot(cumulative_members, aes(x = reg_month, y = cumulative)) +
      geom_area(fill = "#7AB547", alpha = 0.6) +
      geom_line(color = "#083832", linewidth = 1) +
      labs(
        title = "Total Membership Growth Over Time",
        x = "Month",
        y = "Total Members"
      ) +
      theme_minimal()
  })
}

# --- Run the app ---------------------------------------------------------------
shinyApp(ui, server)