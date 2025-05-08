# app.R --------------------------------------------------------------

# Packages
library(shiny)
library(tidyverse)
library(tidymodels)
library(vip)
library(DT)
library(forcats)
library(scales)
library(lubridate)

# Paths
app_dir <- "C:/Users/patri/OneDrive/Desktop/2. Semester Porjekt/Medlemsdata-20250403/NewApp"
model_file <- file.path(app_dir, "LogRegTilApp3.R") # your model script
data_file <- file.path(app_dir, "master_final_-PE+ML.csv") # your data

setwd(app_dir)
source(model_file) # this must define master_final & log_final_model

# Data prep (global)
members <- master_final %>% filter(churnet == "no")
probs <- predict(log_final_model, new_data = members, type = "prob")
pred_probs <- bind_cols(
  members %>% select(CVR = `CVR-nr.`, starts_with("Branche_"), Mindre_Virksomhed:Større_Virksomhed),
  probs
)

summary_df <- pred_probs %>%
  pivot_longer(starts_with("Branche_"), names_to = "Branche", values_to = "has_branch") %>%
  filter(has_branch == 1) %>%
  pivot_longer(c(Mindre_Virksomhed, Mellem_Str._Virksomhed, Større_Virksomhed),
    names_to = "SizeCategory", values_to = "has_size"
  ) %>%
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

# Add Member Growth data preparation
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

# ─── UI ────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(
    tags$style("
      /* Page background */
      body { background-color: #1b5a59; color: #ffffff; }

      /* Panels, inputs, sidebar */
      .panel, .well, .shiny-input-container {
        background-color: #405f63;
        color: #ffffff;
      }

      /* Highlighted text */
      .highlight { color: #cbd726; }

      /* Secondary backgrounds / shadows */
      .panel, .box { background-color: #b3c9c1; }

      /* Links */
      a { color: #ffffff; text-decoration: underline; }
    ")
  ),
  titlePanel("Business Viborg Churn Dashboard"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("role", "Vælg rolle:", c("Medarbejder", "Ledelse")),
      conditionalPanel(
        "input.role=='Medarbejder'",
        numericInput("topN", "Antal højrisiko virksomheder:", 10, 1)
      ),
      conditionalPanel(
        "input.role=='Ledelse'",
        helpText("Høj orange = over-repræsenteret i churn-risiko")
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Churn-risiko",
          conditionalPanel(
            "input.role=='Medarbejder'",
            h4("Højrisiko medlemmer"),
            DTOutput("topRiskDT"),
            h5("Klik for detaljer"),
            DTOutput("memberDetails"),
            h4("Top 20 faktorer"),
            plotOutput("vipPlotMedarbejder")
          ),
          conditionalPanel(
            "input.role=='Ledelse'",
            fluidRow(
              column(6, h4("Antal medlemmer"), textOutput("totalMembers")),
              column(6, h4("Gns. churn-risiko"), textOutput("avgRisk"))
            ),
            h4("Relative Churn-risiko per Branche & Størrelse"),
            plotOutput("heatmap", height = "600px")
          )
        ),
        tabPanel("Events", h4("Event-data her")),
        tabPanel("Møder", h4("Møde-data her")),
        tabPanel(
          "Member Growth",
          fluidRow(
            column(
              3,
              wellPanel(
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
                  choices = unique(growth_df$Branche),
                  multiple = TRUE,
                  selected = unique(growth_df$Branche)[1:4] # Default to first 4 industries
                )
              )
            ),
            column(
              9,
              plotOutput("growthPlot", height = "600px"),
              plotOutput("totalGrowthPlot", height = "250px")
            )
          )
        )
      )
    )
  )
)


# ─── Server ────────────────────────────────────────────────────────
server <- function(input, output, session) {
  # Medarbejder
  top_dt <- reactive({
    pred_probs %>%
      arrange(desc(.pred_yes)) %>%
      slice_head(n = input$topN) %>%
      select(CVR, starts_with("Branche_"), Mindre_Virksomhed:Større_Virksomhed, risk = .pred_yes) %>%
      pivot_longer(starts_with("Branche_"), names_to = "branche", values_to = "has_branch") %>%
      filter(has_branch == 1) %>%
      pivot_longer(c(Mindre_Virksomhed, Mellem_Str._Virksomhed, Større_Virksomhed),
        names_to = "size", values_to = "has_size"
      ) %>%
      filter(has_size == 1) %>%
      mutate(
        branche = str_remove(branche, "^Branche_"),
        `Risk (%)` = paste0(round(risk * 100), "%")
      ) %>%
      select(CVR, branche, size, `Risk (%)`)
  })
  output$topRiskDT <- renderDT(top_dt(), selection = "single", options = list(pageLength = input$topN))
  output$memberDetails <- renderDT(
    {
      sel <- input$topRiskDT_rows_selected
      if (length(sel) == 0) {
        return(NULL)
      }
      top_dt()[sel, , drop = FALSE]
    },
    options = list(dom = "t", paging = FALSE)
  )
  output$vipPlotMedarbejder <- renderPlot({
    glm_fit <- extract_fit_parsnip(log_final_model)$fit
    vip(glm_fit, num_features = 20, geom = "col") + theme_minimal() +
      ggtitle("Top 20 faktorer")
  })

  # Ledelse
  output$totalMembers <- renderText({
    format(nrow(members), big.mark = ".", decimal.mark = ",")
  })
  output$avgRisk <- renderText({
    sprintf("%.1f%%", mean(probs$.pred_yes) * 100)
  })
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

  output$growthPlot <- renderPlot({
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
        values = c(new = "#006d2c", renew = "#41b6c4"), # More distinct colors
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
      geom_area(fill = "#41b6c4", alpha = 0.6) +
      geom_line(color = "#006d2c", linewidth = 1) +
      labs(
        title = "Total Membership Growth Over Time",
        x = "Month",
        y = "Total Members"
      ) +
      theme_minimal()
  })
}

# ─── Launch ───────────────────────────────────────────────────────
shinyApp(ui, server)
