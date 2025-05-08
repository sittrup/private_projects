# Ensure correct working directory
setwd("C:/Users/patri/OneDrive/Desktop/2. Semester Porjekt/Medlemsdata-20250403/NewApp")

# Better file path handling with more robust error messages
model_file <- "LogRegTilApp3.R"
events_file <- "events.rds"

# Check if required files exist and provide clear messages
if (!file.exists(model_file)) {
    stop(paste0(
        "Required model file '", model_file, "' not found in ", getwd(),
        ". Please ensure this file exists before running the app."
    ))
}

# Create www directory if it doesn't exist
dir.create("www", showWarnings = FALSE)

# Check and handle logo file
logo_path <- "www/business-viborg-logo.png"
if (!file.exists(logo_path)) {
    message("Logo not found, downloading a placeholder...")
    download.file("https://i.ibb.co/S0S1R8Q/business-viborg-logo.png", logo_path, mode = "wb")
}

# Load libraries
library(shiny)
library(bslib)
library(tidyverse)
library(tidymodels)
library(vip)
library(DT)
library(forcats)
library(scales)
library(lubridate)
library(gt)
library(shadowtext)
library(ggforce) # For geom_arc_bar

required_packages <- c(
    "shiny", "tidyverse", "tidymodels", "DT", "vip",
    "forcats", "scales", "glmnet", "shinydashboard",
    "gridExtra", "grid", "shadowtext", "markdown",
    "lubridate", "ggplot2", "dplyr"
)

# Install missing packages
for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE)) {
        install.packages(pkg)
        library(pkg, character.only = TRUE)
    }
}

# Data prep - place in try-catch to provide better error messages
tryCatch(
    {
        if (!file.exists("LogRegTilApp3.R")) {
            stop("LogRegTilApp3.R not found – set your working directory correctly.")
        }
        source("LogRegTilApp3.R") # defines `master_final` & `log_final_model`

        # only active members
        nf <- master_final
        members <- nf %>% filter(churnet == "no")

        # predict churn-risk
        probs <- predict(log_final_model, new_data = members, type = "prob")
        pred_probs <- bind_cols(
            members %>%
                select(
                    CVR = `CVR-nr.`,
                    starts_with("Branche_"),
                    Mindre_Virksomhed, Mellem_Str._Virksomhed, Større_Virksomhed
                ),
            probs
        )

        # optional events data
        if (file.exists("events.rds")) {
            events_data <- readRDS("events.rds")
            cvr_event_deltagere <- unique(events_data$`CVR-nr.`)
            nf <- nf %>%
                mutate(
                    event_deltagelse = if_else(
                        `CVR-nr.` %in% cvr_event_deltagere, "Deltaget", "Ikke deltager"
                    )
                )
        } else {
            cvr_event_deltagere <- character()
        }

        # churn vs event summary
        event_status_df <- nf %>%
            mutate(deltager = if_else(
                `CVR-nr.` %in% cvr_event_deltagere, "Deltager", "Ikke deltager"
            )) %>%
            count(churnet, deltager) %>%
            group_by(deltager) %>%
            mutate(andel = n / sum(n)) %>%
            ungroup()

        # branches without events
        g_all <- master_final %>%
            pivot_longer(starts_with("Branche_"), names_to = "branche", values_to = "has_branch") %>%
            filter(has_branch == 1) %>%
            distinct(branche) %>%
            mutate(branche = str_remove(branche, "^Branche_"))
        g_with <- master_final %>%
            filter(`CVR-nr.` %in% cvr_event_deltagere) %>%
            pivot_longer(starts_with("Branche_"), names_to = "branche", values_to = "has_branch") %>%
            filter(has_branch == 1) %>%
            distinct(branche) %>%
            mutate(branche = str_remove(branche, "^Branche_"))
        brancher_uden_deltagelse <- g_all %>%
            filter(!(branche %in% g_with$branche))

        # meeting data for members
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

        # Process data for growth visualization before UI definition
        # member growth prep
        data_growth <- master_final %>%
            pivot_longer(starts_with("Branche_"), names_to = "Branche", values_to = "has_branch") %>%
            filter(has_branch == 1) %>%
            mutate(
                Branche = str_remove(Branche, "^Branche_"),
                reg_month = floor_date(as_date(MedlemsRegistreringsDato), "month"),
                SizeCategory = case_when(
                    Mindre_Virksomhed == 1 ~ "Small",
                    Mellem_Str._Virksomhed == 1 ~ "Medium",
                    Større_Virksomhed == 1 ~ "Large",
                    TRUE ~ NA_character_
                ) %>% factor(levels = c("Small", "Medium", "Large"))
            )

        new_df <- data_growth %>%
            group_by(reg_month, Branche, SizeCategory) %>%
            summarise(new = n(), .groups = "drop")

        months <- seq.Date(
            from = min(data_growth$reg_month, na.rm = TRUE),
            to   = floor_date(Sys.Date(), "month"),
            by   = "month"
        )

        segments <- expand_grid(
            reg_month    = months,
            Branche      = unique(data_growth$Branche),
            SizeCategory = levels(data_growth$SizeCategory)
        )

        growth_df <- segments %>%
            left_join(new_df, by = c("reg_month", "Branche", "SizeCategory")) %>%
            replace_na(list(new = 0)) %>%
            rowwise() %>%
            mutate(
                active = sum(
                    data_growth$reg_month <= reg_month &
                        data_growth$churnet == "no" &
                        data_growth$Branche == Branche &
                        data_growth$SizeCategory == SizeCategory
                ),
                renew = active - new
            ) %>%
            ungroup() %>%
            pivot_longer(c(new, renew), names_to = "type", values_to = "count")
    },
    error = function(e) {
        # Create empty placeholder data if loading fails
        message("Error in data preparation: ", e$message)
        growth_df <<- data.frame(
            reg_month = Sys.Date(),
            Branche = "Placeholder",
            SizeCategory = factor("Small", levels = c("Small", "Medium", "Large")),
            type = "new",
            count = 0
        )
    }
)

# UI - Fix CSS with complete color codes
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
      }

      /* Logo styling */
      .header img {
        height: 35px;
        margin-right: 15px;
      }

      /* Title styling */
      .header h2 {
        color: var(--bv-light-green);
        margin: 0;
        font-family: 'Segoe UI', sans-serif;
        font-weight: 300;
        font-size: 22px;
        letter-spacing: 1.2px;
      }

      /* Content wrapper */
      .content-wrapper {
        max-width: 1400px;
        margin: 0 auto;
        padding: 20px;
        background-color: var(--bv-bg-light);
      }

      /* Better integrated role buttons */
      .role-buttons {
        display: flex;
        gap: 8px;
        margin-top: 10px;
      }

      .role-button {
        background-color: #f8f9fa;
        color: #083832;
        border: 1px solid #083832;
        padding: 8px 16px;
        border-radius: 4px;
        cursor: pointer;
        transition: all 0.2s;
        font-weight: 500;
      }

      .role-button.active {
        background-color: #083832;
        color: #fff;
      }

      .role-button:hover:not(.active) {
        background-color: #e9ecef;
      }

      /* KPI cards styling */
      .kpi-card {
        background-color: #ffffff;
        border-radius: 8px;
        padding: 20px;
        box-shadow: 0 2px 10px rgba(8, 56, 50, 0.1);
        margin-bottom: 20px;
        transition: all 0.3s;
      }

      .kpi-card h4 {
        color: #083832;
        margin-top: 0;
        font-size: 18px;
        font-weight: 500;
        margin-bottom: 15px;
      }

      .kpi-value {
        color: #7AB547;
        font-size: 32px;
        font-weight: bold;
        text-align: center;
      }

      /* KPI boxes for meeting tab */
      .kpi-box {
        background-color: #ffffff;
        border-radius: 8px;
        padding: 15px;
        box-shadow: 0 2px 8px rgba(8, 56, 50, 0.1);
        margin-bottom: 20px;
        text-align: center;
      }

      .kpi-title {
        color: #083832;
        font-size: 16px;
        margin-bottom: 10px;
      }
    ")),
        # Add any other CSS from message (19) you want to include
    ),
    div(
        class = "header",
        tags$img(src = "www/business-viborg-logo.png", height = "35px"),
        h2("CHURN DASHBOARD", style = "color: #7AB547; margin: 0; font-weight: 300; font-size: 22px; letter-spacing: 1.2px;")
    ),

    # Role switcher
    div(
        class = "role-switcher",
        radioButtons("role", "", choices = c("Medarbejder", "Ledelse"), inline = TRUE),
        # Add the JavaScript to style the buttons
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
    ),

    # Main content area - replace with your dynamic tabs
    div(
        class = "content-wrapper",
        uiOutput("dynamicTabs")
    )
)

# ─── SERVER ───────────────────────────────────────────────────────────
server <- function(input, output, session) {
    # --- Medarbejder: top-N high-risk table & VIP plot
    top_dt <- reactive({
        pred_probs %>%
            arrange(desc(.pred_yes)) %>%
            slice_head(n = input$topN) %>%
            pivot_longer(starts_with("Branche_"), names_to = "branche", values_to = "has_branch") %>%
            filter(has_branch == 1) %>%
            pivot_longer(c(Mindre_Virksomhed, Mellem_Str._Virksomhed, Større_Virksomhed),
                names_to = "size", values_to = "has_size"
            ) %>%
            filter(has_size == 1) %>%
            mutate(
                branche    = str_remove(branche, "^Branche_"),
                `Risk (%)` = paste0(round(.pred_yes * 100, 1), "%")
            ) %>%
            select(CVR, branche, size, `Risk (%)`)
    })
    output$topRiskDT <- renderDT(top_dt(),
        selection = "single",
        options = list(pageLength = input$topN)
    )
    output$memberDetails <- renderDT({
        sel <- input$topRiskDT_rows_selected
        if (length(sel)) top_dt()[sel, ]
    })
    output$vipPlotMedarbejder <- renderPlot({
        vip(extract_fit_parsnip(log_final_model)$fit, 20, geom = "col")
    })

    # --- Ledelse: churn-risk KPIs & heatmap
    output$totalMembers <- renderText(nrow(members))
    output$avgRisk <- renderText(sprintf("%.1f%%", mean(probs$.pred_yes) * 100))
    output$heatmap <- renderPlot({
        tryCatch(
            {
                df <- pred_probs %>%
                    pivot_longer(starts_with("Branche_"), names_to = "Branche", values_to = "has_branch") %>%
                    filter(has_branch == 1) %>%
                    pivot_longer(c(Mindre_Virksomhed, Mellem_Str._Virksomhed, Større_Virksomhed),
                        names_to = "SizeCategory", values_to = "has_size"
                    ) %>%
                    filter(has_size == 1) %>%
                    mutate(Branche = str_remove(Branche, "^Branche_")) %>%
                    group_by(Branche, SizeCategory) %>%
                    summarise(
                        n_members = n(),
                        churn_prob_sum = sum(.pred_yes),
                        .groups = "drop"
                    ) %>%
                    mutate(
                        total_members = sum(n_members),
                        pct_members   = 100 * n_members / total_members,
                        pct_churn     = 100 * churn_prob_sum / sum(churn_prob_sum),
                        risk_ratio    = pct_churn / pct_members
                    )
                ggplot(df, aes(SizeCategory, Branche, fill = risk_ratio)) +
                    geom_tile() +
                    scale_fill_viridis_c() +
                    labs(fill = "Risk ratio") +
                    bv_theme()
            },
            error = function(e) {
                # Return a simple error plot
                plot(x = 0:1, y = 0:1, type = "n", axes = FALSE, xlab = "", ylab = "")
                text(0.5, 0.5, paste("Error:", e$message), col = "red")
            }
        )
    })

    # --- Events tab
    output$brancherUdenEvent <- renderDT(brancher_uden_deltagelse)
    output$churnEventPlot <- renderPlot({
        # Prepare data
        churn_event_data <- nf %>%
            mutate(event_category = case_when(
                Ingen_Events_Deltagelse == 1 ~ "Ingen",
                Højst_To_Events_Deltagelse == 1 ~ "Højst 2",
                Højst_Fem_Events_Deltagelse == 1 ~ "Højst 5",
                Flere_End_Fem_Events_Deltagelse == 1 ~ "Højst 5+",
                TRUE ~ "Ukendt"
            )) %>%
            count(event_category, churnet) %>%
            mutate(churnet = ifelse(churnet == "yes", "Churnet", "Ikke churnet"))

        ggplot(churn_event_data, aes(x = event_category, y = n, fill = churnet)) +
            geom_bar(stat = "identity", position = "dodge", width = 0.7) +
            geom_text(aes(label = n),
                position = position_dodge(width = 0.7),
                vjust = -0.5,
                color = "white", size = 4
            ) +
            labs(
                title = "Event-deltagelse og churn",
                x = "Event-deltagelse (antal events)",
                y = "Antal virksomheder",
                fill = "Status"
            ) +
            scale_fill_manual(values = c("Churnet" = "#cbd726", "Ikke churnet" = "#1b5a59")) +
            bv_theme() +
            theme(
                legend.position = "top",
                panel.grid.major.y = element_line(color = "#405f63", linewidth = 0.2),
                panel.grid.major.x = element_blank()
            )
    })
    output$brancheEventCountPlot <- renderPlot({
        # Data preparation
        branche_event_data <- nf %>%
            filter(churnet == "no") %>%
            mutate(event_deltagelse = ifelse(`CVR-nr.` %in% cvr_event_deltagere,
                "Deltaget",
                "Ikke deltager"
            )) %>%
            mutate(churn_status = ifelse(event_deltagelse == "Deltaget",
                "Deltaget og ikke churnet",
                "Ikke deltager og ikke churnet"
            )) %>%
            pivot_longer(starts_with("Branche_"), names_to = "branche", values_to = "has_branch") %>%
            filter(has_branch == 1) %>%
            count(branche, churn_status) %>%
            group_by(branche) %>%
            mutate(
                total = sum(n),
                pct = n / total * 100,
                branche = str_remove(branche, "^Branche_"),
                label = paste0(round(pct), "%")
            ) %>%
            ungroup()

        ggplot(branche_event_data, aes(x = fct_reorder(branche, total), y = n, fill = churn_status)) +
            geom_bar(stat = "identity", position = "stack") +
            geom_text(aes(label = label),
                position = position_stack(vjust = 0.5),
                color = "black", fontface = "bold", size = 3.5
            ) +
            coord_flip() +
            labs(
                title = "Procent af virksomheder med churn-risiko per branche",
                subtitle = "Event-deltagelse og Churn",
                x = "Branche",
                y = "Antal virksomheder"
            ) +
            scale_fill_manual(values = c(
                "Deltaget og ikke churnet" = "#405f63",
                "Ikke deltager og ikke churnet" = "#cbd726"
            )) +
            bv_theme() +
            theme(
                legend.position = "top",
                panel.grid.major.y = element_blank()
            )
    })
    output$eventParticipationSimple <- renderPlot({
        branch_participation <- nf %>%
            filter(churnet == "no") %>%
            pivot_longer(starts_with("Branche_"), names_to = "branche", values_to = "has_branch") %>%
            filter(has_branch == 1) %>%
            group_by(branche = str_remove(branche, "^Branche_")) %>%
            summarise(count = n(), .groups = "drop")

        ggplot(branch_participation, aes(x = fct_reorder(branche, count), y = count)) +
            geom_bar(stat = "identity", fill = "#cbd726") +
            geom_text(aes(label = count), hjust = -0.3, color = "white") +
            coord_flip() +
            labs(
                title = "Event-deltagelse per Branche",
                x = "Branche",
                y = "Antal virksomheder"
            ) +
            bv_theme()
    })

    # --- Møder tab
    output$avgMeetings <- renderText({
        x <- mean(
            members$Et_Møde * 1 +
                members$Højst_Fem_Møder * 3 +
                members$Flere_End_Fem_Møder * 6,
            na.rm = TRUE
        )
        paste0(round(x, 1), " møder")
    })
    output$noMeetingsPct <- renderText(paste0(round(mean(members$Ingen_Møder) * 100), "%"))
    output$antalUdenMoeder <- renderText(sum(members$Ingen_Møder))
    output$moedeVsChurnBar <- renderPlot({
        df3 <- pred_probs %>%
            left_join(members %>% select(CVR = `CVR-nr.`, antal_møder), by = "CVR") %>%
            filter(!is.na(antal_møder)) %>%
            count(antal_møder)
        ggplot(df3, aes(antal_møder, n, fill = antal_møder)) +
            geom_col() +
            bv_theme() +
            labs(x = "Mødetype", y = "Antal")
    })
    output$moedeVsChurnDonut <- renderPlot({
        # Create donut chart data
        donut_data <- members %>%
            mutate(antal_møder = factor(antal_møder,
                levels = c("0 møder", "1 møde", "1-5 møder", "Flere end 5 møder")
            )) %>%
            group_by(antal_møder) %>%
            summarise(churn_risk = mean(predict(log_final_model, newdata = cur_data(), type = "prob")$.pred_yes)) %>%
            mutate(
                risk_pct = round(churn_risk * 100),
                ymax = cumsum(1),
                ymin = lag(ymax, default = 0),
                label_position = (ymin + ymax) / 2
            )

        # Create the donut chart
        p <- ggplot(donut_data, aes(ymax = ymax, ymin = ymin, fill = antal_møder)) +
            geom_rect(aes(xmax = 4, xmin = 3)) +
            coord_polar(theta = "y") +
            xlim(c(0, 4)) +
            theme_void() +
            scale_fill_manual(values = c(
                "0 møder" = "#1b5a59",
                "1 møde" = "#7fdd9e",
                "1-5 møder" = "#cbd726",
                "Flere end 5 møder" = "#405f63"
            )) +
            theme(
                legend.position = "none",
                plot.background = element_rect(fill = "#1b5a59", color = NA),
                plot.title = element_text(color = "#7fdd9e", face = "bold", hjust = 0.5, size = 16)
            ) +
            labs(title = "Churn-risiko fordelt på antal møder")

        # Create legend explanation
        legend_data <- data.frame(
            y = seq(0.8, 0.2, length.out = 4),
            text = c(
                "49% churn-risiko 0 møder",
                "24% churn-risiko 1 møde",
                "14% churn-risiko 1-5 møder",
                "9% churn-risiko >5 møder"
            ),
            color = c("#1b5a59", "#7fdd9e", "#cbd726", "#405f63")
        )

        legend <- ggplot(legend_data, aes(x = 1, y = y, color = color)) +
            geom_point(size = 5) +
            geom_text(aes(x = 1.2, label = text), hjust = 0, color = "white", size = 5) +
            scale_color_identity() +
            theme_void() +
            theme(plot.background = element_rect(fill = "#1b5a59"))

        # Combine the plots
        gridExtra::grid.arrange(p, legend, ncol = 2, widths = c(0.6, 0.4))
    })
    output$churnVsMeetingCount <- renderPlot({
        df5 <- pred_probs %>%
            left_join(members %>% select(CVR = `CVR-nr.`, antal_møder), by = "CVR") %>%
            filter(!is.na(antal_møder)) %>%
            summarise(churn = mean(.pred_yes), .by = antal_møder)
        ggplot(df5, aes(antal_møder, churn)) +
            geom_col() +
            scale_y_continuous(labels = percent_format()) +
            bv_theme()
    })
    output$moedeFordeling <- renderPlot({
        meeting_data <- members %>%
            mutate(antal_møder = factor(antal_møder, levels = c("0 møder", "1 møde", "1-5 møder", "Flere end 5 møder"))) %>%
            count(antal_møder) %>%
            mutate(pct = n / sum(n) * 100)

        ggplot(meeting_data, aes(x = antal_møder, y = pct, fill = antal_møder)) +
            geom_bar(stat = "identity", width = 0.7) +
            geom_text(aes(label = paste0(round(pct), "%")),
                position = position_stack(vjust = 0.5),
                color = "white", fontface = "bold", size = 5
            ) +
            labs(
                title = "Fordeling af medlemmer efter mødeantal",
                x = "",
                y = "Procent"
            ) +
            scale_fill_manual(values = c(
                "0 møder" = "#1b5a59",
                "1 møde" = "#7fdd9e",
                "1-5 møder" = "#cbd726",
                "Flere end 5 møder" = "#1b5a59"
            )) +
            bv_theme() +
            theme(
                legend.position = "none"
            )
    })

    # Add GT table showing churn risk by meeting length
    output$mødelængdeTabel <- render_gt({
        tibble(
            Risiko = c("🔴", "🟡", "🟡", "🟢", "🟢", "🟢"),
            Mødelængde = c("Ingen mødelængde", "Under 60 min", "90 min", "60 min", "75 min", "Over 90 min"),
            Churn_risiko = c("49%", "27%", "20%", "16%", "14%", "13%")
        ) %>%
            gt() %>%
            tab_header(
                title = md("**Churn-risiko baseret på mødelængde**")
            ) %>%
            cols_label(
                Risiko = "",
                Mødelængde = "Mødelængde",
                Churn_risiko = "Churn-risiko"
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
                heading.padding = px(0),
                column_labels.font.weight = "bold",
                column_labels.font.size = 16,
                column_labels.background.color = "#ffffff",
                data_row.padding = px(4)
            )
    })

    # --- Member Growth tab
    output$growthPlot <- renderPlot({
        df6 <- growth_df %>% filter(type == "new")
        p <- ggplot(
            df6 %>% filter(
                if (input$growthGroup == "Branche") {
                    Branche %in% input$selectedBranches
                } else {
                    SizeCategory %in% input$selectedBranches
                }
            ),
            aes(
                x = reg_month, y = count,
                group = if (input$growthGroup == "Branche") Branche else SizeCategory,
                color = if (input$growthGroup == "Branche") Branche else SizeCategory
            )
        ) +
            geom_line() +
            {
                if (input$smoothLines) geom_smooth(se = FALSE)
            } +
            bv_theme()
        if (input$sameYAxis) {
            p <- p + facet_wrap(vars(if (input$growthGroup == "Branche") Branche else SizeCategory),
                scales = "fixed"
            )
        } else {
            p <- p + facet_wrap(vars(if (input$growthGroup == "Branche") Branche else SizeCategory),
                scales = "free_y"
            )
        }
        print(p)
    })
    output$totalGrowthPlot <- renderPlot({
        df7 <- growth_df %>%
            filter(type == "new") %>%
            group_by(reg_month) %>%
            summarise(total = sum(count))
        ggplot(df7, aes(reg_month, total)) +
            geom_line() +
            bv_theme()
    })

    # Add to your server function
    output$forklaring <- renderUI({
        div(
            style = "background-color: #1b5a59; padding: 15px; border-radius: 10px; color: white;",
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

    output$dynamicTabs <- renderUI({
        if (input$role == "Medarbejder") {
            # Only show Churn-risiko tab for Medarbejder
            tabsetPanel(
                tabPanel(
                    "Churn-risiko",
                    div(
                        class = "kpi-card",
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
                tabPanel(
                    "Churn-risiko",
                    fluidRow(
                        column(
                            6,
                            div(
                                class = "kpi-card",
                                h4("Antal medlemmer"),
                                div(class = "kpi-value", textOutput("totalMembers"))
                            )
                        ),
                        column(
                            6,
                            div(
                                class = "kpi-card",
                                h4("Gns. churn-risiko"),
                                div(class = "kpi-value", textOutput("avgRisk"))
                            )
                        )
                    ),
                    div(
                        class = "kpi-card", style = "margin-top: 20px;",
                        h4("Relative Churn-risiko per Branche & Størrelse"),
                        plotOutput("heatmap")
                    )
                ),

                # Add your other tabs here similar to the message (19) structure
                tabPanel(
                    "Events",
                    uiOutput("forklaring"),
                    fluidRow(
                        column(6, plotOutput("churnEventPlot", height = "350px")),
                        column(6, plotOutput("brancheEventCountPlot", height = "350px"))
                    ),
                    fluidRow(
                        column(12, h4("Brancher uden event-deltagelse:", style = "margin-top: 20px; color: #083832;")),
                        column(12, DTOutput("brancherUdenEvent"))
                    )
                ),
                tabPanel(
                    "Møder",
                    fluidRow(
                        column(
                            4,
                            div(
                                class = "kpi-box",
                                h4("Gennemsnitligt antal møder", class = "kpi-title"),
                                div(class = "kpi-value", textOutput("avgMeetings"))
                            )
                        ),
                        column(
                            4,
                            div(
                                class = "kpi-box",
                                h4("Medlemmer uden møder", class = "kpi-title"),
                                div(class = "kpi-value", textOutput("antalUdenMoeder"))
                            )
                        ),
                        column(
                            4,
                            div(
                                class = "kpi-box",
                                h4("Procent uden møder", class = "kpi-title"),
                                div(class = "kpi-value", textOutput("noMeetingsPct"))
                            )
                        )
                    ),
                    fluidRow(
                        column(6, plotOutput("moedeVsChurnDonut", height = "400px")),
                        column(6, plotOutput("moedeFordeling", height = "400px"))
                    ),
                    fluidRow(
                        column(12, gt_output("mødelængdeTabel"))
                    )
                ),
                tabPanel(
                    "Medlemsvækst",
                    fluidRow(
                        column(
                            3,
                            selectInput("growthGroup", "Gruppér efter:",
                                choices = c("Branche", "Virksomhedsstørrelse")
                            ),
                            conditionalPanel(
                                "input.growthGroup == 'Branche'",
                                checkboxGroupInput("selectedBranches", "Vælg brancher:",
                                    choices = unique(growth_df$Branche)[1:min(length(unique(growth_df$Branche)), 10)],
                                    selected = unique(growth_df$Branche)[1:min(length(unique(growth_df$Branche)), 3)]
                                )
                            ),
                            conditionalPanel(
                                "input.growthGroup == 'Virksomhedsstørrelse'",
                                checkboxGroupInput("selectedSizes", "Vælg størrelser:",
                                    choices = c("Small", "Medium", "Large"),
                                    selected = c("Small", "Medium", "Large")
                                )
                            ),
                            checkboxInput("smoothLines", "Vis trend", value = TRUE),
                            checkboxInput("sameYAxis", "Samme y-akse skala", value = FALSE)
                        ),
                        column(
                            9,
                            plotOutput("growthPlot", height = "400px"),
                            plotOutput("totalGrowthPlot", height = "200px")
                        )
                    )
                )
            )
        }
    })
}

# Replace your ichurn_theme() function with this:

bv_theme <- function() {
    theme_minimal() +
        theme(
            plot.title = element_text(color = "#083832", face = "bold", size = 16),
            legend.title = element_text(color = "#083832", face = "bold"),
            legend.text = element_text(color = "#333333"),
            axis.text = element_text(color = "#333333"),
            axis.title = element_text(color = "#083832", face = "bold"),
            panel.grid.major = element_line(color = "#f1f1f1", linewidth = 0.2),
            panel.grid.minor = element_blank()
        )
}

# ─── Launch ───────────────────────────────────────────────────────────
shinyApp(ui, server)
