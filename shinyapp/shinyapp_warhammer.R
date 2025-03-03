install.packages("shiny, shinythemes, ggplot2, dplyr")

# Load libraries
library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)

#######################################################################
## (1) TITANICUS SIMULATION CODE
#######################################################################

roll_d6 <- function() sample(1:6, 1)
roll_d10 <- function() sample(1:10, 1)

create_weapon <- function(name, shots, base_to_hit=3,
                          blast=FALSE, shieldbane=FALSE, rapid=FALSE, inferno=FALSE,
                          strength=0, fusion=FALSE) {
  list(
    name        = name,
    shots       = shots,
    base_to_hit = base_to_hit,
    blast       = blast,
    shieldbane  = shieldbane,
    rapid       = rapid,
    inferno     = inferno,
    strength    = strength,
    fusion      = fusion
  )
}

all_weapons <- list(
  create_weapon("Vulcan Mega-Bolter", 6, 3, blast=FALSE, shieldbane=FALSE, rapid=TRUE, inferno=FALSE, strength=4),
  create_weapon("Plasma Blastgun", 2, 3, blast=TRUE, shieldbane=FALSE, strength=8),
  create_weapon("Plasma Blastgun (Max)", 2, 3, blast=TRUE, shieldbane=FALSE, strength=10),
  create_weapon("Turbo Laser Destructor", 2, 3, blast=FALSE, shieldbane=FALSE, strength=8),
  create_weapon("Turbo Laser Destructor (SB)", 2, 3, blast=FALSE, shieldbane=TRUE, strength=8),
  create_weapon("Inferno Gun", 0, 3, inferno=TRUE, strength=7)
)

calculate_hits <- function(weapon, distance) {
  if (weapon$inferno) {
    return(ifelse(distance <= 8, 3, 0))
  }
  
  total_hits <- 0
  for (i in seq_len(weapon$shots)) {
    d <- roll_d6()
    if (d == 1) next
    if (d == 6) {
      total_hits <- total_hits + 1
      if (weapon$rapid) {
        total_hits <- total_hits + 1
      }
    } else {
      needed <- max(2, weapon$base_to_hit)
      if (d >= needed) total_hits <- total_hits + 1
    }
  }
  total_hits
}

simulate_hit_distribution <- function(weapon, distance=10, n_runs=1000) {
  results <- numeric(n_runs)
  for (i in seq_len(n_runs)) {
    results[i] <- calculate_hits(weapon, distance)
  }
  results
}

#######################################################################
## (2) SHINY UI
#######################################################################

ui <- fluidPage(
  theme = shinytheme("cyborg"),  # Dark theme for Warhammer feel
  
  titlePanel(
    div(
      h2("Titanicus Probability Simulator", style="margin-bottom:10px; color: #FF4500;"),
      img(src = "war.png", height = "100px", alt="Logo here"),
      style = "text-align: center; margin-bottom: 20px;"
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("weaponSelect", "Choose Weapon:",
                  choices = sapply(all_weapons, function(w) w$name),
                  selected = "Vulcan Mega-Bolter"),
      sliderInput("distance", "Target Distance (inches):", min=1, max=32, value=10),
      sliderInput("nRuns", "# of Simulation Runs:", min=100, max=5000, value=1000, step=100),
      
      actionButton("runSim", "Run Simulation"),
      br(), br(),
      h4("Choose Plot Type:"),
      selectInput("plotType", 
                  "Plot Type:", 
                  choices = c("Histogram", "Boxplot", "Line Distribution"),
                  selected = "Histogram")
    ),
    
    mainPanel(
      plotOutput("distPlot"),
      br(),
      h4("Description:"),
      p("This app visualizes the distribution of hits for a chosen weapon at a given distance, over many random simulations."),
      p("Colors and themes are inspired by Warhammer universe aesthetics.")
    )
  )
)

#######################################################################
## (3) SHINY SERVER
#######################################################################

server <- function(input, output, session) {
  
  simData <- eventReactive(input$runSim, {
    wpn <- all_weapons[[which(sapply(all_weapons, function(w) w$name) == input$weaponSelect)]]
    simulate_hit_distribution(wpn, distance=input$distance, n_runs=input$nRuns)
  })
  
  output$distPlot <- renderPlot({
    data_vec <- simData()
    df <- data.frame(hits = data_vec)
    
    if (input$plotType == "Histogram") {
      ggplot(df, aes(x=hits)) +
        geom_histogram(binwidth=1, fill="#FF4500", color="black", alpha=0.7) +
        labs(
          title = paste("Distribution of Hits:", input$weaponSelect),
          subtitle= paste("Distance =", input$distance, "inches,", input$nRuns, "runs"),
          x="Number of Hits",
          y="Count"
        ) +
        theme_minimal()
      
    } else if (input$plotType == "Boxplot") {
      ggplot(df, aes(y=hits, x=1)) +
        geom_boxplot(fill = "#1E90FF", color="black", alpha=0.7) +
        labs(
          title = paste("Boxplot of Hits:", input$weaponSelect),
          x="",
          y="Number of Hits"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_blank())
      
    } else if (input$plotType == "Line Distribution") {
      dist_summary <- df %>%
        group_by(hits) %>%
        summarise(count = n()) %>%
        mutate(prob = count / input$nRuns)
      
      ggplot(dist_summary, aes(x=hits, y=prob)) +
        geom_line(color="#32CD32", size=1.2) +
        geom_point(color="#32CD32", size=3) +
        labs(
          title = paste("Line Distribution of Hits:", input$weaponSelect),
          x="Number of Hits",
          y="Probability"
        ) +
        theme_minimal()
    }
  })
}

#######################################################################
## (4) RUN THE APP
#######################################################################
shinyApp(ui=ui, server=server)
