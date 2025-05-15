# Load Required Libraries
library(shiny)
library(tidyverse)
library(ggplot2)
library(png)
library(grid)
library(ggimage)
library(plotly)
library(DT)
library(fmsb)
library(rlang)
library(tidyr)

# Load Datasets
regular_season_data <- read.csv("Data/2023-2024_NBA_Player_Stats_Regular.csv")
playoffs_data <- read.csv("Data/2023-2024_NBA_Player_Stats_Playoffs.csv")
nba_data <- read_csv("Data/NBA_2025_Shots.csv")  # Replace with your file path if local

# Stat Selector Mapping
stat_choices <- c(
  "PTS (Points per Game)" = "PTS",
  "AST (Assists per Game)" = "AST",
  "TRB (Total Rebounds per Game)" = "TRB",
  "STL (Steals per Game)" = "STL",
  "BLK (Blocks per Game)" = "BLK",
  "TOV (Turnovers per Game)" = "TOV",
  "FGP (Field Goal Percentage)" = "FGP",
  "FTP (Free Throw Percentage)" = "FTP"
)

# Plot Court
circle_points <- function(center = c(0, 0), radius = 1, npoints = 360) {
  angles <- seq(0, 2 * pi, length.out = npoints)
  tibble(x = center[1] + radius * cos(angles),
         y = center[2] + radius * sin(angles))
}

plot_court <- function(court_theme) {
  width <- 50
  height <- 94 / 2
  key_height <- 19
  outer_key_width <- 16
  backboard_width <- 6
  backboard_offset <- 4
  neck_length <- 0.5
  hoop_radius <- 0.75
  hoop_center_y <- backboard_offset + neck_length + hoop_radius
  three_point_radius <- 23.75
  three_point_side_radius <- 22
  three_point_side_height <- 14
  
  court_points <- tibble(
    x = c(width/2, width/2, -width/2, -width/2, width/2),
    y = c(height, 0, 0, height, height),
    desc = "perimeter"
  )
  
  court_points <- bind_rows(court_points, tibble(
    x = c(outer_key_width/2, outer_key_width/2, -outer_key_width/2, -outer_key_width/2),
    y = c(0, key_height, key_height, 0),
    desc = "outer_key"
  ))
  
  court_points <- bind_rows(court_points, tibble(
    x = c(-backboard_width/2, backboard_width/2),
    y = c(backboard_offset, backboard_offset),
    desc = "backboard"
  ))
  
  court_points <- bind_rows(court_points, tibble(
    x = c(0, 0), y = c(backboard_offset, backboard_offset + neck_length), desc = "neck"
  ))
  
  hoop <- circle_points(center = c(0, hoop_center_y), radius = 0.75) %>%
    mutate(desc = "hoop")
  
  restricted <- circle_points(center = c(0, hoop_center_y), radius = 4) %>%
    filter(y >= hoop_center_y) %>%
    mutate(desc = "restricted")
  
  three_point_circle <- circle_points(center = c(0, hoop_center_y), radius = 23.75) %>%
    filter(y >= three_point_side_height, y >= hoop_center_y)
  
  three_point_line <- tibble(
    x = c(22, 22, three_point_circle$x,
          -22, -22),
    y = c(0, 14, three_point_circle$y,
          14, 0),
    desc = "three_point_line"
  )
  
  court_points <- bind_rows(court_points, hoop, restricted, three_point_line)
  
  ggplot() +
    geom_path(data = court_points, aes(x = x, y = y, group = desc), color = court_theme$lines) +
    coord_fixed(ylim = c(0, 45), xlim = c(-25, 25)) +
    theme_void() +
    theme(
      panel.background = element_rect(fill = court_theme$court),
      plot.background = element_rect(fill = court_theme$court)
    )
}

court_theme_white <- list(court = 'white', lines = 'black', text = 'black', made = '#00bfc4', missed = '#f8766d')

# User Interface
ui <- fluidPage(
  titlePanel("NBA Player Explorer: Shot Charts & Analytics (2024-25)"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(id = "tab_selector",
                  tabPanel("Shot Chart",
                           selectInput("player", "Choose Player:", choices = unique(nba_data$PLAYER_NAME)),
                           uiOutput("game_date_ui"),
                           selectInput("shot_type", "Filter by Shot Type:", choices = c("All", unique(nba_data$SHOT_TYPE))),
                           selectInput("zone_range", "Filter by Zone Range:", choices = c("All", unique(nba_data$ZONE_RANGE)))
                  ),
                  tabPanel("Stats",
                           selectInput("stats_tab", "Select Visualization Type:", 
                                       choices = c("Stat Comparison", "Detailed Player Stats", "Top 10 Players", "Radar Graph", "Playoff Prediction", "PCA Player Map")),
                           conditionalPanel("input.stats_tab == 'Stat Comparison'",
                                            selectInput("players", "Select Players:", choices = unique(c(regular_season_data$Player, playoffs_data$Player)), multiple = TRUE),
                                            selectInput("stat", "Select Stat:", choices = stat_choices)
                           ),
                           conditionalPanel("input.stats_tab == 'Detailed Player Stats'",
                                            selectInput("players", "Select Players:", choices = unique(c(regular_season_data$Player, playoffs_data$Player)), multiple = TRUE),
                                            selectInput("stat", "Select Stat:", choices = stat_choices),
                                            checkboxGroupInput("additional_stats", "Select Additional Stats:", 
                                                               choices = c("MP (Minutes Played)" = "MP", "FGA (Field Goal Attempts)" = "FGA", 
                                                                           "FTA (Free Throw Attempts)" = "FTA", "PF (Personal Fouls)" = "PF"))
                           ),
                           conditionalPanel("input.stats_tab == 'Top 10 Players'",
                                            selectInput("stat", "Select Stat:", choices = stat_choices)
                           ),
                           conditionalPanel("input.stats_tab == 'Radar Graph'",
                                            selectInput("players", "Select Players:", choices = unique(c(regular_season_data$Player, playoffs_data$Player)), multiple = TRUE)
                           ),
                           conditionalPanel("input.stats_tab == 'Playoff Prediction'",
                                            selectInput("predict_stat", "Select Stat to Predict:", choices = stat_choices),
                                            selectInput("prediction_players", "Select Players to Show:", choices = unique(intersect(regular_season_data$Player, playoffs_data$Player)), multiple = TRUE)
                           ),
                           radioButtons("season", "Select Season:", choices = c("Regular Season", "Playoffs"))
                  )
      )
    ),
    mainPanel(
      conditionalPanel("input.tab_selector == 'Shot Chart'", plotOutput("shotChart", height = "600px")),
      conditionalPanel("input.tab_selector == 'Stats' && input.stats_tab == 'Stat Comparison'", plotlyOutput("statPlot", height = "500px")),
      conditionalPanel("input.tab_selector == 'Stats' && input.stats_tab == 'Detailed Player Stats'", DTOutput("playerTable")),
      conditionalPanel("input.tab_selector == 'Stats' && input.stats_tab == 'Top 10 Players'", plotlyOutput("topPlayersPlot", height = "550px")),
      conditionalPanel("input.tab_selector == 'Stats' && input.stats_tab == 'Radar Graph'", plotOutput("radarPlot", height = "500px")),
      conditionalPanel("input.tab_selector == 'Stats' && input.stats_tab == 'Playoff Prediction'", DTOutput("predictionTable")),
      conditionalPanel("input.tab_selector == 'Stats' && input.stats_tab == 'PCA Player Map'", plotlyOutput("pcaPlot", height = "500px"))
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  output$game_date_ui <- renderUI({
    filtered_dates <- nba_data %>% filter(PLAYER_NAME == input$player) %>% pull(GAME_DATE) %>% unique()
    selectInput("game_date", "Select Game Date:", choices = filtered_dates)
  })
  
  output$shotChart <- renderPlot({
    req(input$player, input$game_date)
    shots <- nba_data %>% filter(PLAYER_NAME == input$player, GAME_DATE == input$game_date)
    if (input$shot_type != "All") shots <- shots %>% filter(SHOT_TYPE == input$shot_type)
    if (input$zone_range != "All") shots <- shots %>% filter(ZONE_RANGE == input$zone_range)
    player_team <- unique(shots$TEAM_NAME)
    fg_pct <- ifelse(nrow(shots) > 0, paste0("FG%: ", round(mean(shots$SHOT_MADE == "TRUE") * 100, 1), "%"), "FG%: N/A")
    plot_court(court_theme_white) +
      geom_point(data = shots, aes(x = LOC_X, y = LOC_Y, color = as.factor(SHOT_MADE), fill = as.factor(SHOT_MADE)),
                 size = 3, shape = 21, stroke = 0.5) +
      scale_color_manual(values = c("TRUE" = court_theme_white$made, "FALSE" = court_theme_white$missed),
                         labels = c("TRUE" = "Made", "FALSE" = "Missed")) +
      scale_fill_manual(values = c("TRUE" = court_theme_white$made, "FALSE" = court_theme_white$missed),
                        labels = c("TRUE" = "Made", "FALSE" = "Missed")) +
      labs(title = paste(input$player, "-", player_team), subtitle = paste("Game:", shots$HOME_TEAM, "vs.", shots$AWAY_TEAM, "|", "Game Date:", input$game_date, "|", fg_pct), color = "Shot Result",
           fill = "Shot Result")
  })
  
  filtered_data <- reactive({
    df <- if (input$season == "Regular Season") regular_season_data else playoffs_data
    df <- df %>% filter(Player %in% input$players)
    return(df)
  })
  
  output$statPlot <- renderPlotly({
    req(input$players, input$stat)
    data <- filtered_data()
    p <- ggplot(data, aes(y = reorder(Player, .data[[input$stat]]), x = .data[[input$stat]], fill = Player)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Comparison of", names(stat_choices)[stat_choices == input$stat]), x = input$stat, y = "Player") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$playerTable <- renderDT({
    cols <- c("Player", "Tm", "G", input$stat, input$additional_stats)
    datatable(filtered_data()[, cols, drop = FALSE])
  })
  
  output$topPlayersPlot <- renderPlotly({
    data <- if (input$season == "Regular Season") regular_season_data else playoffs_data
    top <- data %>% arrange(desc(.data[[input$stat]])) %>% head(10)
    p <- ggplot(top, aes(y = reorder(Player, .data[[input$stat]]), x = .data[[input$stat]], fill = Player)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Top 10 in", input$stat)) +
      theme_minimal()
    ggplotly(p)
  })
  
  output$radarPlot <- renderPlot({
    req(input$players)
    data <- filtered_data()
    stats <- c("PTS", "AST", "TRB", "BLK", "TOV")
    if (nrow(data) == 0) return(plot.new())
    percentiles <- data.frame(Player = data$Player)
    for (s in stats) percentiles[[s]] <- ecdf(regular_season_data[[s]])(data[[s]]) * 100
    chart_data <- rbind(rep(100, length(stats)), rep(0, length(stats)), percentiles[, -1])
    radarchart(chart_data, axistype = 1, pcol = rainbow(nrow(percentiles)), plwd = 2)
    legend("topright", legend = percentiles$Player, col = rainbow(nrow(percentiles)), lty = 1)
  })
  
  output$predictionTable <- renderDT({
    req(input$predict_stat, input$players)
    
    reg <- regular_season_data %>% select(Player, Regular = !!sym(input$predict_stat))
    play <- playoffs_data %>% select(Player, Playoff = !!sym(input$predict_stat))
    joined <- inner_join(reg, play, by = "Player")
    
    if (length(input$prediction_players) > 0) {
      joined <- joined %>% filter(Player %in% input$prediction_players)
    }
    
    if (nrow(joined) == 0) {
      return(datatable(data.frame(Message = "No matching players found.")))
    }
    
    joined$Predicted <- predict(lm(Playoff ~ Regular, data = joined))
    
    final <- joined %>% select(Player, Regular, Actual = Playoff, Predicted) %>%
      mutate(across(c(Regular, Actual, Predicted), round, 2))
    
    datatable(final, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$pcaPlot <- renderPlotly({
    stats <- regular_season_data %>% select(Player, Tm, PTS, AST, TRB, STL, BLK, TOV, FGP, FTP) %>%
      mutate(across(-c(Player, Tm), as.numeric)) %>% drop_na()
    pca <- prcomp(stats %>% select(-Player, -Tm), scale. = TRUE)
    pca_data <- as.data.frame(pca$x[, 1:2]) %>% mutate(Player = stats$Player, Team = stats$Tm)
    ggplotly(ggplot(pca_data, aes(x = PC1, y = PC2, color = Team, text = Player)) + geom_point() + theme_minimal())
  })
}

# Run the App
shinyApp(ui = ui, server = server)
