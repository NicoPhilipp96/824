library(shiny)
library(flexdashboard)
library(ncaahoopR)
library(DT)
library(tidyverse)

# Get play-by-play data for University of Kansas basketball team for the 2021/22 season
kansas_pbp <- get_pbp("Kansas", "2021-22")
view(kansas_pbp)

kansas_roster <- get_roster("Kansas", "2021-22")
view(kansas_roster)

kansas_boxscores <- season_boxscore("Kansas", "2021-22", aggregate = "average")
kansas_boxscores[, sapply(kansas_boxscores, is.numeric)] <- round(kansas_boxscores[, sapply(kansas_boxscores, is.numeric)], 2)

# UI
ui <- fluidPage(
  titlePanel("University of Kansas Men's Basketball 2021/22 Season"),
  sidebarLayout(
    sidebarPanel(
      selectInput("game_id", "Select Game ID", choices = unique(kansas_pbp$game_id)),
      selectInput("player", "Select Player", choices = unique(kansas_roster$name))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Play By Play Charts",
                 plotOutput("game_flow_chart"),
                 plotOutput("wp_chart"),
                 plotOutput("shot_chart"),
        ),
        tabPanel("Player Assist Nets",
                 plotOutput("team_circle_assist_net"),
                 plotOutput("player_circle_assist_net"),
                 plotOutput("game_assist_net")
      ),
      tabPanel("Box Scores",
               DTOutput("box_score_table")
    )
  )
  )
)
)

# Server
server <- function(input, output) {
  
  output$game_flow_chart <- renderPlot({
    game_flow(input$game_id, home_col = "red", away_col = "blue")
  })
  
  output$wp_chart <- renderPlot({
    wp_chart(input$game_id, home_col = "red", away_col = "blue")
  })
  
  output$shot_chart <- renderPlot({
    game_shot_chart(game_id = input$game_id, heatmap = TRUE)
  })

  output$team_circle_assist_net <- renderPlot({
    circle_assist_net("Kansas", "2021-22", three_weights = TRUE)
  })
  
  output$player_circle_assist_net <- renderPlot({
    circle_assist_net("Kansas", "2021-22", highlight_player = input$player, 
                      highlight_color = "red", three_weights = TRUE)
  })
  
  output$game_assist_net <- renderPlot({
    assist_net("Kansas", season = input$game_id, 
                      node_col = "blue")
  })
  
  output$box_score_table <- renderDT({
    selected_player_stats <- subset(kansas_boxscores)
    datatable(selected_player_stats)
  })
}



# Run the application
shinyApp(ui = ui, server = server)
