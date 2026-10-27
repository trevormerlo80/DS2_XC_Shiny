#    https://shiny.posit.co/

library("ggplot2")
library(jsonlite)
library(tidyverse)
library(shiny)
library(bslib)


tfrrs_results <- fromJSON("http://127.0.0.1:8000/meets/28078?sport=xc")
base_url <- "http://localhost:8000"

teams <- c(
  "Shippensburg",
  "Messiah",
  "RIT"
)

get_athlete <- function(id) {
  fromJSON(
    paste0(
      "http://127.0.0.1:8000/athletes/",
      id
    )
  )
}

ship <- fromJSON("http://127.0.0.1:8000/teams/PA_college_m_Shippensburg?sport=xc")
ship_roster <- ship$roster

messiah <- fromJSON("http://127.0.0.1:8000/teams/PA_college_m_Messiah?sport=xc")
messiah_roster <- messiah$roster

rit <- fromJSON("http://127.0.0.1:8000/teams/NY_college_m_RIT?sport=xc")
rit_roster <- rit$roster

dickinson <- fromJSON("http://127.0.0.1:8000/teams/PA_college_m_Dickinson_College?sport=xc")
dickinson_roster <- dickinson$roster

etown <- fromJSON("http://127.0.0.1:8000/teams/PA_college_m_Elizabethtown?sport=xc")
etown_roster <- etown$roster


ship_runners <- map(ship_roster$athlete_id, get_athlete)
messsiah_runners <- map(messiah_roster$athlete_id, get_athlete)
rit_runners <- map(rit_roster$athlete_id, get_athlete)
dickinson_runners <- map(dickinson_roster$athlete_id, get_athlete)
etown_runners <- map(etown_roster$athlete_id, get_athlete)


rosters_list <- list("1" = ship_roster, "2" = messiah_roster, "3" = rit_roster, "4" = dickinson_roster, "5" = etown_roster)

all_rosters <- bind_rows(ship_roster, messiah_roster, rit_roster)
all_runners <- map(all_rosters$athlete_id, get_athlete)


results <- tibble(all_runners) |>
  unnest_wider(all_runners)

meet_results <- results |>
  unnest(results)


meet_pred <- function(meet_results) {
  runners <- meet_results |>
    arrange(athlete_name, event_name, mark) |> 
    filter(event_name == "8k") |> 
    distinct(athlete_name, .keep_all = T) |> 
    select(athlete_name, mark, current_team_name) |> 
    arrange(mark) |> 
    mutate(points = row_number()) |> 
    group_by(current_team_name) |> 
    slice_head(n = 5) |> 
    arrange(mark) |> 
    mutate(team_score = sum(points))
  
  return(runners)
}

team_scorer <- function(scored_meet) {
  team_score = scored_meet |> 
    group_by(team_score) |> 
    slice(1) |> 
    select(current_team_name, team_score) |> 
    arrange(team_score)
}



##################################################
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  theme = bs_theme(
    bg = "#0b3d91", fg = "white", primary = "#ab3d91", #"#FCC780",
    base_font = "Cinzel",
  ),
  #theme = bslib::bs_theme(bootswatch = "darkly"),
  #theme = bslib::bs_theme(
  #  bg = "#0b3d91", 
  #  fg = "white", 
  #  
  #  base_font = "Cinzel"
  #),
  
  # Application title
  titlePanel("-NCAA XC Meet Predictor-"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "teams_in_race",
        "Select Teams in the Meet",
        choices = list("Shippensburg" = 1, "Messiah" = 2, "RIT" = 3, "Dickinson" = 4., "Etown" = 5),
        selected = 1
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Individual Results", plotOutput("ind_plot")),
        tabPanel("Team Results", plotOutput("team_plot"), plotOutput("team_breakdown_plot"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #thematic::thematic_shiny()
  
  output$ind_plot <- renderPlot({
    # generate bins based on input$bins from ui.R
    teams_in_race <- input$teams_in_race
    
    all_rosters <- bind_rows(rosters_list[as.character(teams_in_race)])
    all_runners <- map(all_rosters$athlete_id, get_athlete)
    
    
    results <- tibble(all_runners) |>
      unnest_wider(all_runners)
    
    meet_results <- results |>
      unnest(results)
    
    scored_meet <- meet_pred(meet_results)
    team_scored_meet <- team_scorer(scored_meet)
    
    # draw the histogram with the specified number of bins
    scored_meet |> 
      ggplot(aes(fct_reorder(athlete_name, period_to_seconds(ms(mark))), period_to_seconds(ms(mark)), color = current_team_name)) + geom_point()  +
      theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1)) + labs(x = "Runner", y = "Time in Seconde")
    
  })
  
  output$team_plot <- renderPlot({
    # generate bins based on input$bins from ui.R
    teams_in_race <- input$teams_in_race
    
    all_rosters <- bind_rows(rosters_list[as.character(teams_in_race)])
    all_runners <- map(all_rosters$athlete_id, get_athlete)
    
    
    results <- tibble(all_runners) |>
      unnest_wider(all_runners)
    
    meet_results <- results |>
      unnest(results)
    
    scored_meet <- meet_pred(meet_results)
    team_scored_meet <- team_scorer(scored_meet)
    
    # draw the histogram with the specified number of bins
    team_scored_meet |> 
      ggplot(aes(fct_reorder(current_team_name, team_score), team_score, fill = current_team_name)) + geom_col()  +
      theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1)) + labs(x = "Team", y = "Team Score")
    ##########add lines on bars for each athletes weight###########
    
  })
  
  output$team_breakdown_plot <- renderPlot({
    teams_in_race <- input$teams_in_race
    
    all_rosters <- bind_rows(rosters_list[as.character(teams_in_race)])
    all_runners <- map(all_rosters$athlete_id, get_athlete)
    
    
    results <- tibble(all_runners) |>
      unnest_wider(all_runners)
    
    meet_results <- results |>
      unnest(results)
    
    scored_meet <- meet_pred(meet_results)
    team_scored_meet <- team_scorer(scored_meet)
    
    scored_meet |> 
      ggplot(aes(points, period_to_seconds(ms(mark)), colour = current_team_name)) + geom_point() + 
      facet_wrap(vars(current_team_name)) +
      labs(x = "Points", y = "Time in Seconde")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


