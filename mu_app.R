library(shiny)
library(tidyverse)
library(reshape2)
library(ggnewscale)
library(teamcolors)
library(janitor)
library(gridExtra)

team_fpts <- read_csv("team_fpts.csv") %>%
  clean_names()

team_fpts[team_fpts == 0] <- .01

###data manipulation
off <- team_fpts %>% 
  select(abv, team, primary, qb_rank, rb_rank, wr_rank, te_rank)

def <- team_fpts %>% 
  select(abv, team, primary, qb_def_rank, rb_def_rank, wr_def_rank, te_def_rank)

# Define UI for application
ui <- pageWithSidebar(
  headerPanel("Fantasy Football Matchups App"),
  
  sidebarPanel(
    uiOutput("select_tm1"),
    uiOutput("select_tm2")
  ),
  
  mainPanel(
    plotOutput("muPlot", height = "300px"),
    plotOutput("muPlot2", height = "300px")
  )
)  
# Define server logic
server <- function(input, output, session) {
   
  output$select_tm1 <- renderUI({
    selectInput("team1", "Select Team", choices = team_fpts$abv, selected = TRUE)
  })
  
  output$select_tm2 <- renderUI({
    selectInput("team2", "Select Team", choices = team_fpts$abv, selected = TRUE)
  })
  
  output$muPlot <- renderPlot({
      ggplot() + 
      geom_col(data = off %>%
                 filter(abv == input$team1) %>% #input$team1
                 mutate(OPP = input$team2) %>% #input$team2
                 left_join(select(def, abv, team, qb_def_rank), by = c("OPP" = "abv")) %>% 
                 melt() %>%
                 filter(variable %in% c("qb_rank", "qb_def_rank")) %>%
                 mutate(QB_FPPG = "QB FPPG"),
               aes(QB_FPPG, value, fill = variable), color = "black", position = "dodge") +
      scale_fill_manual(values = off %>%
                          filter(abv == input$team1) %>% #input$team1
                          mutate(OPP = input$team2) %>% #input$team2
                          left_join(select(def, abv, team, primary), by = c("OPP" = "abv")) %>% 
                          select(abv, primary.x, primary.y) %>% 
                          gather(key = abv) %>% 
                          pull(value), 
                          labels = c(paste0(input$team1, " Offense"), paste0(input$team2, " Defense"))) +
      new_scale("fill") +
      geom_col(data = off %>%
                 filter(abv == input$team1) %>% #input$team1
                 mutate(OPP = input$team2) %>% #input$team2
                 left_join(select(def, abv, team, wr_def_rank), by = c("OPP" = "abv")) %>% 
                 melt() %>%
                 filter(variable %in% c("wr_rank", "wr_def_rank")) %>%
                 mutate(WR_FPPG = "WR FPPG"),
               aes(WR_FPPG, value, fill = variable), color = "black", position = "dodge") +
      scale_fill_manual(values = off %>%
                          filter(abv == input$team1) %>% #input$team1
                          mutate(OPP = input$team2) %>% #input$team2
                          left_join(select(def, abv, team, primary), by = c("OPP" = "abv")) %>% 
                          select(abv, primary.x, primary.y) %>% 
                          gather(key = abv) %>% 
                          pull(value), 
                        labels = c(paste0(input$team1, " Offense"), paste0(input$team2, " Defense"))) +
      new_scale("fill") +
      geom_col(data = off %>%
                 filter(abv == input$team1) %>% #input$team1
                 mutate(OPP = input$team2) %>% #input$team2
                 left_join(select(def, abv, team, rb_def_rank), by = c("OPP" = "abv")) %>% 
                 melt() %>%
                 filter(variable %in% c("rb_rank", "rb_def_rank")) %>%
                 mutate(RB_FPPG = "RB FPPG"),
               aes(RB_FPPG, value, fill = variable), color = "black", position = "dodge") +
      scale_fill_manual(values = off %>%
                          filter(abv == input$team1) %>% #input$team1
                          mutate(OPP = input$team2) %>% #input$team2
                          left_join(select(def, abv, team, primary), by = c("OPP" = "abv")) %>% 
                          select(abv, primary.x, primary.y) %>% 
                          gather(key = abv) %>% 
                          pull(value), 
                        labels = c(paste0(input$team1, " Offense"), paste0(input$team2, " Defense"))) +
      new_scale("fill") +
      geom_col(data = off %>%
                 filter(abv == input$team1) %>% #input$team1
                 mutate(OPP = input$team2) %>% #input$team2
                 left_join(select(def, abv, team, te_def_rank), by = c("OPP" = "abv")) %>% 
                 melt() %>%
                 filter(variable %in% c("te_rank", "te_def_rank")) %>%
                 mutate(TE_FPPG = "TE FPPG"),
               aes(TE_FPPG, value, fill = variable), color = "black", position = "dodge") +
      scale_fill_manual(values = off %>%
                          filter(abv == input$team1) %>% #input$team1
                          mutate(OPP = input$team2) %>% #input$team2
                          left_join(select(def, abv, team, primary), by = c("OPP" = "abv")) %>% 
                          select(abv, primary.x, primary.y) %>% 
                          gather(key = abv) %>% 
                          pull(value), 
                        labels = c(paste0(input$team1, " Offense"), paste0(input$team2, " Defense"))) +
      scale_y_continuous(limits = c(0,1)) +
      labs(x="", y="NFL Percentile Rank",
           title = paste0(input$team1, " Offense vs. ", input$team2, " Defense"),
           subtitle = "To find matchup advantages for the offense look for tall offense bars and short defense bars") +
      theme_minimal() +
      theme(legend.title = element_blank(),
            plot.title = element_text(face = "bold", size = 16),
            plot.subtitle = element_text(size = 12))
      
  })
  
  output$muPlot2 <- renderPlot({
    ggplot() + 
      geom_col(data = off %>%
                 filter(abv == input$team2) %>% #input$team1
                 mutate(OPP = input$team1) %>% #input$team2
                 left_join(select(def, abv, team, qb_def_rank), by = c("OPP" = "abv")) %>% 
                 melt() %>%
                 filter(variable %in% c("qb_rank", "qb_def_rank")) %>%
                 mutate(QB_FPPG = "QB FPPG"),
               aes(QB_FPPG, value, fill = variable), color = "black", position = "dodge") +
      scale_fill_manual(values = off %>%
                          filter(abv == input$team2) %>% #input$team1
                          mutate(OPP = input$team1) %>% #input$team2
                          left_join(select(def, abv, team, primary), by = c("OPP" = "abv")) %>% 
                          select(abv, primary.x, primary.y) %>% 
                          gather(key = abv) %>% 
                          pull(value), 
                        labels = c(paste0(input$team2, " Offense"), paste0(input$team1, " Defense"))) +
      new_scale("fill") +
      geom_col(data = off %>%
                 filter(abv == input$team2) %>% #input$team1
                 mutate(OPP = input$team1) %>% #input$team2
                 left_join(select(def, abv, team, wr_def_rank), by = c("OPP" = "abv")) %>% 
                 melt() %>%
                 filter(variable %in% c("wr_rank", "wr_def_rank")) %>%
                 mutate(WR_FPPG = "WR FPPG"),
               aes(WR_FPPG, value, fill = variable), color = "black", position = "dodge") +
      scale_fill_manual(values = off %>%
                          filter(abv == input$team2) %>% #input$team1
                          mutate(OPP = input$team1) %>% #input$team2
                          left_join(select(def, abv, team, primary), by = c("OPP" = "abv")) %>% 
                          select(abv, primary.x, primary.y) %>% 
                          gather(key = abv) %>% 
                          pull(value), 
                        labels = c(paste0(input$team2, " Offense"), paste0(input$team1, " Defense"))) +
      new_scale("fill") +
      geom_col(data = off %>%
                 filter(abv == input$team2) %>% #input$team1
                 mutate(OPP = input$team1) %>% #input$team2
                 left_join(select(def, abv, team, rb_def_rank), by = c("OPP" = "abv")) %>% 
                 melt() %>%
                 filter(variable %in% c("rb_rank", "rb_def_rank")) %>%
                 mutate(RB_FPPG = "RB FPPG"),
               aes(RB_FPPG, value, fill = variable), color = "black", position = "dodge") +
      scale_fill_manual(values = off %>%
                          filter(abv == input$team2) %>% #input$team1
                          mutate(OPP = input$team1) %>% #input$team2
                          left_join(select(def, abv, team, primary), by = c("OPP" = "abv")) %>% 
                          select(abv, primary.x, primary.y) %>% 
                          gather(key = abv) %>% 
                          pull(value), 
                        labels = c(paste0(input$team2, " Offense"), paste0(input$team1, " Defense"))) +
      new_scale("fill") +
      geom_col(data = off %>%
                 filter(abv == input$team2) %>% #input$team1
                 mutate(OPP = input$team1) %>% #input$team2
                 left_join(select(def, abv, team, te_def_rank), by = c("OPP" = "abv")) %>% 
                 melt() %>%
                 filter(variable %in% c("te_rank", "te_def_rank")) %>%
                 mutate(TE_FPPG = "TE FPPG"),
               aes(TE_FPPG, value, fill = variable), color = "black", position = "dodge") +
      scale_fill_manual(values = off %>%
                          filter(abv == input$team2) %>% #input$team1
                          mutate(OPP = input$team1) %>% #input$team2
                          left_join(select(def, abv, team, primary), by = c("OPP" = "abv")) %>% 
                          select(abv, primary.x, primary.y) %>% 
                          gather(key = abv) %>% 
                          pull(value), 
                        labels = c(paste0(input$team2, " Offense"), paste0(input$team1, " Defense"))) +
      scale_y_continuous(limits = c(0,1)) +
      labs(x="", y="NFL Percentile Rank",
           title = paste0(input$team2, " Offense vs. ", input$team1, " Defense"),
           subtitle = "To find matchup advantages for the offense look for tall offense bars and short defense bars") +
      theme_minimal() +
      theme(legend.title = element_blank(),
            plot.title = element_text(face = "bold", size = 16),
            plot.subtitle = element_text(size = 12))
    
  })
}
# Run the application 
shinyApp(ui = ui, server = server)

