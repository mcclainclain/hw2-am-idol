# Imports
library(shiny)
library(tidyverse)
library(plotly)
library(shinythemes)

# Read in data
auditions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-07-23/auditions.csv')
eliminations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-07-23/eliminations.csv')
finalists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-07-23/finalists.csv')
ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-07-23/ratings.csv')
seasons <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-07-23/seasons.csv')
songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-07-23/songs.csv')


# UI
ui <- navbarPage(
  theme = shinytheme(theme="cosmo"),
  title = "American Idol",
  tabPanel("Viewers",
           sidebarLayout(
             sidebarPanel(
               h4("American Idol Viewers"),
               selectizeInput("Season", "Select a season", c("All", paste("Season", unique(ratings$season)))), 
               conditionalPanel(
                 condition = "input.Season == 'All'",
                 checkboxInput("FinalesOnly", "Finales Only", value=F)
               )
             ),
             mainPanel(
               plotlyOutput("viewers")
             )
           )),
  tabPanel("Songs",
           sidebarLayout(
             sidebarPanel(
               h4("American Idol Songs"),
               selectizeInput("SeasonSong", "Select a season", c("All", paste("Season", as.integer(substr(unique(songs$season), 8, 10))))),
               conditionalPanel(
                 condition = "input.SeasonSong == 'All'",
                 checkboxInput("FinalistsOnly", "Finalists Only", value=F)
               )
             ),
             mainPanel(
               plotlyOutput("songs")
             )
           )
  )
  
)