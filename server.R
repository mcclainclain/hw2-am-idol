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


# All Seasons Viewer Plots

# Finales only dataframe (Gets the final show only of each season)
finales = ratings %>% 
  group_by(season) %>% 
  slice_max(show_number)

# Ratings graph for the full season
full_season_ratings = ggplotly(ratings %>%
                                 left_join(finales %>% mutate(finale=TRUE)) %>%
                                 mutate(pointSize=ifelse(is.na(finale), 0, 1)) %>% 
                                 ggplot(aes(x=season, y=viewers_in_millions, text=paste(
                                   "Show Date: ", airdate,
                                   "\nSeason ", season, ", Episode ", show_number, "\n", episode,
                                   "\n\n", viewers_in_millions, " million viewers", sep=""
                                 ))) +
                                 geom_jitter(aes(size=pointSize>0, color=pointSize>0), width=0.1) +
                                 scale_x_continuous(breaks=ratings$season) +
                                 scale_size_manual(values=c(0.75, 1.5)) +
                                 scale_color_manual(name="Episode Type", values=c("#361e12", "purple"), labels=c("Regular", "Finale")) +
                                 labs(
                                   x = "Season", y = "Viewers (Millions)", title = "Viewers by Season", caption="Finale episodes highlighted in purple") +
                                 theme(legend.position="none")
                               , tooltip = "text", style="plotly_dark") %>% 
  layout(annotations = 
           list(x = 1, y = -0.14, text = "Finale Episodes", 
                showarrow = F, xref='paper', yref='paper', 
                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                font=list(size=12, color="purple"))
  )

# Ratings graph for finales only
finale_ratings = ggplotly(finales %>% 
                            ggplot(aes(x=season, y=viewers_in_millions)) +
                            geom_point(color="purple", aes(text=paste(
                              "Show Date: ", airdate,
                              "\nSeason ", season, ", Episode ", show_number, "\n", episode,
                              "\n\n", viewers_in_millions, " million viewers", sep=""
                            ))) +
                            scale_x_continuous(breaks=ratings$season) +
                            geom_smooth(se=F, color="#361e12") +
                            labs(
                              x = "Season", y = "Viewers (Millions)", title = "Viewers by Season (Finales)")
                          , tooltip="text")




# Server
server <- function(input, output) {
  output$viewers <- renderPlotly({
    if (input$Season == "All") {
      if (input$FinalesOnly == T) {finale_ratings}
      else {full_season_ratings}
    }
    else {
      ggplotly(ratings %>% 
                 filter(season==str_split_i(input$Season, " ", 2)) %>% 
                 ggplot(aes(x=show_number, y=viewers_in_millions)) +
                 geom_point(color="purple", aes(text=paste(
                   "Show Date: ", airdate,
                   "\nSeason ", season, ", Episode ", show_number, "\n", episode,
                   "\n\n", viewers_in_millions, " million viewers", sep=""
                 ))) +
                 scale_x_continuous(breaks=ratings$show_number) +
                 geom_smooth(se=F, color="#361e12") +
                 labs(
                   x = paste(input$Season, " Episode", sep=""), y = "Viewers (Millions)", title = paste("Viewers by Episode (", input$Season, ")", sep=""))
               , tooltip="text")
    }
  })
  
  output$songs = renderPlotly({
    if (input$SeasonSong == "All") {
      if (input$FinalistsOnly == F){
        ggplotly(songs %>% 
                   left_join(finalists %>% mutate(finalist=T) %>% select(Contestant, finalist), by=join_by(contestant==Contestant)) %>%
                   count(artist) %>% 
                   arrange(desc(n)) %>% 
                   top_n(10) %>% 
                   ggplot(aes(n, reorder(artist, n))) +
                   geom_col(aes(text = paste("Artist:", artist, "\nTimes Covered:", n)),
                            fill="lightblue", color="black") +
                   labs(
                     x="Times Covered",
                     y="Artist",
                     title="American Idol: Most Covered Artists All-Time"
                   ),
                 tooltip="text", 
        ) %>% 
          layout(hoverlabel=list(bgcolor="black"))
      } else {
        ggplotly(songs %>% 
                   left_join(finalists %>% mutate(finalist=T) %>% select(Contestant, finalist), by=join_by(contestant==Contestant)) %>%
                   filter(finalist == T) %>% 
                   count(artist) %>% 
                   arrange(desc(n)) %>% 
                   top_n(10) %>% 
                   ggplot(aes(n, reorder(artist, n))) +
                   geom_col(aes(text = paste("Artist:", artist, "\nTimes Covered:", n)),
                            fill="lightblue", color="black") +
                   labs(
                     x="Times Covered",
                     y="Artist",
                     title="American Idol: Most Covered Artists All-Time (By Finalists)"
                   ),
                 tooltip="text", 
        ) %>% 
          layout(hoverlabel=list(bgcolor="black"))
      }
    }
    else {
      ggplotly(songs %>% 
                 mutate(season_adj = as.integer(substr(season, 8, 10))) %>% 
                 left_join(finalists %>% mutate(finalist=T) %>% select(Contestant, finalist), by=join_by(contestant==Contestant)) %>%
                 filter(paste("Season", season_adj) == input$SeasonSong) %>% 
                 count(artist) %>% 
                 arrange(desc(n)) %>% 
                 top_n(10) %>% 
                 filter(n > 1) %>% # Only songs played multiple times
                 head(25) %>% # Limit ties shown
                 ggplot(aes(n, reorder(artist, n))) +
                 geom_col(aes(text = paste("Artist:", artist, "\nTimes Covered:", n)),
                          fill="lightblue", color="black") +
                 labs(
                   x="Times Covered",
                   y="Artist",
                   title=paste("American Idol ", input$SeasonSong, ": ", "Most Covered Artists",sep="")
                 ),
               tooltip="text",
      ) %>% 
        layout(hoverlabel=list(bgcolor="black"))
    }
  })
}