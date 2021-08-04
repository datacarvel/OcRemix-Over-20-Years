
library(rsconnect)
library(tidyverse)
library(rvest)
library(rlist)
library(httr)
library(lubridate)
library(patchwork)
library(ggbeeswarm)
library(extrafont)
library(Cairo)
library(plotly)
library(scales)
library(png)
library(grid)
library(ggtext)
library(shiny)
library(stringi)
library(shinythemes)
library(htmlwidgets)

# Shiny app 2 interactive

# Dataset name is a bit long 

OC_Shiny1 <- OC_musicgenres_withoutNA %>% # Same dataset, for the Shiny plot 
  mutate(GameAcc = stri_trans_general(Game, "Latin-ASCII")) %>% # Removing accents like é in Pokémon
  select("Song Title" = SongTitle, Artist, Game = GameAcc, Date, Genres, "Track URL" = URL)

OC_Shiny2 <- OC_data %>% # Same dataset, for the Shiny datatable
  select(c(1:5, 7, 8)) %>%
  dplyr::filter(URLfound == TRUE) %>%
  drop_na(Genres) %>%
  mutate(GenreV = as.character(Genres) %>% # This part deals with Genre Tags that are currently a list within a column, but I want to concatenate them, separated by a column and one whitespace
           str_replace_all("c\\(", "") %>% # From list to vector, this removes the followwing two characters c(
           str_replace_all(",", "9") %>% # I have to remove all punctuation except the comma (,), so I turn it into something else and bring it back later, with 9
           str_replace_all("-", "8") %>% # Same, but for the dash (-), like in "hip-hop"
           str_replace_all("[:punct:]", "") %>% 
           str_replace_all("9", ",") %>%
           str_replace_all("8", "-"), 
         GameAccent = stri_trans_general(Game, "Latin-ASCII") # Removing accents like é in Pokémon
  ) %>%
  select("Song Title" = SongTitle, Artist, Game = GameAccent, Date, "Genre(s)" = GenreV, "Track URL" = URL)

# Hello Plotly ! We're making it interactive and we will put the full plot in a separate tab within the Shiny app
# To do patchwork-like tasks with plotly = https://stackoverflow.com/questions/61574401/combine-ggplotly-and-ggplot-with-patchwork

ui <- fluidPage(
  
  title = "OCREMIX.ORG : How often was your favorite game soundtrack remixed?",
  titlePanel(h3("OCREMIX.ORG : How often was your favorite game soundtrack remixed?", style = "font-family: Arial Black; color: #17e39f;")),
  titlePanel(HTML("<h5 style = 'font-family: Arial; color: white'>Each dot is a remix/rearrangement! Move your mouse on a dot to reveal the track, enlarge an area by clicking, holding and stretching, or select the pan (cross with arrows) to move around! <strong>Works best on large screens</strong>.</h5>")),
  titlePanel(HTML("<h5 style = 'font-family: Arial; color: white'>The table further below lists all found tracks with all their info, including their link so you can play them! You can also narrow down your prior search in the table by music genre, date, artist, etc.</h5>")),
  tabsetPanel(
    
    tabPanel("Search for a game or franchise", 
             sidebarLayout(
               sidebarPanel(textInput('name', 'Enter the title of a game/franchise (ignore accents and special characters)', "Zelda")),
               mainPanel(img(src = "banner9.png", align = "center"))
             ),
             plotly::plotlyOutput('trend', height = "100%"),
             dataTableOutput('TracksTable')),
    
    tabPanel("Look at all of them", plotly::plotlyOutput('fullviz', height = "100%")
    )
  ),
  titlePanel(em(h5("As of July 31st, 2021"))),
  titlePanel(h5("This chart presents all OverClocked Remix tracks that 1) have an individual web post and 2) were given at least one music genre tag. Some individually published remixes/rearrangements have no genre tags, while many, many other tracks are part of an album but were not the object of an individual web post. Check out their entire discography of 140 albums here:", a("https://ocremix.org/albums/")
  )),
  titlePanel(
    h5("made by", strong("@stevecarufel_ / scarufel.com"), "with the {tidyverse}, Shiny and Plotly")
  ),
  theme = shinythemes::shinytheme("darkly")
  
)

# size = if_else(forsize > 40, 6, 9

server <- function(input, output, session) {
  
  output$trend <- plotly::renderPlotly({
    
    validate(
      #need(input$name != "", "Try something! Like 'Mario', 'Undertale', etc.!"),
      need(nrow(OC_Shiny1 %>% filter(str_detect(Game, regex(input$name, ignore_case = TRUE)))) > 0, "No results! Try something else... Have you misspelled?")
    )
    
    ToPlotly <- ggplot(OC_Shiny1 %>% filter(str_detect(Game, regex(input$name, ignore_case = TRUE))), aes(text = paste("Game:", Game, "<br>", "Track:", '"', `Song Title`, '"', "<br>", "Artist:", Artist))) +
      geom_beeswarm(groupOnX = FALSE, priority = "density", aes(x = Date, y = fct_rev(fct_infreq(Genres)), color = Genres)) +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#222222"),
        plot.background = element_rect(fill = "#222222"),
        axis.title = element_blank(),
        legend.position = "none",
        plot.margin = margin(0, 0, 0, 0),
        axis.text.y = element_text(family = "Pixel Emulator", color = "#00fafd"),
        axis.text.x = element_text(family = "Pixelmania", size = 5, color = "#f8c828", margin = unit(c(8, 0, 0, 0), "pt")),
        plot.caption = element_text(family = "Impact", color = "#12b380", hjust = 1)
      )
    
    ggplotly(ToPlotly, tooltip = c("Genres", "text", "x"), height = 650) %>%
      config(modeBarButtonsToRemove = c("lasso2d", "select2d", "resetScale2d", "toggleSpikelines", "hoverClosestCartesian", "hoverCompareCartesian")) %>%
      layout(yaxis = list(autorange = TRUE))
    
  })
  
  results <- function() {
    results <- OC_Shiny2 %>%
      mutate(`Track URL` = paste0('<a href="', `Track URL`, '">', `Track URL`, '</a>')) %>%
      filter(str_detect(Game, regex(input$name, ignore_case = TRUE)))
  }
  
  output$TracksTable <- renderDataTable({
    results()
  }, escape = c(1:4), options = list(pageLength = 10))
  
  output$fullviz <- plotly::renderPlotly({
    
    ToPlotly2 <- ggplot(OC_Shiny1, aes(text = paste("Game:", Game, "<br>", "Track:", '"', `Song Title`, '"', "<br>", "Artist:", Artist))) +
      geom_beeswarm(groupOnX = FALSE, priority = "density", aes(x = Date, y = fct_rev(fct_infreq(Genres)), color = Genres), cex = 0.2, size = 0.7) +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#222222"),
        plot.background = element_rect(fill = "#222222"),
        axis.title = element_blank(),
        legend.position = "none",
        axis.text.y = element_text(family = "Pixel Emulator", color = "#00fafd"),
        axis.text.x = element_text(family = "Pixelmania", size = 5, color = "#f8c828", margin = unit(c(0, 0, 0, 0), "pt"))
      ) + 
      coord_cartesian(clip = "off", expand = FALSE) +
      xlim(as.Date("1999-11-09"), as.Date("2021-08-31"))
    
    ggplotly(ToPlotly2, height = 1000, tooltip = c("Genres", "text", "x")) %>%
      config(modeBarButtonsToRemove = c("lasso2d", "select2d", "resetScale2d", "toggleSpikelines", "hoverClosestCartesian", "hoverCompareCartesian")) %>%
      layout(yaxis = list(autorange = TRUE), margin = list(l = 0, r = 40, b = 0, t = 0, pad = 15))
    
  })
  
}

shinyApp(ui = ui, server = server)