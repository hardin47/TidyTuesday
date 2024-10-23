#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(maps)

cia <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-22/cia_factbook.csv') |> 
  mutate(iso_a3 = countrycode::countrycode(
    country,
    origin = "country.name",
    destination = "iso3c"
  )) |> 
  drop_na(iso_a3)

vars <- setdiff(names(cia), c("country", "iso_a3"))

world_data <- map_data("world") |>   # ggplot2
  mutate(iso_a3 = countrycode::countrycode(
    region,
    origin = "country.name",
    destination = "iso3c"
  )) |> 
  drop_na(iso_a3)

data4map <- world_data |> 
  left_join(cia, by = "iso_a3")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  pageWithSidebar(
    headerPanel('CIA Factbook map'),
    sidebarPanel(
          selectInput('xcol', 'color variable', vars),
          HTML('<p>The data come from <a href = "https://www.cia.gov/the-world-factbook/" target = "_blank"> CIA World Factbook </a> in 2014</p>'),
          HTML('<p>and were collated at <a href = "https://github.com/rfordatascience/tidytuesday/tree/master/data/2024/2024-10-22" target = "_blank">TidyTuesday 10/21/2024</a>.</p>'),
          HTML("<p>The <em>World Factbook</em> provides basic intelligence on the history, people, government, economy, energy, geography, environment, communications, transportation, military, terrorism, and transnational issues for 265 world entities.</p>")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("mapPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    output$mapPlot <- renderPlot({
      
      data4map |> 
        ggplot(aes(x = long, y = lat, group = group)) + 
        geom_polygon(aes_string(fill = input$xcol)) + 
        theme_void() + 
        scale_fill_gradient(trans = "log")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
