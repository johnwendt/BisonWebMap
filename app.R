library(leaflet)
library(dplyr)
#library(htmltools)
library(rstudioapi)
library(shiny)
library(rsconnect)

#setwd(dirname(getActiveDocumentContext()$path)) # Set working directory to source file location

obs <- read.csv("obsBisonShiny.csv")

ui <- fluidPage(
  
  leafletOutput('map', width = "60%", height = "600px"),
  p(),
  sliderInput(inputId = 'age',
              label = 'Site Age',
              value = c(0, 20000), min = 0, max = 20000),
  actionButton(inputId = "agebound",
               label = "Search")
)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    obs[obs$Lower.Age..IntCal20. >= input$age[1] & obs$Upper.Age..IntCal20. <= input$age[2],]
  })
  
  # Show starting map
  output$map <- renderLeaflet({
    leaflet(data=obs) %>% 
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>% # Add basemap
      addCircleMarkers(~Longitude, ~Latitude, radius = 1.5, popup = ~label)
  })
  
  # Map updates on click
  observeEvent(input$agebound, {
    print(input$age)
    print(head(filteredData()))
    leafletProxy("map", data = filteredData()) %>% 
      clearMarkers() %>% 
      addCircleMarkers(~Longitude, ~Latitude, radius = 1.5, popup = ~label)
  })
  
}

shinyApp(ui = ui, server = server)