library(leaflet)
library(dplyr)
#library(htmltools)
library(rstudioapi)
library(shiny)
library(rsconnect)

#setwd(dirname(getActiveDocumentContext()$path)) # Set working directory to source file location

obs <- read.csv("obsBisonShiny.csv")

ui <- fluidPage(
  
  titlePanel(h1(tags$i("Bison "),"in the fossil record")),
  titlePanel(h5("20,000 - 0 calendar years before present")),
  titlePanel(h5("Explore the distribution of bison through time.")),
  
  sidebarLayout(position = "left",
                sidebarPanel(sliderInput(inputId = 'age',
                                         label = 'Site Age',
                                         value = c(0, 20000), min = 0, max = 20000
                                         ),
                             
                             actionButton(inputId = "agebound",
                                          label = "Search"
                                          ),
                             
                             #p(
                             #  br(),
                             #  "Read the paper: ",
                             #  a(href = "", "")
                             #  ),
                             
                             p(
                                br(),
                               a(href = "https://johnwendt.github.io", "johnwendt.github.io"))
                             ),
                
                mainPanel(leafletOutput('map', width = "100%", height = "600px"))),
  
  #tags$p("This is a",
  #       tags$strong("Shiny"),
  #       "app."),

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