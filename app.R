library(leaflet)
library(dplyr)
#library(htmltools)
library(rstudioapi)
library(shiny)
library(rsconnect)

#setwd(dirname(getActiveDocumentContext()$path)) # Set working directory to source file location

obs <- read.csv("obsBisonShiny.csv")

ui <- fluidPage(
  
  titlePanel(h1(tags$strong("Bison in the fossil record"))),
  titlePanel(h5("Explore the distribution of bison through time.")),
  hr(),
  
  sidebarLayout(position = "left",
                sidebarPanel(sliderInput(inputId = 'age',
                                         label = 'Site age (cal yr BP)',
                                         value = c(0, 20000), min = 0, max = 20000
                                         ),
                             
                             # Search age range button
                             actionButton(inputId = "agebound",
                                          label = "Search"
                                          ),
                             
                             # Download data button
                             downloadButton("downloadData", 
                                            "Download"
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
                
                mainPanel(leafletOutput('map', width = "100%", height = "600px"),
                          a(href = "https://johnwendt.github.io", "johnwendt.github.io"))),
  
  
  #tags$p("This is a",
  #       tags$strong("Shiny"),
  #       "app."),

)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    obs[obs$Lower.Age..IntCal20. >= input$age[1] & obs$Upper.Age..IntCal20. <= input$age[2],]
  })
  
  filteredDataDownload <- reactive({
    obs[obs$Lower.Age..IntCal20. >= input$age[1] & obs$Upper.Age..IntCal20. <= input$age[2], -27]
  })
  
  # Show starting map
  output$map <- renderLeaflet({
    leaflet(data=obs) %>% 
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>% # Add base map
      addCircleMarkers(~Longitude, ~Latitude, radius = 1.5, popup = ~label)
  })
  
  # Map updates on click of 'Search' button
  observeEvent(input$agebound, {
    print(input$age)
    print(head(filteredData()))
    leafletProxy("map", data = filteredData()) %>% 
      clearMarkers() %>% 
      addCircleMarkers(~Longitude, ~Latitude, radius = 1.5, popup = ~label)
  })
  
  # Table of selected data set
  output$table <- renderTable({
    filteredDataDownload()
  })
  
  # Downloadable csv of selected data set
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("bison_ocurrences_", input$age[1], "-", input$age[2], ".csv")
      },
    content = function(file) {
      write.csv(filteredDataDownload(), file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui = ui, server = server)