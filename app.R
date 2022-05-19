library(leaflet)
library(dplyr)
library(rstudioapi)
library(shiny)
#library(shinythemes) # broken
library(rsconnect)

obs <- read.csv("obsBisonShiny.csv")

ui <- fluidPage(
  
  #theme = shinytheme("united"), # shinythemes is broken
  tags$head(HTML("<title>Bison Web Map</title>")),
  titlePanel(h1(tags$strong("Explore bison occurrences through time"))),
  #titlePanel(h5("Explore the distribution of bison through time.")),
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
                                            )),

                # Main map
                mainPanel(leafletOutput('map', width = "100%", height = "600px"))),

                
  hr(),
  fluidRow(
    column(3, 
           p("Creator: ", a(href = "https://johnwendt.github.io", "John Wendt")),
           p("Data: ", a(href = "https://www.neotomadb.org/", "Neotoma"), "and ", a(href = "https://www.canadianarchaeology.ca/", "CARD")),
           p("Code: ", a(href = "https://github.com/johnwendt/BisonWebMap", "Github")),
           p("Read the ", a(href = "https://doi.org/10.1016/j.quascirev.2022.107472", "paper"))
           )
  )

)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to user selection
  filteredData <- reactive({
    obs[pmin(obs$Lower.Age..IntCal20., obs$Upper.Age..IntCal20.) <= pmax(input$age[1], input$age[2]) &
        pmax(obs$Lower.Age..IntCal20., obs$Upper.Age..IntCal20.) >= pmin(input$age[1], input$age[2]),]
  })
  
  filteredDataDownload <- reactive({
    obs[pmin(obs$Lower.Age..IntCal20., obs$Upper.Age..IntCal20.) <= pmax(input$age[1], input$age[2]) &
          pmax(obs$Lower.Age..IntCal20., obs$Upper.Age..IntCal20.) >= pmin(input$age[1], input$age[2]), -27]
  })
  
  # Show starting map
  output$map <- renderLeaflet({
    leaflet(data=obs) %>% 
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>% # Add base map
      addCircleMarkers(~Longitude, ~Latitude, radius = 1.75, popup = ~label)
  })
  
  # Map updates on click of 'Search' button
  observeEvent(input$agebound, {
    print(input$age)
    print(head(filteredData()))
    leafletProxy("map", data = filteredData()) %>% 
      clearMarkers() %>% 
      addCircleMarkers(~Longitude, ~Latitude, radius = 1.75, popup = ~label)
  })
  
  # Table of selected data set
  output$table <- renderTable({
    filteredDataDownload()
  })
  
  # Downloadable csv of selected data set
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("bison_occurrences_", input$age[1], "-", input$age[2], ".csv")
      },
    content = function(file) {
      write.csv(filteredDataDownload(), file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui = ui, server = server)