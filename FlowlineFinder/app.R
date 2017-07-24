setwd("/Users/mikejohnson/nwm1/FlowlineFinder/app.R/")

library(shiny)
library(leaflet)
library(ggmap)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Flowline Finder"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         
        textInput(inputId = 'place', label = 'location', value = ""),
  
        sliderInput("AOIwidth",
                     "width of bounding box (miles):",
                     min = 1,
                     max = 50,
                     value = 10, post = ' miles'),
        
        sliderInput("AOIheight",
                    "height of bounding box (miles):",
                    min = 1,
                    max = 50,
                    value = 10, post = ' miles'),
        
        actionButton("goButton",label = "Find Flowlines")
      )),
      
      # Show a plot of the generated distribution
      
       mainpanel(
         leafletOutput("map", height = "600px")
      )
   )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$map = renderLeaflet({
    
    target_pos = geocode(input$place)
    LAT = target_pos$lat
    LONG = target_pos$lon
    
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite)
        %>%
      addMarkers(lng = LONG, lat = LAT, pop = input$place)
  })  

}

# Run the application 
shinyApp(ui = ui, server = server)

