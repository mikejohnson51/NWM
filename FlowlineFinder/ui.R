library(leaflet)
library(ggmap)

fluidPage(
  
  tags$head(
    tags$script(src="getIP.js")
  ),
  
  # Application title
  titlePanel("Flowline Finder"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      
      textInput(inputId = 'place', label = 'Location', ""),
      
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
      tableOutput("stations"),
      tableOutput("Flowlines")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("map", height = "600px", width = "100%")
    )
  )
)