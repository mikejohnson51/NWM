library(leaflet)
library(ggmap)
library(sp)
library(rgdal)

usgsStation = load('usgsStations.Rdata')

function(input, output, session) {
  


  IP <- reactive({
    input$getIP
  })
  
  observe({
    loc = paste(round(as.numeric(IP()$latitude),2), round(as.numeric(IP()$longitude),2), collapse="")
    x <- input$controller
    updateTextInput(session, "place", value = paste(loc, x))
  })

  output$map = renderLeaflet({
    
    #get_location
    
    target_pos = geocode(input$place)
    LAT = target_pos$lat
    LONG = target_pos$lon
    
    #get bounding box
    
    df = (input$AOIheight/2)/69
    dl = ((input$AOIwidth/2)/69) / cos(LAT * pi/180)
    
    south = LAT - df
    north = LAT + df
    west  = LONG - dl
    east  = LONG + dl
    
    dir.create(paste0(getwd(),"/Flowlines"))
    
    URL = paste0("https://cida.usgs.gov/nwc/geoserver/nhdplus/ows?service=WFS&version=2.0.0&request=GetFeature&typeNames=nhdplus:nhdflowline_network&srsName=EPSG:4326&bbox=",
                 south, ",",
                 west,  ",",
                 north, ",",
                 east,
                 "&outputFormat=SHAPE-ZIP")
    
    destfile = paste0(getwd(), "/Flowlines/flowlines")
    
    download.file(url = URL, destfile = destfile)
    
    unzip(destfile, exdir = paste0(getwd(),"/Flowlines"))
    
    flowlines_84 = spTransform(readOGR(paste0(getwd(),"/Flowlines/",
                                              "nhdflowline_network.shp")),
                               CRS("+proj=longlat +datum=WGS84"))
    
    table = rbind(cbind("Number of Flowlines: ", length(flowlines_84)),
                  cbind("Largest Stream Order: ", max(flowlines_84@data$streamorde)),
                  cbind("Total Area (miles2): ", input$AOIheight * input$AOIwidth),
                  cbind("Unique HUC6 units: ", length(unique(as.numeric(na.omit(unique(substr(flowlines_84$reachcode,1,6))))))))
    colnames(table) = c('Statistic', 'Value')
    output$Flowlines = renderTable(table)
    
    ##############################################################################
    
    usgs1 = subset(usgsStations, usgsStations$lat_reachCent < flowlines_84@bbox[2,2])
    usgs2 = subset(usgs1, usgs1$lat_reachCent > flowlines_84@bbox[2,1])
    usgs3 = subset(usgs2, usgs2$lon_reachCent > flowlines_84@bbox[1,1])
    usgs4 = subset(usgs3, usgs3$lon_reachCent < flowlines_84@bbox[1,2])
    
    usgs_final = cbind(usgs4$feature_id, usgs4$site_no, usgs4$lat_reachCent, usgs4$lon_reachCent)
    colnames(usgs_final) = c("COMID", "USGSgage","lat", "long")
    
    usgs_final = as.matrix(usgs_final)
    
    output$stations = renderTable(usgs_final[,1:2], spacing = c("xs"), width = "25px")
    
    coors = cbind(as.numeric(usgs_final[,4]), as.numeric(usgs_final[,3]))
    if(length(coors)>0){
      sp = SpatialPoints(coors, proj4string = CRS("+proj=longlat +datum=WGS84"))}
    
    num <- usgs_final[,2]  # site number
    nam <- usgs_final[,1] # local site name
    url <- sprintf("https://waterdata.usgs.gov/nwis/inventory/?site_no=%s", num)
    url_call = paste0('<a href=', url,'>',num,"</a>")
    pop <- sprintf("COMID: %s
                   Site No: %s ",
                   nam, url_call, num)
    
    usgsIcon = makeIcon(
      iconUrl= "https://upload.wikimedia.org/wikipedia/commons/0/08/USGS_logo.png",
      iconWidth = 40, iconHeight = 20,
      iconAnchorX = 20, iconAnchorY = 10)
    
    redpin = makeIcon(
      iconUrl= "http://www.clker.com/cliparts/W/0/g/a/W/E/map-pin-red.svg",
      iconWidth = 30, iconHeight = 30,
      iconAnchorX = 10, iconAnchorY = 10)
    
    ##############################################################################
    
    leaflet() %>%
      fitBounds(west, south, east, north) %>%
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
      addTiles(group = "OSM"
      ) %>%
      
      #addMarkers(lng =LONG, lat = LAT, label = input$place,
      #      group = 'AOI', icon = redpin
      #    ) %>%
      
      addRectangles(
        lng1 = west , lat1 = north,
        lng2 = east , lat2 = south,
        fillColor = "transparent",
        group = 'AOI', color = "red"
      ) %>%
      
      addPolylines(data = flowlines_84, color = 'blue', weight = flowlines_84$streamorde,
                   label = paste0(paste0(flowlines_84@data$gnis_name),
                                  paste0(" COMID:", flowlines_84$comid)),
                   group = "NHD Flowlines"
      )%>%
      
      addMarkers(data = usgs_final,
                 lng = as.numeric(usgs_final[,4]),
                 lat = as.numeric(usgs_final[,3]),
                 icon = usgsIcon,
                 popup = pop, group = "USGS Stations"
      )%>%
      
      addLayersControl(
        baseGroups = c("CartoDB","Imagery", "OSM"),
        overlayGroups = c("USGS Stations", "NHD Flowlines", "AOI"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      
      addScaleBar("bottomleft")
    
  })
}