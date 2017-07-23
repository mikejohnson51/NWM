#' Get NHD Flowlines
#' 
#' This function takes a place name, generates a bounding box of a specified size using this 
#' place name as a centroid, and downloads
#' the contained NHDflowlines. The results are displayed in the RStudio viewer via leaflet. By hovering over each flow line
#' user can see the stream/river name and the respective COMID.
#' 
#' @param 
#' 
#' place this can be a lat,long pair, a name such as 'UCSB' or 'Austin' or a general search such as 
#' 'the walmart near Tuscaloosa'. The API engine is driven by google via the ggmap package and thus 
#' will allow queries simular to Google Maps.
#' 
#' AOI.height this value defines the height,in miles, of the bounding box  that will be used to extract flowlines.Default is set to 10 miles.
#' 
#' AOI.width this value defines the width,in miles, of the bounding box  that will be used to extract flowlines.Default is set to 10 miles.
#'
#'
#' @examples 
#' get_flowlines("Walmart in Tuscaloosa AL")
#' 
#' get_flowlines("34.42 -119.69")
#' 
#' get_flowlines("St louis arch")
#' 
#' get_flowlines("UCSB")
#' 
#' @author 
#' 
#' Mike Johnson
#' 
#' @export



get_flowlines = function(place = NULL, AOI.height = 10, AOI.width = 10){
  
  #place = "ceder point iowa"
  
  dir.create(paste0(getwd(),"/", gsub(" ", "", place)))
  setwd(paste0(getwd(),"/", gsub(" ", "", place)))
  
  build_files()
  
  location = as.numeric(geocode(place))
  
  df = (AOI.height/2)/69
  dl = ((AOI.width/2)/69) / cos(location[2] * pi/180)
  
  south = location[2] - df
  north = location[2] + df
  west  = location[1] - dl
  east  = location[1] + dl
  
  URL = paste0("https://cida.usgs.gov/nwc/geoserver/nhdplus/ows?service=WFS&version=2.0.0&request=GetFeature&typeNames=nhdplus:nhdflowline_network&srsName=EPSG:4326&bbox=",
               south, ",",
               west,  ",",
               north, ",",
               east,
               "&outputFormat=SHAPE-ZIP")
  
  destfile = paste0(getwd(), "/Geospatial/Flowlines/", gsub(" ","", place), "_flowlines")
  
  download.file(url = URL, destfile = destfile)
  unzip(destfile, exdir =paste0(getwd(),"/Geospatial/Flowlines"))
  
  
  flowlines = readOGR(paste0(getwd(),'/Geospatial/Flowlines/nhdflowline_network.shp'))
  flowlines_84 = spTransform(flowlines, CRS("+proj=longlat +datum=WGS84"))
  
  usgs1 = subset(usgsStations, usgsStations$lat_reachCent < flowlines_84@bbox[2,2])
  usgs2 = subset(usgs1, usgs1$lat_reachCent > flowlines_84@bbox[2,1])
  usgs3 = subset(usgs2, usgs2$lon_reachCent > flowlines_84@bbox[1,1])
  usgs4 = subset(usgs3, usgs3$lon_reachCent < flowlines_84@bbox[1,2])
  
  
  print("=====================================================")
  print("=====================================================")
  print(paste0("There are ", dim(usgs4)[1], " USGS stations in this area!"))
  print("=====================================================")
  print("=====================================================")
  
  
  usgs_final = cbind(usgs4$feature_id, usgs4$site_no, usgs4$lat_reachCent, usgs4$lon_reachCent)
  colnames(usgs_final) = c("COMID", "USGSgage","lat", "long")
  
  usgs_final = as.matrix(usgs_final)
  
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
  
  m = leaflet() %>%
    
    addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%
    addTiles(group = "OSM") %>%
    
    
    addRectangles(
      lng1 = west , lat1 = north,
      lng2 = east , lat2 = south,
      fillColor = "transparent",
      group = 'AOI'
    ) %>%
    
    
    addMarkers(lng = location[1], lat = location[2],label = place, group = 'AOI') %>%
    
    addPolylines(data = flowlines_84, color = 'blue', weight = flowlines_84$streamorde, 
                 label = paste0(paste0(flowlines_84@data$gnis_name),
                                paste0(" COMID:", flowlines_84$comid)
                 ), group = "NHD Flowlines")%>%
    
    
    addMarkers(data = usgs_final,
               lng = as.numeric(usgs_final[,4]), 
               lat = as.numeric(usgs_final[,3]),
               icon = usgsIcon,
               popup = pop, group = "USGS Stations")%>%
    
    addLayersControl(
      baseGroups = c("CartoDB", "OSM"),
      overlayGroups = c("USGS Stations", "NHD Flowlines", "AOI"),
      options = layersControlOptions(collapsed = TRUE)
    ) %>%
    
    addScaleBar("bottomleft")
  
  print(m)
  
  return(flowlines)
  
  
}





