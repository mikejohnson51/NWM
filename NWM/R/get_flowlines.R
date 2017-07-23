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
#' area.length this value defines the length of each side of the bounding box square that will be used to extract flowlines.
#'
#' @examples 
#' get_flowlines("Walmart in Tuscaloosa AL")
#' 
#' get_flowlines("34.42 -119.69")
#' 
#' get_flowlines("University of Oregon")
#' 
#' @author 
#' 
#' Mike Johnson
#' 
#' @export



get_flowlines = function(place = NULL, area.length = 10){

dir.create(paste0(getwd(),"/", gsub(" ", "", place)))
  setwd(paste0(getwd(),"/", gsub(" ", "", place)))
  
  build_files()

location = as.numeric(geocode(place))

df = (area.length/2)/69
dl = df / cos(location[2] * pi/180)

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

m = leaflet() %>%
  
  addRectangles(
    lng1 = west , lat1 = north,
    lng2 = east , lat2 = south,
    fillColor = "transparent",
    col
  ) %>%
  
  addMarkers(lng = location[1], lat = location[2]) %>%
  
  addProviderTiles(providers$CartoDB.Positron) %>%

  addPolylines(data = flowlines_84, color = 'blue', weight = flowlines_84$streamorde, 
               label = paste0(paste0(flowlines_84@data$gnis_name),
                              paste0(" COMID:", flowlines_84$comid)
                              ))
  
print(m)

return(flowlines)
    

}
 

