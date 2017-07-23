#' Find USGS Gages and Respective COMIDs
#' 
#' This function can be used to indintify USGS gages within the study area as defined by the input NHD flow lines and returns a table of 
#' matching USGSstation, NHD comids, and their respective latitude longitude values.

find_usgs = function(flowlines_path = NULL ){

  #flowlines_path = '/Users/mikejohnson/Documents/R_NWM_tests/UCSB/Geospatial/Flowlines/nhdflowline_network.shp'
  
    flowlines = readOGR(flowlines_path)
     flowlines_84 = spTransform(flowlines, CRS("+proj=longlat +datum=WGS84"))
     
     
        usgs1 = subset(usgs_gages, usgs_gages$lat_reachCent < flowlines_84@bbox[2,2])
          usgs2 = subset(usgs1, usgs1$lat_reachCent > flowlines_84@bbox[2,1])
          usgs3 = subset(usgs2, usgs2$lon_reachCent > flowlines_84@bbox[1,1])
        usgs4 = subset(usgs3, usgs3$lon_reachCent < flowlines_84@bbox[1,2])
        
    
    print("=====================================================")
    print("=====================================================")
    print(paste0("There are ", dim(usgs4)[1], " USGS stations in this area!"))
    print("=====================================================")
    print("=====================================================")
    
    
    usgs_final = cbind(usgs4$feature_id, usgs4$site_no, usgs4$lat_reachCent, usgs4$lon_reachCent)
      colnames(usgs_final) = c("USGSgage", "COMID", "lat", "long")
      
      usgs_final = as.matrix(usgs_final)
    
    coors = cbind(usgs_final[,4], usgs_final[,3])
    sp = SpatialPoints(coors, proj4string = CRS("+proj=longlat +datum=WGS84"))
    
    
    
    m = leaflet() %>%
      
      addProviderTiles(providers$CartoDB.Positron) %>%
      
      addMarkers(data = usgs_final,lng= usgs_final[,4], lat = usgs_final[,3],
                 popup = paste(sep ="<br/>" , "USGS Gage Number: ", usgs_final[,1],
                          "COMID: ", usgs_final[,2]
                          ))%>%
  
       addPolylines(data = flowlines_84, color = 'blue', weight = flowlines_84$streamorde)%>%
    addScaleBar("bottomleft")
    
    print(m)
    return(usgs_final)
  
      
}
