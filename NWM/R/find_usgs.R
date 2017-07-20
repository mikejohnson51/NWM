#' Find USGS Gages and Respective COMIDs
#' 
#' This function can be used to indintify USGS gages within the study area as defined by the input NHD flow lines and returns a table of 
#' matching USGSstation, NHD comids, and their respective latitude longitude values.
find_usgs = function(flowlines_path = NULL){
  
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
    
    coors = cbind(usgs_final[,4], usgs_final[,3])
    sp = SpatialPoints(coors, proj4string = CRS("+proj=longlat +datum=WGS84"))
    
    
    plot(flowlines_84,main = ("Local USGS Stations"),
         xlab = paste0(paste0(dim(usgs_final)[1], " local USGS stations"),
                       paste0(" across ", dim(flowlines_84)[1], " flowlines"),
                       paste0("\n Density of ",round(((dim(usgs_final)[1]/dim(flowlines_84)[1])*100),2), "%." )),
         col = colorRampPalette(c("grey20", "darkblue"))(5)[flowlines_84@data$streamorde],
         lwd = ifelse(flowlines_84@data$streamorde >= 3, (as.numeric(paste(flowlines_84@data$streamorde)))/2, .3))
      plot(sp, col = 'grey20', bg ='orange', pch = 25, cex = .9, add = TRUE)
    
    
    return(usgs_final)
      
}



