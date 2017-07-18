#' Visualize Current Streamflow Forecasts
#' 
#' Automatically generates a proportional streamflow visulization and hydrogrpah at a point.
#' 
#' @details 
#' Each time the function is executed the 'Current' image \cr \cr
#'  
#' Each time the function is executed an additional subfolder will be created and numbered according to t
#' he number of times
#' the function has been executed for that type. \cr \cr
#' 
#' If all three types are executed twice the Images folder will have a subflder named after the csv file. In this folder will three subfolders: "hydrograph", "flows" and "combo".
#' In each of these will be two subfolders named ex: ('type'1), ('type"2).
#' 
#' @seealso 
#' \code{\link{build_csv}}
#' 
#' @seealso 
#' \code{\link{get_COMIDS}}
#' 
#' @seealso 
#' \code{\link{get_current}}
#'
#' @param 
#' COMIDs requires a single COMID. See \code{\link{get_COMIDS}}
#' @param 
#' catchment_path path name to NHDPlusV2 catchments shapefile (.shp)
#' @param 
#' flowlines_path path name to NHDPlusV2 flowlines shapefile (.shp)
#' @param 
#' region a string of the regions name
#' 
#' @return 
#' Images of the specified type to the "Images/" subfolder
#' @author 
#' Mike Johnson and Jim Coll
#' @examples 
#' 
#' Return hydrograph
#' 
#' viz_csv(type = "hydrograph", COMIDs = comids[1:25], csv ="OnionCreek_20170711_20170711.csv",
#' catchment_path = "/Users/mikejohnson/Desktop/Tester/Geospatial/Flowlines/Catchments/catchments.shp",
#' flowlines_path = "/Users/mikejohnson/Desktop/Tester/Geospatial/Flowlines/Flowlines.shp")
#' 
#' @examples 
#' viz_current = function(catchment_path, flowlines_path, COMID, region = "Santa Barbara")

#' @export

  viz_current = function(catchment = NULL, flowlines = NULL, catchment_path = NULL, flowline_path = NULL, COMID = NULL, comid_path = NULL, region){
    
    #setwd("/Users/mikejohnson/Desktop/Arizona/")
    #catchment = NULL; flowlines = 'Geospatial/Flowlines/ForFred/AZ_Deaths.shp'; catchment_path = NULL; flowline_path = NULL; COMID = NULL; comid_path = NULL; region= "Test"
    
    if(is.null(catchment)){
      catchments = catchment_path}
    
    if(is.null(catchment_path)){
      catchment = NULL
    }else{catchment = readOGR(catchment_path)}
    
    
    if(is.null(flowlines)){
      flowlines = flowline_path}
    
    if(is.null(flowlines)){
      print("A flowline shapefile object or path must be provided!")
      break} else {
        flowlines = readOGR(flowlines)
      }
    
  files = dir(paste0(getwd(),"/NetCDFs/Current"))
  file.remove(list.files(paste0(getwd(),"/Images/Current"), pattern = ".png", full.names = TRUE))
  
  data = read.csv(paste0(getwd(),"/Output/Current/", dir(paste0(getwd(),"/Output/Current"))))
  data = as.matrix(data)
  data = na.omit(data)

  subset = cbind(data[,2],data[,2:19])
  
  if(is.null(COMID)){
  index = which.max(data[,2:dim(data)[2]])
  COMID = as.numeric(data[index,1])
  }
  
  for(i in 3:19){ 
    
  png(paste0(getwd(),"/Images/Current/timestep", sprintf("%03d",i-1), ".png"), width = 3000, height = 1500, units= 'px', res = 300)
  
      par(mfrow= c(1,2))
      index = which(data[,1] == COMID)
    
    if(is.null(catchments)){
      
      plot(flowlines, col = 'grey50',lwd = ifelse(flowlines@data$streamorde >=3, (as.numeric(paste(flowlines@data$streamorde)))/4, 0),
           xlab = paste0(paste0("Forcast Type: ", substrRight(substr(files[1], 1, 20),11)),
                         paste0("\nGenerated On: ", Sys.Date()),
                         paste0("\nTime of forecast: ", substrRight(substr(files[1], 1,7),2)),
                         paste0("\nTime since forcast (hr): ", substrRight(substr(files[i-1], 1, 36), 3))))
      
      plot(flowlines, 
           col = ifelse((subset[,i]-subset[,i-1]) == 0,'darkgrey', ifelse((subset[,i]-subset[,i-1]) < 0,'red', 'blue'))
           ,lwd = .2*abs(scale((subset[,i]-subset[,i-1]+2), center = FALSE)), add = TRUE)
      
      
      plot(data[index,2:dim(data)[2]], type = "l", main = paste0("Streamflow at ", COMID), ylab = "Streamflow (cfs)", xlab= 'Time since forecast')
        points(i-1,data[index,i], cex = 1, pch =16, col = 'red') 
      
    }else{
    
    plot(catchments, border = 'black', col= 'grey80', main = paste0("Short Range Forecasts For \n ", region), 
         xlab = paste0(paste0("Forcast Type: ", substrRight(substr(files[1], 1, 20),11)),
                        paste0("\nGenerated On: ", Sys.Date()),
                        paste0("\nTime of forecast: ", substrRight(substr(files[1], 1,7),2)),
                        paste0("\nTime since forcast (hr): ", substrRight(substr(files[i-1], 1, 36), 3)))) 
    
    plot(flowlines, col = 'grey94',
         lwd = ifelse(flowlines@data$StreamOrde >=3, (as.numeric(paste(flowlines@data$StreamOrde)))/4, 0), add=TRUE)
    plot(flowlines, 
         col = ifelse((subset[,i]-subset[,i-1]) == 0,'darkgrey', ifelse((subset[,i]-subset[,i-1]) < 0,'red', 'blue')),
         lwd = .2*abs(scale((subset[,i]-subset[,i-1]+2), center = FALSE)), add = TRUE)
    
    
    plot(data[index,2:dim(data)[2]], type = "l", main = paste0("Streamflow at ", COMID), ylab = "Streamflow (cfs)", xlab= 'Time since forecast')
      points(i-1,data[index,i], cex = 1, pch =16, col = 'red') 
    }
    
    dev.off()
  
   }
}
