#' Visualize Current Streamflow Forecasts
#' 
#' Automatically generates a labeled visulization of hydrogrpahy at a point and porportionalized flow records.
#' 
#' @details 
#' Each time the function is executed a subfolder mirroring the csv files name will be generated in the "Images" subfolder. 
#' An addtion subfolder within this folder will be created named after the 'type' entered. \cr \cr
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
#' @seealso 
#' \code{\link{get_current}}
#' 
#' @param 
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
#' catchment_path = "/Users/mikejohnson/Desktop/Tester/Flowlines/Catchments/catchments.shp",
#' flowlines_path = "/Users/mikejohnson/Desktop/Tester/Flowlines/Flowlines/Flowlines.shp")
#' 
#' @examples 
#' viz_current = function(catchment_path, flowlines_path, COMID, region = "Santa Barbara")

#' @export

  viz_current = function(catchment_path, flowlines_path, COMID, region){
    
  files = dir(paste0(getwd(),"/NetCDFs/Current"))
  file.remove(list.files(paste0(getwd(),"/Images/Current"), pattern = ".png", full.names = TRUE))
  
  data = read.csv(paste0(getwd(),"/Output/Current/", dir(paste0(getwd(),"/Output/Current"))))
  data = as.matrix(data)

  
  catchments = readOGR(catchment_path)
  flowlines = readOGR(flowlines_path)
  
  for(i in 1:18){ 
    
    png(paste0(getwd(),"/Images/Current/timestep", sprintf("%03d",i), ".png"), width = 3000, height = 1500, units= 'px', res = 300)
    
    par(mfrow= c(1,2)); 
    
    index = which(data[,1] == COMID)
    
    plot(catchments, border = 'white', col= 'lightgrey', main = paste0("Short Range Forecasts For \n ", region), 
         ylim = c(catchments@bbox[2,1]+.05,catchments@bbox[2,2]),
         xlab = paste0(paste0("Forcast Type: ", substrRight(substr(files[1], 1, 20),11)),
                        paste0("\nDate: ", Sys.Date()),
                        paste0("\nTime of forecast: ", substrRight(substr(files[1], 1,7),2)),
                        paste0("\nTime since forcast (hr): ", substrRight(substr(files[i], 1, 36), 3)))) 
    
    
    plot(flowlines,add = TRUE, col ="blue", lwd = (data[,i+1]/10))      
    
    plot(data[index,2:dim(data)[2]], type = "l", main = paste0("Streamflow at ", COMID), ylab = "Streamflow (cfs)", xlab= 'Time since forecast')
    points(i,data[index,i+1], cex = 1, pch =16, col = 'red')  
    
    dev.off()
  }
  }
    
    
    