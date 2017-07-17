#' Visualize Streamflow Data
#' 
#' This function offers three methods for visualizing streamflows from a csv \code{\link{build_csv}}. 
#' Options that can be returned are hydrographs, porportional symbol flowlines, and a 
#' combination of hydrograph and proportional flowlines. \cr \cr
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
#' 
#' @param 
#' type What type of images should be returned? Options are: \cr \cr
#' (1) "hydrograph" : This will return a hydrograpgh for COMID(s) across the timeseries,
#'  up to 25 comids can be entered at a time. A single image will be produced.
#' 
#' (2) "flow" : This will return a plot of the flowlines with a line width porportional
#' to the flow (in cfs)/10. A folder of images, one for each time step, will be produced
#' 
#' (3) "combo" : This will return a multiplot with both a hydrograph for a specified COMID 
#' and the proportional width flowlines. A folder of images, one for each time step, will be produced.
#' 
#' @param 
#' COMIDs requires a list of COMIDs. For type = 'hydrograph' up to 25 COMIDs can be entered. 
#' For type = "flow" and type = "combo" only one COMID should be entered. See \code{\link{get_COMIDS}}
#' @param 
#' csv path to a csv where column one is a COMIDS and column 2:X are timeseries flow values
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
#' flowlines_path = "/Users/mikejohnson/Desktop/Tester/Geospatial/Flowlines/Flowlines/Flowlines.shp")
#' 
#' @examples 
#' Return proportionally symbolized flowlines
#' 
#' viz_csv(type = "flow", COMIDs = comids[56], csv ="OnionCreek_20170711_20170711.csv",
#' catchment_path = "/Users/mikejohnson/Desktop/Tester/Geospatia;Flowlines/Catchments/catchments.shp",
#' flowlines_path = "/Users/mikejohnson/Desktop/Tester/Geospatial/Flowlines/Flowlines.shp")
#' 
#' @examples 
#' Return combontion of proportionally symbolized flowlines an hydrograph
#' 
#' viz_csv(type = "combo", COMIDs = comids[56], csv ="OnionCreek_20170711_20170711.csv",
#' catchment_path = "/Users/mikejohnson/Desktop/Tester/Geospatial/Flowlines/Catchments/catchments.shp",
#' flowlines_path = "/Users/mikejohnson/Desktop/Tester/Geospatial/Flowlines/Flowlines.shp")

#' @export


viz_csv = function(type, COMIDs, csv_path, catchment_path, flowlines_path, region){
  
  catchments = readOGR(catchment_path)
  flowlines = readOGR(flowlines_path)

  data = read.csv(csv_path, header = TRUE, sep =",")

  data = as.matrix(data)
  name = gsub(" ","",region)
  
  export_path = paste0(getwd(),"/Images/", name)
  
  
  dir.create(export_path)
  
if(type == "hydrograph"){

  if(length(dir(paste0(export_path,"/", type, "s"))) == 0) {
    dir.create(paste0(export_path,"/", type, "s"))
    number = 1
  } else {
    number = length(paste0(export_path,"/", type, "s")) + 1
  }
  
    row = round(sqrt(length(COMIDs)),0)
    col = round(length(COMIDs)/row, 0)
  
          png(paste0(export_path,"/", type, "s/hydrograph", number,".png"), 
              width = col*1000, height = row*1000, units= 'px', res = 300)
          
          par(mfrow = c(row,col))
          
          for (i in 1:length(COMIDs)){
                index = which(data[,1]==COMIDs[i])
                plot(data[index,2:dim(data)[2]], type = "b", 
                main = paste0('COMIDs ', COMIDs[i]), ylab = "flow (cfs)", 
                xlab= 'Time since forecast', col = 'darkblue', lwd = 4, panel.first=grid()) 
                grid(col = "lightgray")
            }
            dev.off()
            
            
            
            
            
            
            
    
  } else if(type == "flow") { 
    
    if(length(dir(paste0(export_path,"/", type, "s"))) == 0) {
      number = 1
      path000 = paste0(export_path,"/", type, "s")
      dir.create(path000)
      path111 =  paste0(path000, "/", type, number)
      dir.create(path111)
      
    } else {
      number = length(dir(paste0(export_path,"/", type, "s"))) + 1
      path111 =  paste0(paste0(export_path,"/", type, "s"),"/flow", number)
      dir.create(path111)
      
    }
    
   
    
    for(i in 2:dim(data)[2]){ 
    
    png(paste0(path111,"/timestep",sprintf("%03d",i),".png"), 
        width = 3000, height = 1500, units= 'px', res = 300)
    
    plot(catchments, border = 'white', col= 'lightgrey', main = paste0("Flow Forecasts"), 
         ylim = c(catchments@bbox[2,1], catchments@bbox[2,2]),
         xlim = c(catchments@bbox[1,1], catchments@bbox[1,2]))
    plot(flowlines, col = 'blue', lwd = (data[,i]/10), add = TRUE)
    
    
    dev.off()
    
    } 
    
    dev.off()
    
    
    
    
  } else if(type == "combo") {
  
  if(length(dir(paste0(export_path,"/", type, "s"))) == 0) {
    number = 1
    path000 = paste0(export_path,"/", type, "s")
    dir.create(path000)
    path111 =  paste0(path000, "/",type, number)
    dir.create(path111)
    
  } else {
    number = length(dir(paste0(export_path,"/", type, "s"))) + 1
    path111 =  paste0(paste0(export_path,"/", type, "s"),"/", type, number)
    dir.create(path111)
    
  }
  
  for(i in 2:dim(data)[2]){ 
    
    png(paste0(path111,"/timestep",sprintf("%03d",i),".png"), width = 3000, height = 1500, units= 'px', res = 300)
    
    par(mfrow= c(1,2)); 
    
    index = which(data[,1] == COMIDs)
    
    plot(catchments, border = 'white', col= 'lightgrey', main = paste0("Flow Forecasts"), 
         ylim = c(catchments@bbox[2,1], catchments@bbox[2,2]),
         xlim = c(catchments@bbox[1,1], catchments@bbox[1,2]))
                        
    plot(flowlines,add = TRUE, col ="blue", lwd = (data[,i]/10))   
    
    plot(data[index,2:dim(data)[2]], type = "l", main = paste0("Streamflow at ", COMIDs), ylab = "Streamflow (cfs)", xlab= 'Time since forecast')
    points(i-1,data[index,i], cex = 1, pch =16, col = 'red')  
    
    dev.off()
  }

  }
}
  

    