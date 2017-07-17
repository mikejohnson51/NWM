#' Get HUC6 Flowlines, Catchment Boundaries, and HAND Products
#'
#'This function builds a list of needed file and downloads them from CyberGIS FTP server. Spatial boundaries are the HUC6 units and avialable data include NHDflowlines, 
#'catchment boundary, the HAND raster, and the catchment mask.
#'unit code.

#' @param 
#' HUC6 A six unit identifer for the HUC6 unit. Can be a single value or a string.
#' @param 
#' need.shp If TRUE then the shapefiles for the flowlines and HUC6 catchment boundary will be downloaded. 
#' @param 
#' need.hand.data If TRUE the HAND raster and catchment mask .tif files will be downloaded. This are large files and take a significant amount of time to download,
#' only do so if interested in applying the HAND method for innudation mapping. 
#' 
#' @examples 
#' get_HUC6_data(HUC6 = c("180600",180701), need.shp = TRUE, need.hand.data =TRUE)
#' 
#' @author 
#' Mike Johnson
#' 
#' @return 
#' This function downloads the associated shapefiles into the /Flowlines folder built using 
#' \code{\link{build_files}}
#' 
#' @seealso 
#' \code{\link{build_files}}
#' \code{\link{get_stage}}
#' 
#' @export

get_HUC6_data = function(HUC6 = "010100", need.shp = FALSE, need.hand.data = TRUE){
  
  
  
  hand = list() 
  shp = list()
  
  
  for(i in 1:length(HUC6)){
 
  hand[[i]] = c(                          paste0(HUC6[i], "hand.tif"),
                                          paste0(HUC6[i], "catchmask.tif"))
                                    
  
  shp[[i]]       = c(                      paste0(HUC6[i], "-flows.dbf"),
                                           paste0(HUC6[i], "-flows.prj"),
                                           paste0(HUC6[i], "-flows.shp"),
                                           paste0(HUC6[i], "-flows.shx"),
                                           paste0(HUC6[i], "-wbd.dbf"),
                                           paste0(HUC6[i], "-wbd.prj"),
                                           paste0(HUC6[i], "-wbd.shp"),
                                           paste0(HUC6[i], "-wbd.shx"))
                                    

  
  }
  
      if (need.hand.data == FALSE){hand = NULL} 
      if (need.shp == FALSE){shp = NULL} 


      build.file.list = c(Reduce(c,hand),Reduce(c,shp))
      
  if(is.null(build.file.list)) {print("No data requested")
    break
    }else{print("Data list built!")}

  for(i in 1:length(build.file.list)){
    URL = paste0("http://141.142.170.172/nfiedata/HUC6/",HUC6,"/", build.file.list[i])
    download.file(url = URL, destfile = paste0(getwd(),"/Flowlines/",build.file.list[i]))
    }
  
}
