#' Get HUC6 Flowlines, Catchment Boundaries, and HAND Products
#'
#'This function builds a list of needed file and downloads them from CyberGIS FTP server. Spatial boundaries are the HUC6 units and avialable data include NHDflowlines, 
#'catchment boundary, the HAND raster, and the catchment mask.
#'
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
#' This function downloads the associated shapefiles into the /Geospatial/Flowlines folder built using 
#' \code{\link{build_files}}
#' 
#' @seealso 
#' \code{\link{build_files}}
#' \code{\link{get_rating_curve}}
#' 
#' @export

get_HUC6_data = function(HUC6 = "010100", need.shp = FALSE, need.hand.data = TRUE){
  
  hand = list() 
  catchment = list()
  shp = list()
  
  
  for(i in 1:length(HUC6)){
 
  hand[[i]] = c(    paste0(HUC6[i],"/", HUC6[i], "hand.tif"),
                    paste0(HUC6[i],"/",HUC6[i], "catchmask.tif"))
                                    
  
  shp[[i]]  = c(    paste0(HUC6[i],"/",HUC6[i], "-flows.dbf"),
                    paste0(HUC6[i],"/",HUC6[i], "-flows.prj"),
                    paste0(HUC6[i],"/",HUC6[i], "-flows.shp"),
                    paste0(HUC6[i],"/",HUC6[i], "-flows.shx"),
                    paste0(HUC6[i],"/",HUC6[i], "-wbd.dbf"),
                    paste0(HUC6[i],"/",HUC6[i], "-wbd.prj"),
                    paste0(HUC6[i],"/",HUC6[i], "-wbd.shp"),
                    paste0(HUC6[i],"/",HUC6[i], "-wbd.shx"))
                                    
  }
  
      if (need.hand.data == FALSE){hand = NULL}
      if (need.hand.data == FALSE){catchment = NULL}
      if (need.shp == FALSE){shp = NULL} 


  for(i in 1:length(hand)){
    URL = paste0("http://141.142.170.172/nfiedata/HUC6/", hand[i])
    download.file(url = URL, destfile = paste0(getwd(),"/Geospatial/HAND/",substr(hand[i],8,nchar(hand[i]))))
  }
  
  for(i in 1:length(catchment)){
    URL = paste0("http://141.142.170.172/nfiedata/HUC6/", catchment[i])
    download.file(url = URL, destfile = paste0(getwd(),"/Geospatial/Catchments/",substr(catchment[i],8,nchar(catchment[i]))))
  }
  
  for(i in 1:length(shp)){
    URL = paste0("http://141.142.170.172/nfiedata/HUC6/", shp[i])
    download.file(url = URL, destfile = paste0(getwd(),"/Geospatial/Flowlines/",substr(shp[i],8,nchar(shp[i]))))
  }
  
}


