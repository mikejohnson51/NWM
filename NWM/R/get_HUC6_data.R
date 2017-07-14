#' Get HUC6 Flowlines and Catchment Boundaries
#'
#'This function hits the CyberGIS FTP server to pull the flowline and catchment shapefiles for a HUC6 unit specified by 
#'unit code.

#' @param 
#' HUC6 This is the six unit identifer for the HUC6 unit. 
#'
#' @examples 
#' get_HUC6_data(HUC6 = "010100")
#' 
#' @author 
#' Mike Johnson and Jim Coll
#' 
#' @return 
#' This function downloads the associated shapefiles into the /Flowlines folder built using 
#' \code{\link{build_files}}
#' 
#' @seealso 
#' \code{\link{build_files}}
#' 
#' @export

get_HUC6_data = function(HUC6 = "010100"){
  build.file.list = c(paste0(HUC6, "-flows.dbf"),
                      paste0(HUC6, "-flows.prj"),
                      paste0(HUC6, "-flows.shp"),
                      paste0(HUC6, "-flows.shx"),
                      paste0(HUC6, "-wbd.dbf"),
                      paste0(HUC6, "-wbd.prj"),
                      paste0(HUC6, "-wbd.shp"),
                      paste0(HUC6, "-wbd.shx"))

  for(i in 1:length(build.file.list)){
    URL = paste0("http://141.142.170.172/nfiedata/HUC6/",HUC6,"/", build.file.list[i])
    download.file(url = URL, destfile = paste0(getwd(),"/Flowlines/",build.file.list[i]))
  }
  
  }