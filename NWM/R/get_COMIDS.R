#' Get COMIDs from a Shapefile
#'
#'Takes a shapefile already process in R using readOGR() or a path to a shapefile folder and converts it into a list of COMIDs. 
#'This list can be used in the \code{\link{build_csv}} function. \cr \cr
#'
#'Either a path to a local .shp file OR a shapefile object is required. \cr \cr
#'
#'For validation this function will plot the shapefile to ensure it is correct.
#'
#' @param 
#' shapefile takes a shapefile object of COMIDs. This must be preprossed in R using readOGR() or other package 
#' @param 
#' path takes a path to a .shp

#' @examples 
#' comid_OC = get_COMIDs(shapefiles = flowlines)
#' 
#' comid_OC = get_COMIDs (path ="F:/johnson/NWM_DATA/Flowlines/nhdflowlines_OC.shp")
#' 
#' @author 
#' Mike Johnson and Jim Coll
#' 
#' @export
#' 
#' @return 
#' This function returns a list of COMIDs from a shapefile of flowlines.

get_COMIDs = function(shapefile = NULL, path = NULL){
 
  if(is.null(shapefile)){
  
    flowlines = readOGR(path)
    comids = vector(mode = "numeric",length(flowlines@data$OBJECTID))
    comids = flowlines@data$COMID
    comids = as.numeric(levels(comids))[comids]
    plot(flowlines, col = 'blue', main= "Your Area of Interest")
  
  } else {
  
    comids = vector(mode = "numeric",length(shapefile@data$OBJECTID))
    comids = shapefile@data$COMID
    comids = as.numeric(levels(comids))[comids]
    plot(shapefile, col = 'blue', main= "Your Area of Interest")
  }
  
  return(comids)
  
}
