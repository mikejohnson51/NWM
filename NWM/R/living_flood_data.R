#' Getting Current Stage Values for Living Flood Workflow
#' 
#' This script is part 2/2 of the living flood workflow. Once set up with \code{\link{setup_livingflood()}}
#' users can run this script using the outputs from \code{\link{setup_livingflood()}}. This script can be set to
#' run hourly constantly providing modeled stage heights and discharge values for all input COMIDs 
#' 
#' @param 
#' region.name Region being modeled. Will be used to name output files 
#' @param 
#' comid_path A path to a list of COMIDs produced in \code{\link{setup_livingflood()}}. Alternatives can be generated using
#' \code{\link{get_COMIDs()}}
#' @param 
#' rating_curve_path A path to a list of rating curves produced in \code{\link{setup_livingflood()}}.  Alternatives can be generated using
#' \code{\link{get_rating_curves()}}
#' 
#' @author 
#' Mike Johnson
#' 
#' @export
#' 

living_flood_data = function(region.name, comid.path, rating_curve_path){
    
  region.name = "Austin"
  comid_path  = "/Users/mikejohnson/Desktop/AustinExample/Output/comids.csv"
  rating_curve_path = rating_curve_path
  
  
  comids        = read.csv(comid_path)
    comids      = comids[,2]
    
  ratingCurve   = read.csv(rating_curve_path)
    ratingCurve = ratingCurve[,2:4]
  
  get_current()

  Q = build_csv(folder = 'current', comids = comids, regions.name = region.name)

  get_stage(discharge_csv_path = Q,
          rating_curve_path  = rating_curve_path,
          units_discharge = "cfs",
          regions.name    = regions.name
            )
}