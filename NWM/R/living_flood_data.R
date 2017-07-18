#' Getting Current Stage Values for Living Flood Workflow
#' 
#' This script is part 2/2 of the living flood workflow. Once set up with \code{\link{setup_livingflood()}}
#' users can run this script using the outputs from \code{\link{setup_livingflood()}}. This script can be set to
#' run hourly constantly providing modeled stage heights and discharge values for all input COMIDs 
#' 
#' @param 
#' area_of_interest Region being modeled. Will be used to name output files 
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

living_flood_data = function(area_of_interest, comid_path = 'Output/comids.csv', rating_curve_path = "Output/rating_curves.csv", flowlines_path){

  comids        = read.csv(comid_path)
    comids      = comids[,2]
    
    print('COMIDs read in successfully!')
    print("----------------------------")
    print("----------------------------")
    print("----------------------------")
    print("----------------------------")
    print("----------------------------")
    
  ratingCurve   = read.csv(rating_curve_path)
    ratingCurve = ratingCurve[,2:4]
    
    print('Rating Curves read in successfully!')
    print("----------------------------")
    print("----------------------------")
    print("----------------------------")
    print("----------------------------")
    print("----------------------------")
  
  get_current()
  
  print('Most Current SR forecast read in successfully!')
  print("----------------------------")
  print("----------------------------")
  print("----------------------------")
  print("----------------------------")
  print("----------------------------")

  Q = build_csv(folder = 'current', comids = comids, regions.name = area_of_interest)

  print('Discharge Data built successfully!')
  print("----------------------------")
  print("----------------------------")
  print("----------------------------")
  print("----------------------------")
  print("----------------------------")
  
  get_stage(discharge_csv_path = Q,
          rating_curve_path  = rating_curve_path,
          units_discharge = "cfs",
          regions.name    = area_of_interest)
  
  print('Stage Data built successfully!')
  print("----------------------------")
  print("----------------------------")
  print("----------------------------")
  print("----------------------------")
  print("----------------------------")
  
  print('You now have a .csv for discharge and stage for this forecast hour. Move into ArcMap to map!')
  print("----------------------------")
  print("----------------------------")
  print("----------------------------")
  print("----------------------------")
  print("----------------------------")
  
 discharge = as.matrix(read.csv(Q))
 index = which.max(discharge[,2:dim(discharge)[2]])
 COMID = as.numeric(discharge[index,1])
  
  viz_current(region = area_of_interest, COMID = COMID, flowlines_path = flowlines_path)
 
}