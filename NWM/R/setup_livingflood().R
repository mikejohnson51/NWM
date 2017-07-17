#' Setting Up Data for Living Flood Workflow
#' 
#' Step one for running 'LivingFlood'. This function downloads and structures the data needed from a supplied 
#' flowline shapefile. Data is  stored in the appropriate folders as setup with \code{\link{build_files}}. Set up only needs to be run once 
#' for each community and the approximete time needed is ~ 15 for the City the size of Austin, Texas. 
#' 
#' @param 
#' flow_lines.shp a path to a flowlines shapefile downloaded and stored in the /Flowlines folder
#' generated with the \code{\link{build_files()}}. 
#' 
#' @param 
#' hand.raster.needed default is set to TRUE. If a user already has the necessary hand rasters then avoid long
#' download times by setting to FALSE.



setup_livingflood = function(flowlines_path, hand.raster.needed = TRUE){
  
  
  flow_lines.shp = readOGR(flowlines_path)
  print("shapefile built correctly!")    
  
  HUC_units6  = as.numeric(na.omit(unique(substr(flow_lines.shp$reachcode,1,6))))
  print(paste0(length(HUC_units6),' HUC units found!' ))
  print()
  
  
  if(hand.raster.needed == TRUE){
    get_HUC6_data(HUC_units6, need.shp = FALSE, need.hand.data = TRUE)
    print('HAND datasets downloaded to /Flowlines/ folder')
    print()
  }
  
  comids = as.numeric(levels(flow_lines.shp$comid))
  print(paste0(length(comids)," COMIDs found!" ))
  print()
  
  write.csv(comids, file = paste0(getwd(), "/Output/", 'comids.csv'))
  print('COMIDs writen to comids.csv in the /Output folder.' )
  print()
  
  get_rating_curves(HUC_units6, comids)
  print(paste0("Rating Curves for " , length(comids), "  writen to rating_curves.csv in the /Output folder."))
  print()
  print()
  print()
  print()
  print("Success! All components collected successfully.")
  print()
  
}
