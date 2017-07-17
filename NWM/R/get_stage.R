#' Convert Discharge to Stage via HAND synthetic rating curves
#' 
#' This function converts a .csv of discharge data, which can be found via \code{\link{build_csv}} to stage
#' using the sythetic rating curves assoicated with the HAND methodology. These rating curves can be generated via
#' the \code{\link{get_rating_curve}}.
#' 
#' @param
#' disharge_csv_path The path to a .csv file containing disharge data built from \code{\link{get_current}},
#' \code{\link{get_server}}, and \code{\link{build_csv}}.
#' 
#' @param 
#' rating_curve_path The path to a rating curve .csv generated from \code{\link{get_rating_curve}}
#' 
#' @param 
#' units_discharge  The units of the discharge values in discharge_csv_path
#' 
#' @param 
#' regions.name The name of the region being studied. Used for nameing the output file.
#' 
#' @export
#' 
#' @author 
#' 
#' Mike Johnson
#'     

get_stage = function(discharge_csv_path, rating_curve_path, units_discharge = 'cms', regions.name){

  #discharge_csv_path = '/Users/mikejohnson/Desktop/AustinExample/Output/Current/Austin_20170717_.csv'
  #rating_curve_path = '/Users/mikejohnson/Desktop/AustinExample/Output/rating_curves.csv'
  #units_discharge = 'cfs'

  rating = read.csv(rating_curve_path)
    rating = as.matrix(cbind(rating$COMIDs, rating$Discharge..cms., rating$Stage))
  
  discharge = as.matrix(read.csv(discharge_csv_path))
    discharge = na.omit(discharge)  
    
  if(units_discharge == 'cms'){
    discharge = cbind(discharge[,1], discharge[,2:dim(discharge)[2]]) 
  }else{
    discharge = cbind(discharge[,1], discharge[,2:dim(discharge)[2]] * 0.028316847)
  }

  comids_final = discharge[,1][discharge[,1] %in% as.numeric(unique(rating[,1]))]
 
  
stage_data = matrix(0, nrow = length(comids_final), ncol = dim(discharge)[2])
  stage_data[,1] = comids_final
 
    for(i in 1:dim(stage_data)[1]){
   
        COMcurve = subset(rating, rating[,1] == stage_data[i,1])

      for (j in 2:dim(stage_data)[2]){
        
        index = which.min(abs(COMcurve[,2]-discharge[i,j]))
        stage_data[i,j] = COMcurve[index,3]
        
      }
    }
  
write.csv(stage_data, file = paste0(getwd(),"/Output/", region.name, "stage_data_cms.csv"))

}