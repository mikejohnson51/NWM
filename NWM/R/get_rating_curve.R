#' Get Hand Rating Curves for COMIDs of Interest
#' @export



get_rating_curves = function(HUC_units, comids){

for(i in 1:length(HUC_units)){
    URL = paste0("http://141.142.170.172/nfiedata/HUC6/",HUC_units[i],"/", "hydroprop-fulltable-",HUC_units[i], ".csv")
    download.file(url = URL, destfile = paste0(getwd(),"/Raw_RatingCurves/rating-curve-table",HUC_units[i], ".csv"))
  }


rating_curves = list()

 for (i in 1:length(dir(paste0(getwd(),"/Raw_RatingCurves")))){
   
 
   build =  read.csv(paste0(getwd(),"/Raw_RatingCurves/", dir(paste0(getwd(),"/Raw_RatingCurves"))[i]))
   
   build = cbind(build[,1], build[,15], build[,2])
   
   build = build[comids %in% build[,1],]
   
   rating_curves[[i]] = build
 }

all_rating_curves = do.call(rbind, rating_curves)
colnames(all_rating_curves) = c("COMIDs", "Discharge (cms)", "Stage")

write.csv(all_rating_curves, file = paste0(getwd(), "/Output/", "rating_curves.csv"))
   
}





