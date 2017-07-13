#' Getting Historic NWM data from HydroShare Server
#'
#'Accesses  HydroShare server to download data for a time period (analysis_assim) or day (all other forecast_types). 
#'If that day is not avaiable error messages help identify days with data.
#'Parameters are avaliable to filter what data is downloaded. These include 'forecast type', 'data type', and time of forecast and latency. 
#'If left blank all files for the day will be downloaded.
#'
#' @param 
#' start.date given as "YYYY-MM-DD" 
#' @param 
#' forecast_type default is "short_term" but can be chaged to "medium_range", "long_range", "analysis_assim" \cr
#' analysis_assim data is avaliable from '2016-05-28' forward, all others have ~2 weeks latency
#' @param 
#' end.date this is ONLY applicable for forecast_type = 'analysis_assim'. Must be given in 'YYYY-MM-DD' format
#' @param 
#' type this is the type of data to be returned. Default is set to 'channel' other options include 'land', 'terrain', or 'reservoir'
#' @param 
#' time give as "HH" this is the time the forecast is made, default set to 'ALL'
#' @param 
#' forecast_hour given as "HHH", this is the number of hours from the time the forecast is made (time), defualt is set to "ALL"
#' If using anal
#' @examples 
#' To get the inital (forecast_hour = "001") analysis and assimulation data for all hours between  Oct 10, 2016 and Oct 14, 2016
#' the following code would be used: \cr \cr
#' 
#' get_server = function(start.date = "2016-10-10", forecast_type = "analysis_assim", 
#' end.date = "2016-10-14", type = "channel", time= NULL, forecast_hour = "001")
#' 
#' @author 
#' Mike Johnson and Jim Coll
#' 
#' @export
#' 
#' @return 
#' This function returns all .nc files to the NetCDFs subfolder for given inputs. 
#'


get_server = function(start.date, end.date = NULL, forecast_type = "short_range", 
             type = NULL, time= NULL, forecast_hour = NULL){

# starting function -------------------------------------------------------

  start.date = "2016-10-10"; forecast_type = "analysis_assim"; end.date = "2016-10-14"; type = NULL; time= NULL; forecast_hour = NULL

  if(forecast_type != "analysis_assim" &  !is.null(end.date) ){
   print("End date is only applicable for forecast type = 'analysis_assim'. Please leave as NULL for other forecast types.")
    break}
 
  if(is.null(time)){
  time = "nwm"
}else{
  time = paste0("t", time)
}

if(is.null(forecast_hour)){forecast_hour = "nwm"
}else{
  forecast_hour = paste0("f", forecast_hour)
}

if(is.null(type)){type = "channel"
}else{
  type = type
}

filelist_hydro = paste0(getwd(), "/Output/", forecast_type,"_", type, "_", gsub("-","", start.date),"_t",".json")

if(is.null(end.date) ){
  end.date = NULL
} else{
  end.date = paste0("&endDate=", end.date)
}

getFilelist = paste0("https://apps.hydroshare.org/apps/nwm-data-explorer/api/GetFileList/?config=", forecast_type,"&startDate=",start.date, end.date)

download.file(url = getFilelist, destfile = filelist_hydro)

files = fromJSON(file=filelist_hydro)

  if (forecast_type != "analysis_assim" & start.date < "2017-06-00"){
    print("Short, medium and long range products not avaialable prior to 2017-06-00, please select a date after this.")
    break
    
  }else if (start.date < "2016-05-28" ){
    print("Analysis and assimulation data not available prior to 2016-05-28, please select a date after this.")  
    break
    
  }else if (start.date > Sys.Date()){
    print("This day hasn't happend yet :(")
    break
  
  } else if(length(files) > 3 ) {
    
    print("Success!")

    
  } else {
    
    repeat{
      
      test = start.date
      
      getFilelist = paste0("https://apps.hydroshare.org/apps/nwm-data-explorer/api/GetFileList/?config=", forecast_type,"&startDate=",test)
      
      download.file(url = getFilelist, destfile = filelist_hydro)
      
      files = fromJSON(file=filelist_hydro)
      
      if (length(files) < 3) {
        
        start.date = as.Date(start.date) + 1}
      
      else {print(paste0("The first date with data still archived is ", test, ". Please Try Again.")) 
        break}
  }
  } 


# Subseting  --------------------------------------------------------------

subset_files = grep(type, files, value = TRUE)
subset_files = grep(time, subset_files, value = TRUE)
subset_files = grep(forecast_hour, subset_files, value = TRUE)


# Get Data ----------------------------------------------------------------

if(time == 'nwm'){time = "tall"
}else{
  time = time
}

if(forecast_hour == 'nwm'){forecast_hour = NULL
}else{
  forecast_hour = forecast_hour
}

folder = paste0(getwd(),"/NetCDFs/", forecast_type,"_", type, "_", gsub("-","", start.date), time, forecast_hour)
dir.create(folder)

for (i in 1:length(subset_files)){
  
  URL = paste0("https://apps.hydroshare.org/apps/nwm-data-explorer/api/GetFile?file=", subset_files[i])
  download.file(url = URL, mode = "wb", destfile = paste0(folder, "/", subset_files[i]))
  
}

}
  
