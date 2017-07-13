#' Getting Most Current NWM data
#'
#'This function accesses the NOAA Nomads server to download the most up to date short term forcast NetCDF files. 
#'These files cover the spatial domain of the continential United States. These files will be stored in the 'NetCDFs/Current' folder
#'created using the 'build_files()' function in the NWM package. Each time this function is run the old files will be removed.
#'Therefore if you see something interesting be sure to save the data elsewhere!
#'
#' @param None 
#' @examples 
#' get_current()
#' @author 
#' Mike Johnson and Jim Coll
#' @export
#' @return 
#' This fuction saves the 18 NetCDF files related to the most recent short term forecast to the NetCDFs/Current folder.



get_current= function() {
  
  
  # Get most recent file list --------------------------------------------------------------
  
   Directory = paste0(getwd(), "/")
   
  
  file.remove(list.files(paste0(Directory, "NetCDFs/Current"), pattern = ".nc", full.names = TRUE))
  file.remove(list.files(paste0(Directory, "Output/Current"), pattern = ".csv", full.names = TRUE))
  
  date = format(Sys.Date(), tz = "GMT")
  time = strptime(format(Sys.time(), tz = "GMT"), format = "%Y-%m-%d %H:%M:%S")$hour
  
  # Where to save your file list:
  filelist = paste0(Directory, 'Output/fileList.json') 
  
  repeat{ 
    getFilelist = paste0("https://apps.hydroshare.org/apps/nwm-data-explorer/api/GetFileList/?config=short_range&startDate=",date,"&time=", time,
                         "&geom=channel")
    download.file(url = getFilelist, destfile = filelist)
    
    
    files = fromJSON(file=filelist)
    
    if (length(files) >= 3 ) {break}
    else {time = (time - 1)}
  }  
  
  
  time = substr(substrRight(files[1], 40),1,2)
  files = substrRight(files, 45)
  
  # Extract Data ------------------------------------------------------------
  
  #Where to save NetCDF files
  netCDFfiles = paste0(Directory,"NetCDFs/Current/")
  
  for (i in 1:length(files)){
    URL = paste0("http://nomads.ncep.noaa.gov/pub/data/nccf/com/nwm/prod/nwm.",gsub("-", "", date),"/short_range/", files[i])
    name = paste0( gsub("-", "", date), substrRight(substr(files[i],1,57),3) , "_streamflow.nc") 
    download.file(url = URL, mode = "wb", destfile = paste0(netCDFfiles, files[i]))
  }
  
  return(time)
  
}



