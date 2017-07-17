#' Exporting Timeseries Data for NWM data for select COMIDs using the NWM package
#' 
#' This function subsets a folder of temporal .nc data to a spatial list of COMIDs and returns a .csv of time series by COMID.
#' If the user wants all temporal slices only the first three parameters are needed. Otherwise start.date, start.time, end.date, 
#' and end.time can be used to refine the data that is returned.
#' 
#' @param 
#' folder a path to a folder with netcdf files. If not yet obtained the #' Exporting Timeseries Data for NWM data for select COMIDs
#' 
#' This function subsets a folder of temporal .nc data to a spatial list of COMIDs and returns a .csv of time series by COMID.
#' If the user wants all temporal slices only the first three parameters are needed. Otherwise start.date, start.time, end.date, 
#' and end.time can be used to refine the data that is returned.
#' 
#' @param 
#' folder a path to a folder with netcdf files. If not yet obtainded consider: \code{\link{get_server}} 
#' @param
#' start.date in format 'YYYY-MM-DD'. If left empty the first timeseries entry will default first day in the folder.
#' @param 
#' start.time in format 'HH" If left empty the first time series entry will default to the first file in the folder.
#' @param
#' end.date in format 'YYYY-MM-DD' If left empty the first timeseries entry will default last day in the folder.
#' @param 
#' end.time in format 'HH' If left empty the first time series entry will default to the last file in the folder.
#' @param 
#' COMID(s) of interest can be a single number or a list.
#' If not yet determined consider \code{\link{get_COMIDs}} 
#' @param 
#' regions.name The name of the region, this will be used to name the output files
#' @return 
#' A .csv of flow records for the given time period for each given COMID. This file will be stored in the Output subfolder and will be named according to the dates and region provided
#' @author 
#' Mike Johnson and Jim Coll
#' 
#' @examples 
#' 
#' Example 1) Return all time slices in a folder: \cr
#' 
#' build_csv(folder = "F:/johnson/NWM_DATA/NetCDFs/Current",  regions.name = "Onion Creek", comids = comids)
#' 
#' Example 2) Return a subset of time slices in a folder \cr 
#' 
#' build_csv(folder = "F:/johnson/NWM_DATA/complete2014_retro", \cr
#' start.date = "2014-01-01", start.time = "00", \cr
#' end.date = "2014-10-31",end.time= "23",  \cr
#' regions.name = "Onion Creek", comids = comids.onion.creek)
#' @export 
#' 
#' @param
#' start.date in format 'YYYY-MM-DD'. If left empty the first timeseries entry will default first day in the folder.
#' @param 
#' start.time in format 'HH" If left empty the first time series entry will default to the first file in the folder.
#' @param
#' end.date in format 'YYYY-MM-DD' If left empty the first timeseries entry will default last day in the folder.
#' @param 
#' end.time in format 'HH' If left empty the first time series entry will default to the last file in the folder.
#' @param 
#' COMID(s) of interest can be a single number or a list.
#' If not yet determined consider \code{\link{get_COMIDs}} 
#' @param 
#' regions.name The name of the region, this will be used to name the output files
#' @return 
#' A .csv of flow records for the given time period for each given COMID. This file will be stored in the Output subfolder and will be named according to the dates and region provided
#' @author 
#' Mike Johnson and Jim Coll
#' @examples 
#' 
#' Example 1) Return all time slices in a folder: \cr
#' 
#' build_csv(folder = "F:/johnson/NWM_DATA/NetCDFs/Current",  regions.name = "Onion Creek", comids = comids)
#' 
#' Example 2) Return a subset of time slices in a folder \cr 
#' 
#' build_csv(folder = "F:/johnson/NWM_DATA/complete2014_retro", \cr
#' start.date = "2014-01-01", start.time = "00", \cr
#' end.date = "2014-10-31",end.time= "23",  \cr
#' regions.name = "Onion Creek", comids = comids.onion.creek)
#' @export

build_csv = function(folder = 'current', start.date = NULL, start.time = NULL, end.date = NULL, end.time = NULL, comids, regions.name = "OnionCreek"){
  
  # Getting indexes -----------------------------------------------------
  #folder = 'current'; start.date = NULL;start.time = NULL; end.date = NULL; end.time = NULL; 
  #folder = "current"; comids = HUC6_flows; regions.name = "paste0(HUC6)"
  
  if (folder == 'current'){
    folder = paste0(getwd(), "/NetCDFs/Current")
    folder2 = 'current'
  }else{
    folder = folder
    folder2 = "BugsBunny"
  }
  
  files = dir(folder)
  
  if (is.null(start.date)){
    start.index = 1
    start.date = gsub("-","", Sys.Date())
  }else{
    start.date = gsub("-", "", start.date)
    fileOne = paste0(start.date)
    start.index = grep(fileOne, files)[1]
  }
  
  if (is.null(end.date)){
    end.index = length(files)
    end.date = start.date
  }else{
    end.date = gsub("-", "", end.date)
    end.index = grep(end.date, files)
    end.index = end.index[length(end.index)]
  }
  
  if(is.null(start.time)){
    start.time = NULL
  }else{
    start.time = paste0("t",start.time)
  }
  
  files = files[start.index:end.index]
  
  
  nc <- nc_open(filename = paste0(folder,"/", files[[1]]))
    vars <- nc$var
    
    test1test2 = vars$streamflow$dim[[1]]$vals
    comids_of_value = comids[comids %in% test1test2]
    
    start <- vector(mode = "numeric", length(comids_of_value))
    
    for(i in 1:length(comids_of_value)){
      start[i] = which(test1test2 == comids_of_value[i])}
    
    
    nwm.flow = matrix(NA, ncol = length(files), nrow = length(start))
  nc_close(nc)
  
  for (i in 1:length(files)) {
        nc = nc_open(filename = paste0(folder,"/", files[i]))
        values = ncvar_get(nc, varid = "streamflow")
    for(j in 1:length(start)){
        nwm.flow[j,i] = values[start[j]]
  
    } 
    nc_close(nc)
  }
  
  rownames(nwm.flow) = comids_of_value
  colnames(nwm.flow) = substr(files, 1, 12)
  
  nwm.flow = nwm.flow * 35.3147
  
  regions.name = gsub(" ", "", regions.name)
  
  if(start.date == end.date){end.date = NULL}
  
  if(folder2 ==  'current'){
    write.csv(nwm.flow, paste0(getwd(),"/Output/Current/",regions.name,"_",start.date,"_", end.date, ".csv"))
  }else{
    write.csv(nwm.flow, paste0(getwd(),"/Output/",regions.name,"_",start.date,"_", end.date, ".csv"))
  }
  
    
  }
  





