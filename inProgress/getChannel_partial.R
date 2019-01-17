#' Get Channel Data for a set of indeces from a file path
#'
#' Download channel output from the HydroShare Thredds server for a vector of sequential COMID indeces (eg c(1:100)).
#'
#' @param filepath filename (generated from getFiles)
#' @param index    vector of COMID indeces (generated from comidList and getBreaks)
#' @param param    parameter of interest
#'
#' @return a vector of numeric values
#' @export
#' @author Mike Johnson
#' @keywords internal

get_channel_partial = function(filepath, index, param) {
  file = paste0(filepath,
                "?time[0:1:0],",
                param,
                "[",
                min(index),
                ":1:",
                max(index),
                "]")

  vals = RNetCDF::var.get.nc(RNetCDF::open.nc(file), param)

  return(vals)

}


getTime = function(filepath){

  file = paste0(filepath, "?time[0:1:0]")
  time = RNetCDF::var.get.nc(RNetCDF::open.nc(file), "time")

  time = as.POSIXct('1970-01-01 00:00:00', tz = 'GMT') + time*60

  return(time)
}

