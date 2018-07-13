#' Get Time for Gridded output from downloadNWM
#'
#' @param r a raster stack
#'
#' @return a vector of POSITct values
#' @export


getGridTime = function(r){

  times = substring(names(r), 3)

  final = as.POSIXct('1970-01-01 00:00:00', tz = 'GMT') + as.numeric(times)*60

  return(final)


}
