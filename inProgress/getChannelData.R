#' Get Channel Data from a file path
#'
#' Download channel output from the HydroShare Thredds server for a list of sequential COMID indeces.
#' Parallized version of \code{get_channel_partial} using \code{doParallel} and \code{foreach}.
#'
#' @param filepath filename (generated from getFiles)
#' @param index    list of COMID indeces (generated from comidList and getBreaks)
#' @param param    parameter of interest
#'
#' @return a vector of numeric values
#' @export
#' @author Mike Johnson
#' @keywords internal

get_channel_data = function(filepath, index, param) {

  i = NULL
  `%dopar%` <- foreach::`%dopar%`

  no_cores <- parallel::detectCores() - 1
  doParallel::registerDoParallel(no_cores)

  tmp <-
    foreach::foreach(i = 1:length(index), .combine = 'c') %dopar% get_channel_partial(filepath = filepath, index = index[[i]], param = param)

  message(paste("Finished", filepath))

  return(tmp)
}




