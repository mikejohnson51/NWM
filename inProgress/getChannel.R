#' Get NWM channel output
#'
#' Download channel output from the HydroShare Thredds server for a list of input files.
#' Parallized version of \code{get_channel_data} using \code{doParallel} and \code{foreach}.
#'
#' @param AOI a AOI to subset data (generated with AOI::getAOI())
#' @param filelist a list of filepaths (generated with getFiles)
#' @param param a channel paramater
#'
#' @return a matrix of values where columns are files and rows are COMIDs
#' @export

getChannel = function(AOI, filelist, param) {

  i = NULL
  `%dopar%` <- foreach::`%dopar%`

  idList = comidList(AOI, 2)

  no_cores <- parallel::detectCores() - 1
  doParallel::registerDoParallel(no_cores)

  data <- foreach::foreach(i = 1:length(filelist), .combine = "cbind") %dopar% get_channel_data(filepath = filelist[i], index = idList, param = param)

  time <- foreach::foreach(i = 1:length(filelist), .combine = 'c') %dopar% getTime(filelist[i])

  final = trimChannel(idList = idList, data = data, time = time)

  return(final)

}

i= 1
