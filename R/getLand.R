#' Get NWM land output
#'
#' Download land output from the HydroShare Thredds server for a list of input files.
#' Parallized version of \code{get_land_data} using \code{doParallel} and \code{foreach}.
#'
#' @param AOI a AOI to subset data (generated with AOI::getAOI())
#' @param filelist a list of filepaths (generated with getFiles)
#' @param param a channel paramater
#'
#' @return a matrix of values where columns are files and rows are COMIDs
#' @export

getGrid_data = function(AOI, filelist, param) {

  i = NULL
  `%dopar%` <- foreach::`%dopar%`

  grid = define_AOI_grid(AOI)

  no_cores <- parallel::detectCores() - 1
  doParallel::registerDoParallel(no_cores)

  stack <- foreach::foreach( i = 1:length(filelist)) %dopar% get_gridded_data(AOI = AOI,
                                                                                      grid = grid,
                                                                                      filepath = filelist[i],
                                                                                      param = param)

  r = do.call(raster::stack, stack)

  return(r)

}

foreach::`%dopar%`
