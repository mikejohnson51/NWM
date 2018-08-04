#' Get NWM channel output
#'
#' Download channel output from the HydroShare Thredds server for a list of input files.
#' Parallized version using \code{doParallel} and \code{foreach}.
#'
#' @param AOI a AOI to subset data (generated with AOI::getAOI())
#' @param filelist a list of filepaths (generated with getFiles)
#' @param param a channel paramater
#'
#' @return a matrix of values where columns are files and rows are COMIDs
#' @export

getChannel = function(AOI, filelist, param) {

  i = NULL
  j = NULL
  `%dopar%` <- foreach::`%dopar%`

  combine_lists <- function(LL1, LL2) {
    vals <- c(LL1$vals, LL2$vals)
    time <- c(LL1$time, LL2$time)
    return(list(vals = vals, time = time))
  }

  combine_files <- function(LL1, LL2) {
    vals <- cbind(LL1$vals, LL2$vals)
    time <- c(LL1$time, LL2$time)
    return(list(vals = vals, time = time))

  }

  idList <-
    comidList(AOI, 2)

  no_cores <- parallel::detectCores() - 1
  doParallel::registerDoParallel(no_cores)

  res <-
    foreach::foreach(i = 1:length(filelist) , .combine = combine_files) %dopar% {
      foreach::foreach(j = 1:length(idList),  .combine = combine_lists) %dopar% {
        file = paste0(filelist[i],
                      "?time[0:1:0],",
                      param,
                      "[",
                      min(idList[[j]]),
                      ":1:",
                      max(idList[[j]]),
                      "]")

        nc = ncdf4::nc_open(file)
        vals = ncdf4::ncvar_get(nc, param)
        time = ncdf4::ncvar_get(nc, "time")
        ncdf4::nc_close(nc)

        return(list(vals = vals, time = time))

      }
    }

  final <-
    trimChannel(idList = idList,
                data = res$vals,
                time = unique(res$time))

  return(final)

}
