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

getChannel = function(AOI = NULL,
                      nhd = NULL,
                      comids = NULL,
                      filelist, param) {

  i = NULL
  j = NULL
  `%dopar%` <- foreach::`%dopar%`

  combine_lists <- function(LL1, LL2, LL3) {
    vals <- c(LL1$vals, LL2$vals)
    time <- c(LL1$time, LL2$time)
    f <- c(LL1$f, LL2$f)
    return(list(vals = vals, time = time, f = f))
  }

  combine_files <- function(LL1, LL2) {
    vals <- cbind(LL1$vals, LL2$vals)
    time <- c(LL1$time, LL2$time)
    f <- c(LL1$f, LL2$f)
    return(list(vals = vals, time = time, f = f))

  }

  idList <- comidList(AOI = AOI, nhd = nhd, comids = comids,  2)

  no_cores <- parallel::detectCores() - 1
  doParallel::registerDoParallel(no_cores)

  res <-
    foreach::foreach(i = 1:length(filelist) , .combine = combine_files) %dopar% {
      foreach::foreach(j = 1:length(idList),  .combine = combine_lists) %dopar% {
        try({
          file = paste0(filelist[i],
                      "?time[0:1:0],",
                      param,
                      "[",
                      min(idList[[j]]),
                      ":1:",
                      max(idList[[j]]),
                      "]")

        nc   = RNetCDF::open.nc(file)
        vals = RNetCDF::var.get.nc(nc, param)
        time = RNetCDF::var.get.nc(nc, 'time')
        RNetCDF::close.nc(nc)

        })

        f = as.numeric(regmatches(filelist[i], regexec('f(.*?)\\.conus', filelist[i]))[[1]][2])

        return(list(vals = vals, time = time, f = f))

      }
    }

  subs = seq(1, length(res$time), length(idList))

  final <- trimChannel(idList = idList,
                data = res$vals,
                time = res$time[subs],
                f = res$f[subs],
                param = param)

  return(final)

}

