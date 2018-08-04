#' Download National Water Model Data
#'
#' @param AOI an Area of Interest to subset to (generated with getAOI)
#' @param filelist a filelist generated with getFilelist
#' @param param a parameter to get
#'
#' @return a matrix of point values or a stack of gridded raster images
#' @export
#'
#' @author Mike Johnson

downloadNWM = function(AOI = NULL,
                       filelist = NULL,
                       param = NULL) {

  if(!(class(AOI) %in% c("list","HydroData"))){AOI = list(AOI = AOI)}

  tmp = filelist[1]

  config <-
    regmatches(tmp, regexec("nwm/(.*?)/", tmp))[[1]][2]

  types  = c("channel", "land", "forcing")

  type = types[sapply(types, grepl, tmp)]

  param.error = error.check(
    error = "parameter",
    param = param,
    config = config,
    type = type
  )

  if (!is.null(param.error)) {
    stop(param.error)
  }

  if (type == 'channel') {
    vals = getChannel(AOI = AOI$AOI,
                      filelist = filelist,
                      param = param)
  }

  if (type %in% c('land', "forcing")) {
    vals = getGridded(AOI = AOI$AOI,
                      filelist = filelist,
                      param = param)
  }

  AOI[[param]] = vals

  return(AOI)

}
