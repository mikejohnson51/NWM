#' Download National Water Model Data
#'
#' @param AOI an Area of Interest to subset to (generated with getAOI)
#' @param filelist a filelist generated with getFilelist
#' @param param a parameter to get
#' @param layer for soil and snow land parameters a layer must be declared (defaults to 1)
#'
#' @return a matrix of point values or a stack of gridded raster images
#' @export
#'
#' @author Mike Johnson

downloadNWM = function(AOI = NULL,
                       nhd = NULL,
                       comids = NULL,
                       filelist = NULL,
                       param = NULL,
                       layer = NULL) {

  if(!(class(AOI) %in% c("list","HydroData"))){AOI = list(AOI = AOI)}

  config <- regmatches(filelist[1], regexec("nwm/(.*?)/", filelist[1]))[[1]][2]

  types  = c("channel", "land", "forcing", "terrain", "reservoir")

  type = types[sapply(types, grepl, filelist[1])]

  param.error = error.check(
    error = "parameter",
    param = param,
    config = config,
    type = type
  )

  if (!is.null(param.error)) {
    stop(param.error)
  }

  if (type %in% c('channel', 'reservoir')) {
    AOI = getNWM_point(AOI = AOI, nhd = nhd, comids = comids, type = type, filelist = filelist, param = param)
  }

  if (type %in% c('land', "forcing", "terrain")) {

    for(i in seq_along(param)){

      for(j in 1:max(1, length(layer))){

      l = defineLayers(param[i], layer[j])

      vals = getGridded(AOI = AOI$AOI,
                      filelist = filelist,
                      param = param[i],
                      layer = layer[j])

      AOI[[paste0(param[i], l$layer)]] = vals
      }
    }
  }

  return(AOI)

}


