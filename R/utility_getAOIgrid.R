#' Get NWM grid indeces for AOI
#'
#' Ingests AOI object and returns grid indeces and values for the NWM native grid
#'
#' @param AOI an AOI to transform to grid
#'
#' @return a list of min/max lon/lat values and grid indeces
#' @export
#' @author Mike Johnson

define_AOI_grid = function(AOI){

  AOI = sp::spTransform(AOI, "+proj=lcc +lat_1=30 +lat_2=60 +lat_0=40 +lon_0=-97 +x_0=0 +y_0=0 +a=6370000 +b=6370000 +units=m +no_defs")

  lat  = seq(-1919500, 1919500, 1000)
  long = seq(-2303501, 2303499, 1000)

  x.min = which.min(abs(long - AOI@bbox[1,1]))
  x.max = which.min(abs(long - AOI@bbox[1,2]))
  y.min  = which.min(abs(lat  - AOI@bbox[2,1]))
  y.max  = which.min(abs(lat  - AOI@bbox[2,2]))

  long.min = long[x.min]
  long.max = long[x.max + 1]
  lat.min = lat[y.min]
  lat.max = lat[y.max + 1]

  return(list(y.min = y.min,
              y.max = y.max,
              x.min  = x.min,
              x.max = x.max,

              long.min = long.min,
              long.max = long.max,
              lat.min  = lat.min,
              lat.max  = lat.max
          ))
}
