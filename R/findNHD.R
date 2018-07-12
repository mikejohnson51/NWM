#' Find NHD lines
#'
#' @param AOI an Area of Interest
#' @param spatial spatial return object as `sp` (default is `sf`)
#'
#' @return a \code{SpatialPolylines} object
#' @export
#' @author Mike Johnson


findNHD = function(AOI, spatial = TRUE) {

  sl = query_cida(AOI = AOI, type = 'nhdflowline_network', spatial= spatial)

  return(sl)

  }

