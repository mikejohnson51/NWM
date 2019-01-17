#' National Water Model Client
#'
#' \code{AOI} package
#'
#' See the \href{https://github.com/mikejohnson51/nwm/blob/master/README.md}{README} on github
#'
#' @docType package
#'
#' @name nwm
#' @import      AOI
#' @importFrom  crayon blue underline
#' @importFrom  RNetCDF open.nc close.nc var.get.nc
#' @importFrom  foreach %do% %dopar% foreach
#' @importFrom  doParallel registerDoParallel
#' @importFrom  httr write_disk POST
#' @importFrom  sf st_zm read_sf
#' @importFrom  stats reshape
#' @importFrom  raster raster projection extent stack plot
#' @importFrom  sp spTransform
#' @importFrom  utils unzip

NULL


