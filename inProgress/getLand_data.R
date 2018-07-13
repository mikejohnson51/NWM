#' Get Land Data from a file path
#'
#' Download land output from the HydroShare Thredds server from a single file.
#'
#' @param AOI an AOI to subset (generated from AOI::getAOI)
#' @param grid the AOI grid (generated from getGrid_land)
#' @param filepath filename (generated from getFiles)
#' @param param parameter of interest
#'
#' @return a vector of numeric values
#' @export
#' @author Mike Johnson
#' @keywords internal
#'

get_gridded_data = function(AOI, grid, filepath = NULL, param = NULL){

  param = toupper(param)

  file = paste0(filepath,
                "?time[0:1:0],",
                param,
                "[0:1:0][",
                grid$y.min,
                ":1:",
                grid$y.max ,
                "][",
                grid$x.min,
                ":1:",
                grid$x.max,
                "]"
  )

  nc = ncdf4::nc_open(file)
  vals = ncdf4::ncvar_get(nc, param)
  time = ncdf4::ncvar_get(nc, "time")

  mat <- apply(t(vals),2,rev)
  r <-raster::raster(mat)
  raster::projection(r) = sp::CRS("+proj=lcc +lat_1=30 +lat_2=60 +lat_0=40 +lon_0=-97 +x_0=0 +y_0=0 +a=6370000 +b=6370000 +units=m +no_defs")
  raster::extent(r) = c(grid$long.min, grid$long.max, grid$lat.min, grid$lat.max)
  raster::res(r) = 1000

  r = raster::projectRaster(from = r, crs = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

  return(r)
}
