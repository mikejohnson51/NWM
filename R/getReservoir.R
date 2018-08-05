#' Get NWM reservior output
#'
#' Download reservoir output from the HydroShare Thredds server for a list of input files.
#' Parallized version using \code{doParallel} and \code{foreach}.
#'
#' @param AOI a AOI to subset data (generated with AOI::getAOI())
#' @param filelist a list of filepaths (generated with getFiles)
#' @param param a reservoir paramater
#'
#' @return a matrix of values where columns are files and rows are COMIDs
#' @export

getReservoir = function(AOI, filelist, param) {

i = NULL
`%dopar%` <- foreach::`%dopar%`

no_cores <- parallel::detectCores() - 1
doParallel::registerDoParallel(no_cores)

tmp <- foreach::foreach(i  = 1:length(filelist), .combine = 'rbind') %dopar% {
  file = paste0(filelist[i],
                "?time[0:1:0],longitude[0:1:1505],latitude[0:1:1505],feature_id[0:1:1505],",
                param,
                "[0:1:1505]")

  nc = ncdf4::nc_open(file)

  all.res  = data.frame(
  ID = ncdf4::ncvar_get(nc, "feature_id"),
  lat = ncdf4::ncvar_get(nc, "latitude"),
  lng = ncdf4::ncvar_get(nc, "longitude"),
  time = as.POSIXct('1970-01-01 00:00:00', tz = 'GMT') + ncdf4::ncvar_get(nc, "time")*60,
  val = ncdf4::ncvar_get(nc, "inflow")
  )

  ncdf4::nc_close(nc)

  all.res = all.res[all.res$lat > AOI@bbox[2,1], ]
  all.res = all.res[all.res$lat < AOI@bbox[2,2], ]
  all.res = all.res[all.res$lng > AOI@bbox[1,1], ]
  all.res = all.res[all.res$lng < AOI@bbox[1,2], ]

  return(all.res)
}

tmp = tmp[order(tmp$ID),]
colnames(tmp) <- c("ID", "lat", "lng", "dateTime", param)
rownames(tmp) <- NULL

return(tmp)
}

