#' Trim data to key COMIDs
#'
#' @param idList list of ides
#' @param data flow data to be trime
#' @param time a vector of time inputs
#' @param param string used to name time varying column
#'
#' @return a wide data.frame
#' @export
#' @author Mike Johnson


trimChannel = function(idList = NULL, data = NULL, param = NULL, time = NULL,  f = NULL){

  i = NULL

  if(class(idList) != 'list') { idList = list(idList)}

  `%dopar%` <- foreach::`%dopar%`

  no_cores <- parallel::detectCores() - 1
  doParallel::registerDoParallel(no_cores)

  comids.full = foreach::foreach(i = 1:length(idList), .combine = 'c') %dopar%  nwm::comids_all[min(idList[[i]]):max(idList[[i]])]

  fin = cbind(comids.full, data*35.3147)

  if(NROW(fin) > 1 ){
    fin = fin[which(comids.full %in% nwm::comids_all[do.call("c", idList)]),]
  }

  fin = as.data.frame(fin)

  names(fin) = c('COMIDS', paste0("result.", 1:(ncol(fin)-1)))

  tst = reshape(fin,
                direction = 'long',
                varying = c(2:ncol(fin)),
                idvar = 'COMIDS', new.row.names = NULL)

  tst = tst[order(tst$COMIDS),]

  row.names(tst) <- NULL

  #tst$t = foreach::foreach(i = 1:nrow(tst), .combine = 'c') %dopar% t[tst$time[i]]
  tst$start = foreach::foreach(i = 1:nrow(tst), .combine = 'c') %dopar% f[tst$time[i]]
  tst$time = foreach::foreach(i = 1:nrow(tst), .combine = 'c') %dopar% time[tst$time[i]]


  tst$time = as.POSIXct('1970-01-01 00:00:00', tz = 'GMT') + tst$time*60

  tst$start = tst$time - (tst$start)*60*60

  tst = tst[order(tst$COMIDS, tst$time, tst$start),]

  names(tst) <- c("COMIDS", "DateTime", param, 'start')

  return(tst)
}

