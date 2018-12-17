#' Break COMIDs into sequential lists
#'
#' @param AOI an AOI object
#' @param num the number of desired lists (default 2)
#'
#' @return a list of vectors
#' @export
#' @author Mike Johnson

comidList = function(AOI = NULL, nhd = NULL, comids = NULL, num){

  i = NULL
  `%do%` <- foreach::`%do%`

  if(!is.null(comids)){
    fin = comids
  } else if(!is.null(nhd)) {
    fin = nhd$comid
  } else {
    fin = findNHD(AOI, streamorder = 1)
    fin = fin$comid
  }

  index = which(nwm::comids_all %in% fin)

  index = index[!is.na(index)]

  series <- foreach::foreach(i = 1:250, .combine = "c") %do% length(getBreaks(index, i))

  if(!(num %in% series)) { num = min(series)}

  xx = getBreaks(index, gap = which(series == num)[1])

  return(xx)
}


