#' Download National Water Model Data
#'
#' @param AOI an Area of Interest to subset to (generated with getAOI)
#' @param filelist a filelist generated with getFilelist
#' @param param a parameter to get
#'
#' @return a matrix of values
#' @export
#'
#' @author Mike Johnson

downloadNWM = function(AOI = NULL, filelist = NULL, param = NULL){

config <- regmatches(filelist[1],regexec("nwm/(.*?)/",filelist[1]))[[1]][2]

if(grepl("channel", filelist[1])) {type = 'channel'}
if(grepl("land", filelist[1]))    {type = 'land'}
if(grepl("forcing", filelist[1])) {type = 'forcing'}

param.error = error.check(error = "parameter", param = param, config = config, type = type)
if(!is.null(param.error)){stop(param.error)}

if(type == 'channel'){
  vals = getChannel(AOI = AOI, filelist = filelist, param = param)
}

if(type %in% c('land', "forcing")){
  vals = getGrid_data(AOI = AOI, filelist = filelist, param = param)
}

return(vals)

}
