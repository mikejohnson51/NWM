#' Vizualize Channel Flows
#'
#' @param AOI an AOI object (for background plotting) (optional if AOI is not NULL)
#' @param nhd an NHD set generated with getNHD (optional if AOI is not NULL)
#' @param data flow data data.frame
#' @param num number of reaches to differentiate (default = 5)
#' @param max if TRUE the high flows are plotted, if FALSE then low flows
#'
#' @export


vizFlows = function(AOI = NULL, nhd = NULL, data = NULL, num = 5, max = TRUE){

  if(all(is.null(AOI), is.null(nhd))){stop("Must provide either AOI or NHD file...")}
  if(is.null(nhd)){ nhd = getNHD(AOI)}

  flows = data[order(data$cfs, decreasing = ifelse(max, TRUE, FALSE)),]

  if(!is.null(AOI)){ sp::plot(AOI, col = 'gray') }

  sp::plot(nhd,
           col = ifelse(nhd$comid %in% unique(flows$COMIDS)[1:num], ifelse(max, "navy", "red"), "blue"),
           lwd = ifelse(nhd$comid %in% unique(flows$COMIDS)[1:num], 10, 3), add = ifelse(!is.null(AOI), TRUE, FALSE),
           main =paste(num, ifelse(max, "high", "low"), "flow reaches"))


}

