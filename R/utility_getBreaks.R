#' Get Breaks
#'
#' Split a vector into a list of vecotrs with sequential values
#'
#' @param index provide a vector of values
#' @param gap what is the maximum gap allowed between sequential values
#'
#' @return a list of vecotrs
#' @noRd
#' @keywords internal
#' @author Mike Johnson

getBreaks = function(index, gap = 50) {
  x <- index[order(index)]

  b <- c(0, which(diff(x) > gap), length(x))

  xx = sapply(seq(length(b) - 1),
              function(i)
                x[(b[i] + 1):b[i + 1]])
  return(xx)
}
