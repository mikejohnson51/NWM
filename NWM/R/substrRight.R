#' Substrings of a Character Vector from the Right

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))}

