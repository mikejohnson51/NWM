#' View NWM Model Metadata
#'
#' @param config \code{character} model configuration
#' @param type  \code{character}  model type
#'
#' @return prints a description to the console
#' @export
#'
#' @examples
#' \dontrun{
#' look("short_range")
#' look("short_range", "channel")
#' }
#'
#' @author Mike Johnson


look = function(config, type = NULL) {
  if (!(config %in% names(nwm))) {
    stop(paste(config, "not a valid configuraion. Use:\n"),
         paste(names(nwm), collapse = "\n"))
  }

  data = eval(parse(text = paste0("nwm$", config)))

  x = ceiling(sqrt(length(data$meta$flist)))
  f.data = as.numeric(substr(data$meta$flist, 2, 4))
  length(f.data) <- x*(ceiling(length(data$meta$flist)/x))
  f.mat = matrix(f.data, nrow = x, byrow = T)
  #f.mat[is.na(f.mat)]   <- " "

  if(length(data$meta$tlist) > 5){
  x = ceiling(sqrt(length(data$meta$tlist)))
  t.data = as.numeric(substr(data$meta$tlist, 2, 3))
  length(t.data) <- x*x
  t.mat = matrix(t.data, nrow = x, byrow = T)
  #t.mat[is.na(t.mat)]   <- ""
  } else {
    t.mat = as.numeric(substr(data$meta$tlist, 2, 3)) }

  if (is.null(type)) {
    cat("You are viewing metadata for the",
        paste0("`",config, "`"),
        "configuration:\n\n")
    cat(data$meta$DESCRIPTION, "\n\n")
    cat("Valid Types include:\n",
        paste(names(data), collapse = ", "),
        "\n\n")

    cat("Valid `t` values include:\n\t")

    if(length(data$meta$tlist) > 5){
      prmatrix(t.mat, rowlab = rep("", nrow(t.mat)), collab = rep("", ncol(t.mat)))
    } else {cat(paste(t.mat, collapse = ", "))}
    cat("\n\n")

    cat("Valid `f` values include:\n\t")

    prmatrix(f.mat, rowlab = rep("", nrow(f.mat)), collab = rep("", ncol(f.mat)))
    cat("\n")


  } else {
    if (!(type %in% names(data))) {
      stop(paste(config, "not a valid configuraion. Use:\n"),
           paste(names(data), collapse = "\n"))
    }

    type_data = eval(parse(text = paste0("nwm$", config, "$", type)))
    cat("You are viewing metadata for the",
        paste0("`", config ,"`"),
        "configuration and",
        paste0("`",type, "`"),
        "type:\n\n")

    cat(data$meta$DESCRIPTION, "\n\n")

    cat("Valid", paste0("`",type, "`"),  "parameters include:\n\n")
    mat = as.matrix(type_data, byrow = T)
    prmatrix(mat, rowlab = rep("", nrow(mat)))
    cat("\n")

    cat("Valid `t` values include:\n\t")

    if(length(data$meta$tlist) > 5){
    prmatrix(t.mat, rowlab = rep("", nrow(t.mat)), collab = rep("", ncol(t.mat)))
    } else {cat(paste(t.mat, collapse = ", "))}
    cat("\n\n")

    cat("Valid `f` values include:\n\t")
    prmatrix(f.mat, rowlab = rep("", nrow(f.mat)), collab = rep("", ncol(f.mat)))
    cat("\n")
  }

}
