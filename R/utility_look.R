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


look = function(config = NULL, type = NULL) {

  if(is.null(config)){

    message('The National Water Model comes in three configurations:\n"', paste0(names(nwm), collapse = '", "'), '":')
    message("The short_range configuration provides data 18 hours out at an hourly timestep")
    message("The medium_range configuration provides data 10 days out at an 3 hour timestep")
    message("The long_range configuration provides data 30 days hours out at a 6 hour timestep")

  } else {

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

  } else {
    t.mat = as.numeric(substr(data$meta$tlist, 2, 3)) }

  if (is.null(type)) {
    cat(crayon::blue$underline("You are viewing metadata for the",
        paste0("`",config, "`"),
        "configuration:\n"))
    cat(data$meta$DESCRIPTION, "\n\n")

    cat(crayon::blue$underline("Valid Types include:\n"))
    cat(paste(names(data)[1:length(names(data)) -1], collapse = ", "), "\n\n")

    cat(crayon::blue$underline("Valid `t` values include:\n"))

    cat(paste(as.numeric(substr(data$meta$tlist, 2, 3)), collapse = ", "))
    cat("\n\n")

    cat(crayon::blue$underline("Valid `f` values include:\n"))

    cat(paste(as.numeric(substr(data$meta$flist, 2, 4)), collapse = ", "))
    cat("\n")


  } else {
    if (!(type %in% names(data))) {
      stop(paste(config, "not a valid configuraion. Use:\n"),
           paste(names(data), collapse = "\n"))
    }

    type_data = eval(parse(text = paste0("nwm$", config, "$", type)))
    cat(crayon::blue$underline("You are viewing metadata for the",
        paste0("`", config ,"`"),
        "configuration and",
        paste0("`",type, "`"),
        "type:\n"))

    cat(data$meta$DESCRIPTION, "\n\n")

    cat(crayon::blue$underline("Valid", paste0("`",type, "`"),  "parameters include:\n"))
    mat = as.matrix(type_data, byrow = T)
    prmatrix(mat, rowlab = rep("", nrow(mat)))

    cat(crayon::blue$underline("Valid `t` values include:\n"))

    cat(paste(as.numeric(substr(data$meta$tlist, 2, 3)), collapse = ", "))
    cat("\n\n")

    cat(crayon::blue$underline("Valid `f` values include:\n"))

    cat(paste(as.numeric(substr(data$meta$flist, 2, 4)), collapse = ", "))
    cat("\n")
  }

  }
}
