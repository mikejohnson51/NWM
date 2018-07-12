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

  if (is.null(type)) {
    cat("You are viewing metadata for the",
        config,
        "configuration:\n\n")
    cat(data$meta$description, "\n\n")
    cat("Valid Types include:\n\t",
        paste(names(data), collapse = ", "),
        "\n\n")
    cat("Valid T include:\n\t",
        paste(substr(data$meta$tlist, 2, 3), collapse = ", "),
        "\n\n")
    cat("Valid F include:\n\t", paste(substr(data$meta$flist, 2, 4), collapse = ", "))
  } else {
    if (!(type %in% names(data))) {
      stop(paste(config, "not a valid configuraion. Use:\n"),
           paste(names(data), collapse = "\n"))
    }

    type_data = eval(parse(text = paste0("nwm$", config, "$", type)))
    cat("You are viewing metadata for the",
        config,
        "configuration and",
        type,
        "type:\n\n")
    cat(data$meta$description, "\n\n")
    cat("Valid", type,  "parameters include:\n\n")
    mat = as.matrix(type_data)
    prmatrix(mat, rowlab = rep("", nrow(mat)))
    cat("\n\n")
    cat("Valid T values include:")
    xx =  ceiling(sqrt(length(data$meta$tlist)))
    mat = suppressWarnings(matrix(as.numeric(substr(
      data$meta$tlist, 2, 3
    )), ncol = xx, byrow = F))
    prmatrix(mat,
             rowlab = rep("", nrow(mat)),
             collab = rep("", ncol(mat)))
    cat("\n")
    cat("Valid F values include:")
    xx =  ceiling(sqrt(length(data$meta$flist)))
    mat = suppressWarnings(matrix(as.numeric(substr(
      data$meta$flist, 2, 4
    )), ncol = xx, byrow = F))
    prmatrix(mat,
             rowlab = rep("", nrow(mat)),
             collab = rep("", ncol(mat)))
    cat("\n")
  }

}
