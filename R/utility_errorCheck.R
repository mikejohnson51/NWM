#' Basic Error Checking
#'
#' @param error the type of error to check
#' @param param the parameter being requested
#' @param config the configuration of interest
#' @param type the output type of interest
#' @param t the time of forecast
#' @param f the time since t
#' @param date the date of interest
#'
#' @return a message
#' @export

error.check = function(error = NULL, param = NULL, config = NULL, type = NULL, t = NULL, f = NULL, date = NULL){

message = NULL
data = eval(parse(text = paste0("nwm$", config)))

# 1. Error check `date` ------------------------------------------------------

if(error == 'date'){

true.date = as.POSIXlt(format(Sys.Date(), tz = "GMT", usetz = TRUE))
max.date = as.POSIXlt(format(Sys.Date() - 40, tz = "GMT", usetz = TRUE))
request.dates = as.POSIXlt(paste(date), format = "%Y%m%d", tz = "GMT")

if (any(request.dates > true.date)) {
  message = paste0("Resquested date(s) have not happend...")
}

if (any(request.dates < max.date)) {
  message = paste0("Data only archived from ", max.date, " on ...")
}

}

# 2. Error check `parameter` -------------------------------------------------

if(error == "parameter"){

  if(grepl("forcing", config)){config = gsub("forcing_", "", config)}
  good.param = eval(parse(text = paste0("nwm$", config, "$", type, "$PARAM")))

  bad.param  = param[!(param %in% good.param)]

  if(length(bad.param) > 0) {

    message = paste(
      toupper(bad.param),
      "not a valid",
      type,
      "parameter."
    )
  }
}


# 3. Error check `config` ----------------------------------------------------

if(error == "configuration"){

  if (!(config %in% names(nwm))) {

    message = paste(
      toupper(config),
      "not a valid configuration, select from:\n\n",
      paste(names(nwm), collapse = "\n ")
    )
  }
}

# 4. Error check `F` ---------------------------------------------------------

if(error == "f"){

  if (all(!is.element(paste0("f", sprintf("%03d", f)), data$meta$flist))){

    bad.f = which(!is.element(paste0("f", sprintf("%03d", f)), data$meta$tlist))

    message = paste(
      f[bad.f],
      "not a valid f value for",
      config,
      "configuration.",
      "Use: \n\n",
      paste(as.numeric(substr(data$meta$flist, 2, 4)), collapse = ", "),
      paste0("\n\nIf having trouble, run look('", config, "')")
    )
  }
}

# 5. Error check `T` ---------------------------------------------------------

if(error == "t"){

  if (all(!is.element(paste0("t", sprintf("%02d", t)), data$meta$tlist))){

    bad.t = which(!is.element(paste0("t", sprintf("%02d", t)), data$meta$tlist))

    message = paste(
      t[bad.t],
      "not a valid t value for",
      config,
      "configuration.",
      "Use: \n\n",
      paste(as.numeric(substr(data$meta$tlist, 2, 3)), collapse = ", "),
      paste0("\n\nIf having trouble, run look('", config, "')")
    )
  }
}

# 6. Error check `type` ------------------------------------------------------

if(error == "type"){

  if (!(type %in% names(data))) {

    message = paste(
      toupper(type),
      "is not a valid output type, select from:\n\n",
      "channel\n",
      "land\n",
      "forcing",
      paste0("\n\nIf having trouble, run look('", config, "')")
    )
  }
}

# Return message ----------------------------------------------------------

return(message)

}


