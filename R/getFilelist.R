#' Get file list
#' @description get a list of paths to files containing the requested data
#'
#' @param config model configuration
#' @param type output type
#' @param date dates of interst
#' @param t time of forecast
#' @param f hours foward from t
#' @param useHTTP use HTTP? default = FALSE
#'
#' @return a list of file paths
#' @export
#' @author Mike Johnson

getFilelist = function(config = "medium_range",
                    type = "channel",
                    date = 20180615,
                    t = 0,
                    f = 3,
                    useHTTP = FALSE) {
  #dates.utc = as.POSIXlt(paste(date, t), format = "%Y%m%d %H", tz = "GMT")
  #dates.local = format(dates.utc, tz=Sys.timezone(),usetz=TRUE)

  #if(is.null(eval(parse(text = paste0("nwm$", config))))){stop("Config not right !")}
  #if(is.null(eval(parse(text = paste0("nwm$", config, "$", type))))){stop("Type not right !")}

# Gerneral Error Checking -------------------------------------------------

 date.error = error.check(error = "date", config = config, type = type, t = t, f = f, date = date)
 if(!is.null(date.error)){ stop (date.error)}

 config.error = error.check(error = "configuration", config = config, type = type, t = t, f = f, date = date)
 if(!is.null(config.error)){ stop (config.error)}

 type.error = error.check(error = "type", config = config, type = type, t = t, f = f, date = date)
 if(!is.null(type.error)){ stop (type.error)}

 t.error = error.check(error = "t", config = config, type = type, t = t, f = f, date = date)
 if(!is.null(t.error)){ stop (t.error)}

 f.error = error.check(error = "f", config = config, type = type, t = t, f = f, date = date)
 if(!is.null(f.error)){ stop (f.error)}

# Check if forcing is requested -------------------------------------------

 if (type == 'forcing') { config = paste(type, config, sep = "_") }

# Make sure requested data has been posted to THREDDS ---------------------

 server.base = "http://thredds.hydroshare.org/thredds/"

  if (gsub("-", "", Sys.Date()) %in% date) {
    url = paste(
      server.base,
      "catalog/nwm",
      config,
      gsub("-", "", Sys.Date()),
      sep = "/"
    )

    raw = try(readLines(paste(url, "catalog.html", sep = "/"), n = 30))
    paths = raw[grep('.nc</tt>' , raw)]
    current.t = unique(gsub(".*nwm.t\\s*|z..*", "", paths))

    if(any(t > as.numeric(current.t))) {

     message = paste0(
          "Archived data only avaiable up to: \nDate: ",
          Sys.Date(),
          "\nUTC hour: ",
          current.t)

     stop(message)

    }
  }

# Use HTTP or OpenDaP -----------------------------------------------------

 base = paste0(server.base, ifelse(isTRUE(useHTTP), "fileServer", "dodsC"), "/nwm/")

# Build file paths --------------------------------------------------------

  all.files = paste0(
    base,
    config,
    "/",
    date,
    "/",
    paste(
      "nwm",
      paste0("t", sprintf("%02d", t), "z"),
      ifelse(type == "forcing", sub('.*forcing_', '', config), config),
      ifelse(type == "channel", paste0(type, "_rt"), type),
      paste0("f", sprintf("%03d", f)),
      "conus.nc",
      sep = "."
    )
  )

# Return file paths -------------------------------------------------------

  return(all.files)

}

