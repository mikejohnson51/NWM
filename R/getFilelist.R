#' Get file list
#' @description get a list of paths to files containing the requested data
#'
#' @param config model configuration
#' @param type output type
#' @param date dates of interst
#' @param t time of forecast
#' @param f hours foward from t
#' @param n if f is null n can be used to limit the number of files returned
#' @param useHTTP use HTTP? default = FALSE
#'
#' @return a list of file paths
#' @export
#' @author Mike Johnson


getFilelist = function(config = "medium_range",
                    type = "channel",
                    date = NULL,
                    t = NULL,
                    f = NULL,
                    n = 5,
                    useHTTP = FALSE) {


  server.base = "http://thredds.hydroshare.org/thredds"

  errors = error.check(
    error = "configuration",
    config = config,
    type = type,
    t = t,
    f = f,
    date = date
  )

  if (!is.null(errors)) { stop (errors) }


  if(any(is.null(date), date == gsub("-", "", Sys.Date()))){

    date = gsub("-", "", Sys.Date())

     url = paste(
       server.base,
       "catalog/nwm",
       config,
       date,
       sep = "/"
     )

     raw = try(readLines(paste(url, "catalog.html", sep = "/"), n = 30))
     paths = raw[grep('.nc</tt>' , raw)]
     current.t = unique(gsub(".*nwm.t\\s*|z..*", "", paths))

     if(is.null(t)){
       t = as.numeric(current.t)
     } else if(any(t > as.numeric(current.t))) {

       message = paste0(
         "Archived data only avaiable up to: \nDate: ",
         Sys.Date(),
         "\nUTC hour: ",
         current.t)

       stop(message)

     }

     if(is.null(f)){
        f = as.numeric(substring(eval(parse(text = paste0("nwm$", config, "$meta$flist")))[1:n], 2))
     }
    }


# Gerneral Error Checking -------------------------------------------------

  for(i in c("date", "type", "t", "f")) {
    errors = error.check(
      error = i,
      config = config,
      type = type,
      t = t,
      f = f,
      date = date
    )
    if (!is.null(errors)) {
      stop (errors)
    }
  }

# Check if forcing is requested -------------------------------------------

 if (type == 'forcing') { config = paste(type, config, sep = "_") }

# Use HTTP or OpenDaP -----------------------------------------------------

 base = paste0(server.base, ifelse(isTRUE(useHTTP), "/fileServer", "/dodsC"), "/nwm/")

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

 all.files = all.files[!grepl('NA', all.files)]

# Return file paths -------------------------------------------------------

  return(all.files)

}

