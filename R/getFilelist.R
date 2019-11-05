#' Get file list
#' @description get a list of paths to files containing the requested data
#'
#' @param config model configuration
#' @param type output type
#' @param startDate dates of interst
#' @param t time of forecast
#' @param f hours foward from t
#' @param n if f is null n can be used to limit the number of files returned
#' @param m ensemble memeber (long_range only)
#' @param useHTTP use HTTP? default = FALSE
#'
#' @return a list of file paths
#' @export
#' @author Mike Johnson


getFilelist = function(config = "short_range",
                    type = "channel",
                    startDate = NULL,
                    endDate = NULL,
                    t = NULL,
                    f = NULL,
                    n = 5,
                    m = NULL) {

  if(config == "medium_range"){
    stop("As of 11/05/2019 (Mike last checked) the HydroShare server is not archiving the new ensemble MR forecasts. Until they do, MR values cannot be queried.")
  }

  server.base = "http://thredds.hydroshare.org/thredds/"

  if(!is.null(startDate)) {
    if (is.null(endDate)) {
      endDate = startDate
    }
    date = seq.Date(as.Date(startDate), as.Date(endDate), 1)
  } else {
    date = NULL
  }

  errors = error.check(
    error = "configuration",
    config = config,
    type = type,
    t = t,
    f = f,
    date = date
  )

  if (!is.null(errors)) { stop (errors) }

  if(is.null(date) || max(date) == Sys.Date()){

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

  date = gsub("-", "", date)

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

 base = "http://thredds.hydroshare.org/thredds/dodsC/nwm/"

# Build file paths --------------------------------------------------------

  if(config == 'long_range'){ ext = paste0("_", m) } else { ext = NULL }

  ext = param.combine(config, type, t, f, ext)

  ext = expand.grid(date, "/", ext, stringsAsFactors = F)

  ext = as.matrix(ext[order(ext$Var1),])

  ext <- paste0(ext[,1],ext[,2], ext[,3])

  all.files = paste0(
    base,
    config,
    "/",
    ext
  )

 all.files = all.files[!grepl('NA', all.files)]

# Return file paths -------------------------------------------------------

  return(all.files)

}

