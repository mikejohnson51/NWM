#' @title Download Retrospective NWM model data
#' @description the function downloads retrospective data from the AWS server. Data is avaialable from 1993-01-01 to 2017-12-31.
#' All data is downlaoded to a users to disk and stored in a folder called 'retro_nwm' either in the current working directory or
#' in a path specified with write path.
#' @param date a date provided as a character string in %Y%m%d
#' @param t the time of forecast (t)
#' @param type the output type
#' @param write.path the directory path to save to
#' @return NULL
#' @export
#' @author Mike Johnson

getRetro= function(date, t = 0, type = 'channel', write.path = NULL){

  `%+%` <- crayon::`%+%`

  ## Buid Dates in only year is provided

  if(all(nchar(date) == 4)){
    min = min(date)
    max = max(date)
    date = gsub("-", "", seq.Date(
      as.Date(paste0(min,'-01-01')),
      as.Date(paste0(max,'-12-31')),
      by = 1
    ))
  }

  ## Build dates on month and year are provided

  if(all(nchar(date) == 6)){
    min = min(date)
    max = NA
    i = 31
    max.date = min(date)

    while (is.na(max)){
      max = as.Date(paste0(max.date,i), format = '%Y%m%d')
      i = i -1
    }

    date = gsub("-", "", seq.Date(
      as.Date(paste0(min,"01"), format = '%Y%m%d'),
      max,
      by = 1
    ))
  }

  if(is.null(write.path)){ write.path = "./retro_nwm" }

  if(!dir.exists(write.path)){dir.create(write.path)}

  raw.dir = normalizePath(write.path, mustWork = T)

  if( any(substr(date, 1,4) > 2017, substr(date, 1,4) < 1993)){ stop( date, " not a valid date. Must be between 1993-01-01 and 2017-12-31") }

  if( any(t > 23, t < 0) ){ stop( t, " not a valid time. Must be between 0 and 23") }

  meta = data.frame(type = c("channel", "land", "reservoir", "terrain"),
           prod = c("CHRTOUT", "LDASOUT", "LAKEOUT", "RTOUT"),
           stringsAsFactors = FALSE)

  if (!all(type %in% meta$type)){
    bad.param <- type[!(type %in% meta$type)]
    stop("'", paste(bad.param, collapse = "', '"), "'", if (length(bad.param) > 1)
    {
      paste(" are")
    } else {
      paste(" is")
    }, " not a valid type. Must be: \n'", paste(meta$type, collapse = "', '"), "'")
  }

  year = substr(date, 1, 4)

  dir = expand.grid(unique(year), unique(type))
    names(dir) = c("year", "type")

  for(i in 1:NROW(dir)){
    p = paste0(raw.dir, "/", dir$year[i])
    if(!dir.exists(p)){ dir.create(p) }
    o = paste0(p, "/", dir$type[i])
    if(!dir.exists(o)){ dir.create(o) }
  }

time = paste0(sprintf("%02d", t), "00")
dates = paste0(substr(date, 1,4), "/", date)
dateTime = expand.grid(dates, time)
dateTime = paste0(do.call(paste, c(dateTime, sep="")), ".")

call = expand.grid(dateTime, meta$prod[which(type %in% meta$type)])
call = do.call(paste, c(call, sep=""))

for(i in seq_along(call)){

  url = paste0("https://nwm-archive.s3.amazonaws.com/", call[i], "_DOMAIN1.comp")

  tt = meta$type[grepl(sub('.*\\.', '', call[i]), meta$prod)]

  tmp <- paste0(raw.dir,
                "/",
                substr(call[i],1,4), #year
                "/",
                tt, #type
                "/nwm.",
                substr(call[i], 6,13),
                ".t",
                substr(call[i],14,15), # dateTime
                "z.",
                tt, #type
                if(tt %in% c("channel", "terrain")){"_rt"},
                ".conus.nc")

  if(!file.exists(tmp)){
    cat(crayon::white(paste0("Downloading (", i, "/", length(call), "): ")) %+% crayon::yellow(basename(tmp)), "\n")
    httr::GET(url, httr::write_disk(tmp), httr::progress())
  } else {
    cat(crayon::white("Archived: ") %+% crayon::green(basename(tmp)), "\n")
  }
}

cat("\n")
cat(crayon::white("All files stored at: ") %+% crayon::blue(paste0(raw.dir, "/...")))

}

getRetro(date = 19950309, type = c("land", "channel"))

