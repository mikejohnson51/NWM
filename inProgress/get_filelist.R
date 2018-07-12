get_filelist = function(config = "medium_range", type = "channel", date = NULL, t = 0, f = 1){

  all.files = NULL

  base = "http://thredds.hydroshare.org/thredds"

  if(type == 'forcing'){ config = paste(type, config, sep = "_")}

    url = paste(base, "catalog/nwm", config, date, sep = "/")

    if (!(config %in% nwm.setup$config.list)) {
      stop(paste0(toupper(config), " not a valid configuration for ", config, " configuration"))
    }

    if(all(!is.element(paste0("f", sprintf("%03d", f)), eval(parse(text = paste0("nwm.setup$", config, ".flist")))))){
      stop(paste0(f, " not a valid f value"))
    }

    if(all(!is.element(paste0("t", sprintf("%02d", t)), eval(parse(text = paste0("nwm.setup$", config, ".tlist")))))){
      stop(paste0(t, " not a valid t value for ", config, " configuration.\n\n", "Use: "), paste(eval(parse(text = paste0("nwm.setup$", config, ".tlist"))), collapse = ", "))
    }

    for( d in seq_along(url)){

    L = readLines(paste0(url[d], "/catalog.html"))
    LL = L[grep('.nc</tt>' , L)]
    LLL = LL[grep(type , LL)]
    LLLL = LLL[grep(paste0("t", sprintf("%02d", t), collapse = "|") , LLL)]
    LLLLL = LLLL[grep(paste0("f", sprintf("%03d", f), collapse = "|"), LLLL)]

    files = paste0(base,
                   "/dodsC/nwm/",
                   config,
                   "/",
                   date[d],
                   "/",
                   gsub(".*<tt>\\s*|</tt>.*", "", LLLLL))

    if (length(files) == 0) {
      stop("No files found for specified parameters")
    }

    all.files = append(files, all.files)
    }

  return(all.files)
}



setClass( "nwm_call", representation("list"))

# show method (here's how the output would be printed
# you can format to whatever you want... to show and how to show
setMethod("show", "nwm_call", function(object) {
  cat("Returning Results from the NOAA National Water Model:\n\n")
  cat("Model Configuration:", toupper(gsub("_", " ", config)), "\n\n")
  cat("Description:", eval(parse(text = paste0("nwm.setup$", config, ".des"))), '\n\n')

  cat("Year(s):",  unique(substring(date, 1,4)), "\n")
  cat("Month(s):", unique(substring(date, 5,6)), "\n")
  cat("Day(s):", unique(substring(date, 7,8)), "\n\n")
  cat("Hour of forecast:", paste(sprintf("%02d", t), sep = ", "), "\n")
  cat("Hour(s) forward:", paste(sprintf("%03d", f), sep = ", "), "\n\n")

  cat("Requested COMIDS:", nrow(df), "\n\n")


  cat("Total output files needed:", length(all.files))
})

# now your actual function (here dummy of course)
my_fun <- function(x) {
  t <- list(config = config, date = date, t = t, f = f, df = df )
  new("nwm_call", t)
}


