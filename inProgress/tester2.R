
##get data for one index

get_channel_data = function(filebase, val, param) {
  file = paste0(filebase,
                "?",
                param,
                "[",
                min(val),
                ":1:",
                max(val),
                "]")

  vals = ncvar_get(nc_open(file), "streamflow")

  return(vals)
}

## get data for one file all index
getData = function(filename, xx) {
  no_cores <- detectCores() - 1
  registerDoParallel(no_cores)

  tmp <-
    foreach(i = 1:length(xx), .combine = "c") %dopar% get_channel_data(filebase = filename, val = xx[[i]])

  message(paste("Finished", filename))

  return(tmp)
}


##  get data for all files all index

finalData = function(AOI, filelist){
xx = comidList(AOI, 2)
no_cores <- detectCores() - 1
registerDoParallel(no_cores)
fin <- foreach(i = 1:length(filelist), .combine = "cbind") %dopar% getData(filename = filelist[i], xx = xx)
return(fin)
}


