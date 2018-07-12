

download.streamflow = function(file, index){
  nc = nc_open(paste0(filelist[1],"?streamflow[", min(index),
               ":1:",
               max(index),
               "]"))
  var = ncvar_get(nc, "streamflow")
  sub = match(index, c(min(index):max(index)))
  var = var[sub]
  return(var)
} # 5.397

system.time(
foreach( i = 1:length(filelist)) %do% download.streamflow(file = filelist[i], index = index)
) #19.422

system.time(
  foreach( i = 1:length(filelist)) %do% dl.strm(file = filelist[i], dd = dd)
) #42



dl.strm = function(file, dd){
  utils::flush.console()
  vals = NULL
  for (j in 1:length(dd)) {
    file = paste0(
      filelist[i],
      "?streamflow[",
      min(dd[[j]]),
      ":1:",
      max(dd[[j]]),
      "]"
    )
    #print("Open")
    nc = nc_open(file, readunlim = F, suppress_dimvals = F)
    #print("Read")
    tmp = ncvar_get(nc, "streamflow")
    #print("write")
    vals = append(vals, tmp)
    nc_close(nc)

  }
  dff[[paste0("Flow_", i)]] = vals
  message(paste0("File ", i, " of ", length(filelist), " complete!"))
  gc()


} #30.183

