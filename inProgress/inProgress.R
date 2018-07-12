http.files = get_files(date = c(20180525:20180601),
                       t = 0,
                       f = 3, useHTTP = T)

open.files = get_files(
  date = c(20180521:20180530),
  t = 0,
  f = 3, useHTTP = F
)

load("/Users/mikejohnson/Desktop/all_COMIDS.rda")


#system.time({
  ## 38.161 for 6 files
#  tmp = tempdir()
#  for (i in seq_along(http.files)) {
#    destfile = paste0(tmp, "/current.nc")

#    download.file(url = http.files[i],
#                  mode = "wb",
#                  destfile = destfile)


#    tmp.file = nc_open(destfile)
#    df[[paste0("Flows_", i)]] = ncvar_get(tmp.file, "streamflow")[index]

#    file.remove(destfile)

#  }

#})


df = get_nwm_points(getAOI(clip_unit = list("UCSB", 30, 10)))
comids_of_value = df$COMID

system.time({

  index = match(df$COMID, comids.all)

  id = which(diff(index) > 50)
  id.max = id + 1
  id.min = c(1, head(id.max + 1, -1))

  dd = list()

  for(i in 1:length(id.max)){
    dd[[paste0("set",i)]] = index[id.min[i]:id.max[i]]
  }

  cid = list()
  for(i in 1:length(dd)){
    cid[[paste0("set", i)]] = c(min(dd[[i]]):max(dd[[i]]))
  }
  ## 20.155 seconds for 6 files
  dff = data.frame(comid = unlist(cid, use.names = FALSE))
  for (i in 1:length(open.files)) {
    vals = NULL

    for (j in 1:length(dd)) {
      file = paste0(
        open.files[i],
        "?streamflow[",
        min(dd[[j]]),
        ":1:",
        max(dd[[j]]),
        "]"
      )

    vals = append(vals, ?ncvar_get(nc_open(file, readunlim = F, suppress_dimvals = T), "streamflow"))

    }
    dff[[paste0("Flow_", i)]] = vals
    message(paste0("File ", i, " of ", length(open.files), " complete!"))
  }

  dff1 = dff[which(dff$comid %in% index), ]
  dff1$comid = df$COMID

})

