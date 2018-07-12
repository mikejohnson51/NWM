downloadNWM = function(AOI = AOI, filelist = filelist, param = NULL){

  load("data/all_COMIDS2.rda")
  df = get_nwm_points(AOI)
  message("Streamflow stations identified. :)")

  index = match(df, comids.all)
  index = index[complete.cases(index)]

  id = which(diff(index) > 50)
  id.max = c(id, length(index))
  id.min = c(1, head(id.max + 1,-1))

  dd = list()

  for(i in 1:length(id.max)){
     dd[[paste0("set",i)]] = index[id.min[i]:id.max[i]]
  }

  cid = list()

  for(i in 1:length(dd)){
      cid[[paste0("set", i)]] = c(min(dd[[i]]):max(dd[[i]]))
  }


  dff = data.frame(COMID = unlist(cid, use.names = FALSE))

  message("Beggining download.")

  for (i in 1:length(filelist)) {

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
        print("Open")
        nc = nc_open(file, readunlim = F, suppress_dimvals = F)
        print("Read")
        tmp = ncvar_get(nc, "streamflow")
        print("write")
        vals = append(vals, tmp)
        nc_close(nc)

      }
      dff[[paste0("Flow_", i)]] = vals
      message(paste0("File ", i, " of ", length(filelist), " complete!"))
      gc()
    }

    dff1 = dff[which(dff$COMID %in% index), ]
    dff1$COMID = df

    df3 = merge(df, dff1, "COMID")

    return(df3)

}
