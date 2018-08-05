getNWM_point = function(AOI, type, filelist, param){

  if(!(class(AOI) %in% c("list","HydroData"))){AOI = list(AOI = AOI)}

  if(type == "channel"){

    for(i in seq_along(param)){

      vals = getChannel(AOI = AOI$AOI,
                        filelist = filelist,
                        param = param[i])

      AOI[[param[i]]] = vals
    }
  }

  if(type == "reservoir"){

    for(i in seq_along(param)){

      vals = getReservoir(AOI = AOI$AOI,
                        filelist = filelist,
                        param = param[i])

      AOI[[param[i]]] = vals
    }
  }

  return(AOI)
}