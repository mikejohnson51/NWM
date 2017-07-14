
  


get_HUC6_data = function(HUC6 = "010100"){
  build.file.list = c(paste0(HUC6, "-flows.dbf"),
                      paste0(HUC6, "-flows.prj"),
                      paste0(HUC6, "-flows.shp"),
                      paste0(HUC6, "-flows.shx"),
                      paste0(HUC6, "-wbd.dbf"),
                      paste0(HUC6, "-wbd.prj"),
                      paste0(HUC6, "-wbd.shp"),
                      paste0(HUC6, "-wbd.shx"))

  for(i in 1:length(build.file.list)){
    URL = paste0("http://141.142.170.172/nfiedata/HUC6/",HUC6,"/", build.file.list[i])
    download.file(url = URL, destfile = paste0(getwd(),"/Flowlines/",build.file.list[i]))
  }
  
  }