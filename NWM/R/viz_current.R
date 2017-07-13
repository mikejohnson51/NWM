  viz_current = function(catchment_path, flowlines_path, COMID, region){
    
  files = dir(paste0(getwd(),"/NetCDFs/Current"))
  file.remove(list.files(paste0(getwd(),"/Images/Current"), pattern = ".png", full.names = TRUE))
  
  data = read.csv(paste0(getwd(),"/Output/Current/", dir(paste0(getwd(),"/Output/Current"))))
  data = as.matrix(data)

  
  catchments = readOGR(catchment_path)
  flowlines = readOGR(flowlines_path)
  
  for(i in 1:18){ 
    
    png(paste0(getwd(),"/Images/Current/timestep", sprintf("%03d",i), ".png"), width = 3000, height = 1500, units= 'px', res = 300)
    
    par(mfrow= c(1,2)); 
    
    index = which(data[,1] == COMID)
    
    plot(catchments, border = 'white', col= 'lightgrey', main = paste0("Short Range Forecasts For \n ", region), 
         ylim = c(catchments@bbox[2,1]+.05,catchments@bbox[2,2]),
         xlab = paste0(paste0("Forcast Type: ", substrRight(substr(files[1], 1, 20),11)),
                        paste0("\nDate: ", Sys.Date()),
                        paste0("\nTime of forecast: ", substrRight(substr(files[1], 1,7),2)),
                        paste0("\nTime since forcast (hr): ", substrRight(substr(files[i], 1, 36), 3)))) 
    
    
    plot(flowlines,add = TRUE, col ="blue", lwd = (data[,i+1]/10))      
    
    plot(data[index,2:dim(data)[2]], type = "l", main = paste0("Streamflow at ", COMID), ylab = "Streamflow (cfs)", xlab= 'Time since forecast')
    points(i,data[index,i+1], cex = 1, pch =16, col = 'red')  
    
    dev.off()
  }
  }
    
    
    