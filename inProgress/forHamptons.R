AOI = getAOI(rgdal::readOGR("/Users/mikejohnson/Desktop/HAMPTONS/HamptonRoads/HamptonRoads.shp"), )
sp::plot(AOI)

nhd = HydroData::findNHD(AOI)

nhd = nhd$nhd[nhd$nhd$streamorde > 2,]
sp::plot(nhd)


files = getFilelist(n = 10)

xx = system.time({
  flows <-downloadNWM(AOI, files, "streamflow")
})

plot(flows$streamflow$streamflow[flows$streamflow$COMIDS == '8456868' ], type = 'l')
