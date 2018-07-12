
file.tst = paste0("http://thredds.hydroshare.org/thredds/dodsC/nwm/medium_range/20180709/nwm.t18z.medium_range.land.f033.conus.nc?",PARAM,"[0:1:0][", lat.min, ":1:", lat.max ,"][",long.min, ":1:", long.max,"]")
nc = nc_open(file.tst)
vals = ncvar_get(nc, PARAM)

foo <- apply(t(vals),2,rev)

r = raster::raster(foo)

raster::projection(r) = A@proj4string

raster::extent(r) = raster::extent(A)


raster::plot(r)
sp::plot(A, add = T)

