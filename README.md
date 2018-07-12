## NWM: An R client for the National Water Model. 


Each day the NOAA NWM produces ~400 GB of forecast data for the CONUS. This data is stored for a 40 day rolling window on the HydroShare Thredds server. This package aims to provide access to this data through the R environment. <br>

The package relies on the [AOI](https://github.com/mikejohnson51/AOI) package 
for defining an area of interest, and currently offers access to the `channel`, `land`, and `forcing` output
from the `short` and `medium` range configurations. <br>

Point data (channel) are output as "long" data.frames with COMIDS, DateTime (UTC), and the requested value. <br>
Gridded data (land and forcing) are returned as a raster stack. <br>

All downloads are parralelized using the `doParallel` and `foreach` packages to offer fast and efficient access.
<br>

More info about the National Water Model can be found [here](http://water.noaa.gov/about/nwm).

Documentation is upcoming ...

### Installation:

```
install.packages("devtools")
devtools::install_github("mikejohnson51/nwm")
```

### Support:

Package development is supported with funds from the UCAR COMET program; the NOAA National Water Center; and the University of California, Santa Barbara
