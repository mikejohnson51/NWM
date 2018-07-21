## NWM: An R client for the National Water Model.

[![Build Status](https://travis-ci.org/mikejohnson51/NWM.svg?branch=master)](https://travis-ci.org/mikejohnson51/NWM)

Each day the NOAA NWM produces ~400 GB of forecast data for the CONUS. This data is stored for a 40 day rolling window on the HydroShare Thredds server amounting to over 16,000 GB (2 TB) of data being stored and accessible at any one time. This package aims to provide access to this data in a clean, fast, and convenient way through the R environment. <br>

The package relies on the [AOI package](https://github.com/mikejohnson51/AOI) 
for defining an area of interest, and currently offers access to the `channel`, `land`, and `forcing` output
from the `short` and `medium` range configurations. In the works are (1) access to the `long` range configuration<br>

Point data (channel outputs) are generated as "long" data.frames with COMIDS, DateTime (UTC), and the requested values. <br>
Gridded data (land and forcing) are returned as a raster stack. <br>

All downloads are parallelized using the `doParallel` and `foreach` packages to offer fast and efficient access.
<br>

More info about the National Water Model can be found [here](http://water.noaa.gov/about/nwm).

**VERY ROUGH** Documentation can be found [here]( https://mikejohnson51.github.io/NWM/) with improvements coming soon...

### Installation:

```
install.packages("devtools")
devtools::install_github("mikejohnson51/nwm")
```

### Support:

Package development is supported with funds from the UCAR COMET program; the NOAA National Water Center; and the University of California, Santa Barbara
