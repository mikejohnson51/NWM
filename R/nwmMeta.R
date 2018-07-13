forcing = data.frame(

  PARAM = c(
    "t2d",
    "q2d",
    "u2d",
    "v2d",
    "psfc",
    "swdown",
    "lwdown",
    "rainrate"
  ),

  UNITS = c(
    "degrees K",
    "kg/kg",
    "m/sec",
    "m/sec",
    "Pa",
    "w/m2",
    "w/m2",
    "kg/m2sec"
  ),

  DESCRIPTION = c(
    "2 meter temperature",
    "2 meter specific humidity",
    "10 meter U wind component",
    "10 meter V wind component",
    "Surface pressure",
    "Downward shortwave radiation",
    "Downward longwave radiation",
    "Precipitation rate"
  ),
  stringsAsFactors = FALSE
)


channel = data.frame(

  PARAM = c("streamflow",
            "nudge",
            "velocity",
            "q_lateral"),

  UNITS = c("m3/sec",
            "m3/sec",
            "m/s",
            "m3/sec"),

  DESCRIPTION = c(
    "Streamflow",
    "Streamflow data assimilation increment",
    "Stream velocity",
    "Channel inflow"
  ),

  stringsAsFactors = FALSE
)




nwm = list(

  short_range = list(

    channel = channel,

    land = data.frame(

      PARAM = c("soilsat_top",
                "accet",
                "snowt_avg",
                "fsno",
                "sneqv",
                "snowh"),

      UNITS = c("fraction",
                "mm",
                "degrees K",
                "fraction",
                "kg/m2",
                "m"),

      DESCRIPTION = c(
        "Near surface soil moisture deficit 40cm thickness",
        "Accumulated ET",
        "Snow temperature - column integrated",
        "Column averaged snow cover fraction",
        "Snow water equivalent",
        "Snow depth"
      ),

      stringsAsFactors = FALSE
    ),

    forcing = forcing,

    meta = list(
      flist = paste0("f", sprintf("%03d", 1:18)),
      tlist = paste0("t", sprintf("%02d", 0:23)),
      DESCRIPTION = "Forced with meteorological data from the HRRR and RAP models, the Short Range Forecast configuration cycles hourly and produces hourly deterministic forecasts of streamflow and hydrologic states out to 18 hours. The model is initialized with a restart file from the Analysis and Assimilation configuration and does not cycle on its own states."
    )

  ),


  medium_range = list(
    channel = channel,

    land = data.frame(

      PARAM = c(
        "ugdrnoff",
        "acsnom",
        "snowh",
        "sneqv",
        "canwat",
        "accecan",
        "accetran",
        "accedir",
        "snliq",
        "isnow",
        "soil_t",
        "snowt_avg",
        "fsno",
        "soil_m",
        "soilsat_top",
        "soilice",
        "accet",
        "grdflx",
        "hfx",
        "lh",
        "fira",
        "fsa",
        "trad"
      ),

      UNITS = c(
        "mm",
        "mm",
        "m",
        "kg/m2",
        "mm",
        "mm",
        "mm",
        "mm",
        "mm",
        "integer",
        "degrees K",
        "degrees K",
        "fraction",
        "m3/m3",
        "fraction",
        "fraction",
        "mm",
        "w/m2",
        "w/m2",
        "w/m2",
        "w/m2",
        "w/m2",
        "degrees K"
      ),

      DESCRIPTION = c(
        "Accumulated underground runoff",
        "Accumulated snowmelt",
        "Snow depth",
        "Snow water equivalent",
        "Total canopy water storage",
        "Accumulated canopy evaporation",
        "Accumulated transpiration",
        "Accumulated direct soil evaporation",
        "Snow layer liquid water",
        "Number of snow layers",
        "Soil temperature on native layers",
        "Snow temperature - column integrated",
        "Snow cover fraction",
        "Volumetric soil moisture on native layers",
        "Near surface soil moisture defiict 40cm thickness",
        "Soil ice fraction - column integrated",
        "Accumulated evapotranspiration",
        "Ground heat flux",
        "Sensible heat flux",
        "Latent heat flux",
        "Net longwave flux",
        "Net shortwave flux",
        "Surface radiative temperature"
      ),

      stringsAsFactors = FALSE
    ),

    forcing = forcing,

    meta = list(
      flist = paste0("f", sprintf("%03d", seq(3, 240, 3))),
      tlist = paste0("t", sprintf("%02d", seq(0, 23, 6))),
      DESCRIPTION =  "The Medium Range Forecast configuration is executed four times per day, is forced with GFS model output and extends out to 10 days. It produces 3-hourly deterministic output and is initialized with the restart file from the Analysis and Assimilation configuration."
    )
  ),


  long_range = list(

    channel =  channel[-3,],

    land = data.frame(

      PARAM = c(
        "ugdrnoff",
        "sfcrnoff",
        "acsnom",
        "sneqv",
        "soilsat",
        "canwat",
        "soilsat_top",
        "accet"
      ),

      UNITS = c("mm",
                "mm",
                "mm",
                "kg/m2",
                "fraction",
                "mm",
                "fraction",
                "mm"),

      DESCRIPTION = c(
        "Underground runoff",
        "Surface runoff",
        "Accumulated snowmelt",
        "Snow water equivalent",
        "Total column soil moisture deficit",
        "Total canopy water storage",
        "Near surface soil moisture deficit, 40cm thickness",
        "Accumulated evapotranspiration"
      ),

      stringsAsFactors = FALSE
    ),

    forcing = forcing,

    meta = list(
      flist = paste0("f", sprintf("%03d", seq(6,720,6))),
      tlist = "t00",
      DESCRIPTION = "The Long Range Forecast cycles four times per day (i.e. every 6 hours) and produces a daily 16-member 30-day ensemble forecast. There are 4 ensemble members in each cycle of this forecast configuration, each forced with a different CFS forecast member. It produces 6-hourly streamflow and daily land surface output, and, as with the other forecast configurations, is initialized with a common restart file from the Analysis and Assimilation configuration."
    )
  )
)

