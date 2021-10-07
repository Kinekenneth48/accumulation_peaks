# This is the SWE estimation method used by HILL

hill_conversion <- function(h, lon, lat, date, format = NULL,
                            coord_projection = NULL) {

  # Check if the date argument is valid. It can be a string or
  # a date object. If it is a string, the function requires
  # that you also input a format. If it is type Date,
  # the method just accepts the value. If neither are true,
  # methd stops.
  if (is.character(date)) { # Check if date is character
    if (is.null(format)) {  # If character, check if format is given
      stop('Inputting date argument as type "character" requires',
           ' an appropriate format to be used.', call. = FALSE)
    }
    # If both previous criteria are met, save DATE
    DATE <- as.Date(date, format = format)
  } else if (class(date) == "Date") { # Check if class "Date"
    DATE <- date
  } else {
    stop(
      'Class ', class(date), " is an invalid data-type for ",
      'the date argument. This method requires a "Date" type, ',
      'or a string with an appropriate format for the as.Date()',
      ' function.', call. = FALSE
      )
  }

  # Check if projection is given. If no projection is given,
  # assume data are projectionless and come from datum=WGS84
  if (is.null(coord_projection)) {
    tproj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  } else {
    tproj <- coord_projection
  }

  # Create Data Frame of Latitude and Longitude locations
  locationsDF <- data.frame(LONGITUDE = lon, LATITUDE = lat)
  locationsSP <- sp::SpatialPoints(coords = locationsDF)

  # Project data according to specified projection
  sp::proj4string(locationsSP) <- tproj

  # Load climateNA raster brick
  climateNA <- raster::brick("../data/climateNA.grd")

  # Convert locations to same projection as climateNA grid
  proj_locSP <- sp::spTransform(
    locationsSP, sp::CRS(sp::proj4string(climateNA))
  )

  # Get climate data for locations of interest
  climateData <- data.frame(
    raster::extract(climateNA, proj_locSP)
  )

  # Pass values into private hill__ method
  hill__(h, pptwt = climateData[, 'PPTWT'],
         td = climateData[, 'TD'], date = DATE)
}


hill__ <- function(h, pptwt, td, date) {
  # outputs SWE in mm, input is depth in mm

  if (length(date) == 1) {
    date <- rep(date, length(h))
  } else if (length(date) != length(h)) {
    stop('Length of date argument should be 1 or same length as h.', call. = FALSE)
  }

  YEAR <- lubridate::year(date)
  MONTH <- lubridate::month(date)

  prev_oct_yr <- ifelse(MONTH >= 10, YEAR, YEAR - 1)

  wtr_yr_start <- lubridate::make_date(prev_oct_yr,
                                       month = 10L,
                                       day = 1L)

  doy = as.numeric(
    difftime(
      date, wtr_yr_start, units = c('days')
    )
  )

  A = 0.0533
  B = 0.0481
  a1 = 0.9480
  b1 = 1.0395
  a2 = 0.1701
  b2 = 0.1699
  a3 = -0.1314
  b3 = -0.0461
  a4 = 0.2922
  b4 = 0.1804

  doy_star = 180

  ifelse(
    doy < doy_star,
    A * (h ^ a1) * (pptwt ^ a2) * (td ^ a3) * (doy ^ a4),
    B * (h ^ b1) * (pptwt ^ b2) * (td ^ b3) * (doy ^ b4)
  )
}
