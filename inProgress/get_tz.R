get_tz = function(coords)
{
  # https://developers.google.com/maps/documentation/timezone/

  if(length(coords) != 2){stop("Coord input not of length 2")}

  #require(RCurl)
  #require(RJSONIO)

  url = paste0("https://maps.googleapis.com/maps/api/timezone/json?location=",coords[1],",",coords[2], "&timestamp=0&sensor=false")

  loc = RJSONIO::fromJSON(RCurl::getURL(url))$timeZoneId

  return( loc )
}

