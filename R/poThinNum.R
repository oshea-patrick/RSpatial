#returns a thinned dataFrame
#' @export
#' @example
#' x=1
poThinNum <- function(df, lon, lat, num, maxRounds) {
  last <- 100
  thinned <-
    RSpatial::poThin(
      df = df,
      spacing = last,
      dimension = length(df$lon),
      lon = "lon",
      lat = "lat"
    )
  removed <- length(thinned)
  
  correction = (length(df$lat) - num) / removed
  
  round <- 1
  while ((length(df$lat)) > num*1.1 || (length(df$lat)) < num* 0.90) {
    if (correction > 1)
      df <- df[-thinned, ]
    
    thinned <-
      RSpatial::poThin(
        df = df,
        spacing = last*correction,
        dimension = length(df$lon),
        lon = "lon",
        lat = "lat"
      )
    removed <- length(thinned)
    last <- last * correction
    if (removed != 0)
      correction = (length(df$lat) - num) / removed
    round = round + 1
    if (round > maxRounds)
      break
  }
  
  return(df)
  
}
