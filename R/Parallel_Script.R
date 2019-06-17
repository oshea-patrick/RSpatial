#returns a thinned dataFrame
#' @export
#' @example 
#' x=1
poThinP <- function(df, spacing, lon, lat, numCores) {
  requireNamespace(RSpatial)
  requireNamespace(parallel)
  
  #wrapped function
  poThinWrap <- function(testing) {
    vec = (RSpatial::poThin(df = testing, spacing = spacing, dimension = length(testing$lon),lon = lon1, lat=lat1))
    thinned = testing[vec, ]
    return(thinned$originalIndices)
  }
  
  
  blocks <- classifySpatiallyByBlocks(df=df,lon = lon, lat = lat, size = length(df$lon), blockSize = 10)
  df <- df[,c(lon, lat)]
  indices <- 1:length(df$lon)
  df <- cbind(df, blocks, indices)
  colnames(df) <- c(lon, lat, "blocks", "originalIndices")
  cl <- parallel::makeCluster(numCores)
  
  #define "globally"
  spacing = spacing
  lon1 = lon
  lat1 = lat
  
  #break up into different lists of dataframes for cores
  
  lists <- list()
  n=1
  for (i in unique(df$blocks)) {
    lists[[n]] <- df[which(df$blocks == i), ]
    n = n+1
  }
  
  parallel::clusterExport(cl,
                c(
                 "spacing",
                  "lon1",
                  "lat1"
                ),
                envir = environment())

  lists <- parallel::parLapply(cl, lists, poThinWrap)
  parallel::stopCluster(cl)
  
  unlisted <- unlist(lists)
  thinnedDf <- df[-unlisted, ]
  
  thinnedDf <- thinnedDf[,c(lon, lat)]
  
  return(thinnedDf[-poThin(df=thinnedDf, spacing = spacing, dimension = length(thinnedDf$lon), lon = lon, lat = lat), ])
}

