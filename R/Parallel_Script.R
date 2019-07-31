#returns a thinned dataFrame
#' @export
#' @example
#' x=1
poThinP <- function(df, spacing, lon, lat, numCores) {
  require(RSpatial)
  require(parallel)
  
  #wrapped function
  poThinWrap <- function(testing) {
    return (RSpatial::poThin(df = testing, spacing = spacing, dimension = length(testing$lon),lon = lon1, lat=lat1))
  }
  
  
  blocks <- RSpatial::classifySpatiallyByBlocks(df=df,lon = lon, lat = lat, size = length(df$lon), blockSize = 10)
  df <- df[,c(lon, lat)]
  df <- cbind(df, blocks)
  colnames(df) <- c(lon, lat, "blocks")
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
  
  # Clustering the cores and doring parallel calls
  parallel::clusterExport(cl,
                c(
                 "spacing",
                  "lon1",
                  "lat1"
                ),
                envir = environment())

  lists <- parallel::parLapply(cl, lists, poThinWrap)
  
  # Thins the sub-dataframes
  unlisted <- unlist(lists)
  thinnedDf <- df[-unlisted, ]
  thinnedDf <- thinnedDf[,c(lon, lat)]
  
  blocks <- classifySpatiallyByBlocks(df=thinnedDf,lon = lon, lat = lat, size = length(thinnedDf$lon), blockSize = 20)
  thinnedDf <- thinnedDf[,c(lon, lat)]
  thinnedDf <- cbind(thinnedDf, blocks)
  colnames(thinnedDf) <- c(lon, lat, "blocks")
  
  
  lists <- list()
  n=1
  for (i in unique(thinnedDf$blocks)) {
    lists[[n]] <- thinnedDf[which(thinnedDf$blocks == i), ]
    n = n+1
  }
  
  lists <- parallel::parLapply(cl, lists, poThinWrap)
  
  # Thins the sub-dataframes
  unlisted <- unlist(lists)
  thinnedDf <- thinnedDf[-unlisted, ]
  thinnedDf <- thinnedDf[,c(lon, lat)]
  
  
  parallel::stopCluster(cl)
  
  #last Ditch Thinning just in case
  ld <- poThin(df = thinnedDf, spacing = spacing, dimension = length(thinnedDf$lon),lon = lon, lat=lat)
  thinnedDf <- thinnedDf[-ld, ]
  
  return(thinnedDf)
}

