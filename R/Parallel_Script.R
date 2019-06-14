#returns a thinned dataFrame

poThinP <- function(df, spacing, lon, lat, numCores) {
  require(RSpatial)
  require(parallel)
  
  #wrapped function
  poThinWrap <- function(testing) {
    vec = (RSpatial::poThin(df = testing, spacing = spacing, dimension = length(testing$lon),lon = lon1, lat=lat1))
    thinned = testing[vec, ]
    return(thinned$originalIndices)
  }
  
  
  blocks <- classifySpatiallyByBlocks(df=df,lon = lon, lat = lat, size = length(df$lon))
  df <- df[,c(lon, lat)]
  indices <- 1:length(df$lon)
  df <- cbind(df, blocks, indices)
  colnames(df) <- c(lon, lat, "blocks", "originalIndices")
  cl <- makeCluster(numCores)
  
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
  
  clusterExport(cl,
                c(
                 "spacing",
                  "lon1",
                  "lat1"
                ),
                envir = environment())

  lists <- parLapply(cl, lists, poThinWrap)
  stopCluster(cl)
  
  unlisted <- unlist(lists)
  thinnedDf <- df[-unlisted, ]
  
  return(thinnedDf[-poThin(df=thinnedDf, spacing = spacing, dimension = length(thinnedDf$lon), lon = lon, lat = lat), ])
}
