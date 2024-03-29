
#' poThin
#' 
#' This is a Test
#' 
#' @param df
#' @param spacing
#' @param dimension
#' @param lon
#' @param lat
#' 
#' @export
#' @example 
#' x=1
#' 
poThin <- function(df, spacing, dimension, lon, lat) {
.Call('_RSpatial_poThin', PACKAGE = 'RSpatial', df, spacing, dimension, lon, lat)
}

#' classifySpatiallyByBlocks
#' 
#' This is also a Test
#' 
#' @param df
#' @param lon
#' @param lat
#' @param size
#' 
#' @export
#' @example 
#' x=1
#' 
classifySpatiallyByBlocks <- function(df, lon, lat, size, blockSize) {
  .Call('_RSpatial_classifySpatiallyByBlocks', PACKAGE = 'RSpatial', df, lon, lat, size, blockSize)
}

#' createNextGen
#' 
#' This is also a Test
#' 
#' @param df
#' @param avnValues
#' @param mutationRate
#' @param size
#' @param randoms
#' 
#' @export
#' @example 
#' x=1
#' 
createNextGen <- function(df, avnValues, mutationRate, explore, size, randoms) {
  .Call('_RSpatial_createNextGen', PACKAGE = 'RSpatial', df, avnValues, mutationRate, explore, size, randoms)
}

#' isInsideConvexHull
#' 
#' This is also a Test
#' 
#' @param hulls
#' @param checkx
#' @param checky
#' @param size
#' 
#' @export
#' @example 
#' x=1
#' 
isInsideConvexHull <- function(hulls, checkx, checky, size) {
  .Call('_RSpatial_isInsideConvexHull', PACKAGE = 'RSpatial', hulls, checkx, checky, size)
}
