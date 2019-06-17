
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
.classifySpatiallyByBlocks <- function(df, lon, lat, size, blockSize) {
  .Call('_RSpatial_classifySpatiallyByBlocks', PACKAGE = 'RSpatial', df, lon, lat, size, blockSize)
}
