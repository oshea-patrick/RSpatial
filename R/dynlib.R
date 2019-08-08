
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

#' rasterMaker
#' 
#' This is a Test
#' 
#' @param df
#' @param minX
#' @param maxY
#' @param nCols
#' @param nRows
#' @param sizeOfGen
#' @param sizeOfList
#' @param degree
#' @param block
#' 
#' @export
#' @example 
#' x=1
#' 
rasterMaker <- function(df, minX, maxY, nCols, nRows, sizeOfGen, sizeOfList, degree, block) {
  .Call('_RSpatial_rasterMaker', PACKAGE = 'RSpatial', df, minX, maxY, nCols, nRows, sizeOfGen, sizeOfList, degree, block)
}

#' rasterMakerAdjust
#' 
#' This is a Test
#' 
#' @param df
#' @param minX
#' @param maxY
#' @param nCols
#' @param nRows
#' @param sizeOfGen
#' @param sizeOfList
#' @param degree
#' @param block
#' @param horizontal
#' @param vertical
#' 
#' @export
#' @example 
#' x=1
#' 
rasterMakerAdjust <- function(df, minX, maxY, nCols, nRows, sizeOfGen, sizeOfList, degree, block, horizontal, vertical) {
  .Call('_RSpatial_rasterMakerAdjust', PACKAGE = 'RSpatial', df, minX, maxY, nCols, nRows, sizeOfGen, sizeOfList, degree, block, horizontal, vertical)
}
