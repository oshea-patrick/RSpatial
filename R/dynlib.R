#' @useDynLib RSpatial
#' @importFrom Rcpp sourceCpp
NULL

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
