library(raster)
library(ENMeval)
require(spocc)
library(rgbif)
library(fields)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra)
#testing
# Get climate data
clim = stack(list.files('/usr/share/data/wc2.0/bio2.5', pattern="*.tif", full.names = T))
ext = extent(c(-140, -60, 25, 65))
Env = crop(clim, ext)

# Download species distribution
#occ = occ('Ambystoma opacum', from = 'gbif', limit=10000)
occ = occ('Ursus americanus', from = 'gbif', limit = 5000)
#occ = occ_search(scientificName = "Ursus americanus", fields=c('name','basisOfRecord','protocol'), limit = 5000)
occdf = occ2df(occ)
loc <- na.omit(occdf)
loc <- cbind(loc$longitude,loc$latitude)
loc = loc[loc[,1]<= -60,]
loc = na.omit(loc)
colnames(loc) <- c('lon', 'lat')
remove <- poThin(df = loc, spacing = 50, dimension = 4702, lon = 'lon', lat = 'lat')
loc <- loc[-remove,]
trial <- assign_clusters(as.data.frame(loc), 10, 10)

res = ENMevaluate(
  occ = loc,
  env = Env,
  bg.coords = background,
  method = 'block',
  parallel = T,
  numCores = 24,
  fc = c("L"),
  RMvalues = seq(0.5, 4, 0.5),
  progbar = TRUE,
  updateProgress = TRUE,
  rasterPreds = T
)


#Pick best model: Min AICc
best = which(res@results[,'AICc']==min(na.omit(res@results[,'AICc'])))
best = which(res@results[,'avg.test.AUC']==max(na.omit(res@results[,'avg.test.AUC'])))

#predict
pr = predict(Env, res@models[[best]])

#Plot model prediction
plot(pr)

#returns a matrix with clusters attactched
assign_clusters <- function(df, byLat, byLon) {
  #get small and big lat/lon
  lon_vec <- df$lon
  lat_vec <- df$lat

  small_lat <- 100000
  big_lat <- -100000
  small_lon <- 100000
  big_lon <- -100000

  for (i in 1:length(df$lon)) {
    if (lat_vec[i] < small_lat)
      small_lat = lat_vec[i]
    if (lat_vec[i] > big_lat)
      big_lat = lat_vec[i]
    if (lon_vec[i] > big_lon)
      big_lon = lon_vec[i]
    if (lon_vec[i] < small_lon)
      small_lon = lon_vec[i]
  }

  small_lat <- small_lat %/% 10 * 10
  big_lat <- big_lat %/% 10 * 10 + 10
  small_lon <- small_lon %/% 10 * 10
  big_lon <- big_lon %/% 10 * 10 + 10

  #print(small_lat)
  #print(big_lat)
  #print(small_lon)
  #print(big_lon)

  # create data structure to hold spatial blocks
  num_needed <- ((big_lat-small_lat) %/% byLat) * ((big_lon-small_lon) %/% byLon)

  spatial_blocks <- list()
  index = 0
  for (i in seq(from=small_lat, to=big_lat, by=byLat)){
    for (g in seq(from=small_lon, to=big_lon, by=byLon)){
      #print(paste(i, " " ,(i+ byLat), " ", g, " ", (g + byLon)))
      sort <- df[df$lon >= g, ]
      sort <- sort[sort$lon < (g+byLon), ]
      sort <- sort[sort$lat >= (i), ]
      sort <- sort[sort$lat < (i + byLat), ]
      #print(sort)
      spatial_blocks <-append(spatial_blocks, sort, after=length(spatial_blocks))
      #print(sort)
      index = index + 1
    }
  }

  return(spatial_blocks)
  # assign points to sptial blocks

  # run convex hulls and save how many of original points were in each hull

  # apply genetic algorithm

  # repeat above steps
}

