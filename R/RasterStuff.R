makeHulls <- function(listOfDataFrames) {
  listOut <- list()
  index =1 
  for (i in 1:length(listOfDataFrames)) {
    temp <- listOfDataFrames[[index]]
    listOut[[index]] <- temp[chull(temp) , ]
    index = index + 1
  }
  return(listOut)
}

# takes in dataframe with lon, lat, and blocks
# returns AVN numbers without summation and division based on clusters in spatial blocks
makeAVN <- function(points) {
  
  avns <- c()
  for (i in 1:length(points$lon)) {
    avns[i] = length(which(points$blocks == points$blocks[i]))
  }
  return(avns)
}

refineAVN <- function(points, AVNs) {
  points <- cbind(points, AVNs)
  points <- points[order(AVNs), ]
  total <- 0
  for (i in 1:length(points$AVNs))
    total <- total + points$AVNs[i]
  for (i in 2:length(points$AVNs))
    points$AVNs[i] = (points$AVNs[i]+points$AVNs[i-1]*1.0)
  for (i in 1:length(points$AVNs))
    points$AVNs[i] = (points$AVNs[i]*1.0)/(total*1.0)
  return(points)
}

# takes in a dataframe of points with lon lat and blocks and returns the points with AVN values
sortByAVN <- function(points) {
  points <- refineAVN(points, makeAVN(points))
  return(points)
}

fixPoints <- function(points, degree) {
  for (i in 1:length(points$lon)) {
    points$lon[i] = roundT(points$lon[i], degree)
    points$lat[i] = roundT(points$lat[i], degree)
  }
  return(points)
}

#returns a thinned dataFrame
#' @export
#' @example
#' x=1
simulateDistribution <- function(df, gen, degree, block, adjust, vert, hori, rastIn) {
  #prep for the loop
  
  rast = raster(nrows=( rastIn@extent@xmax-rastIn@extent@xmin)*(10^degree), ncols=(rastIn@extent@ymax-  rastIn@extent@ymin)*(10^degree), xmn= rastIn@extent@xmin, xmx = rastIn@extent@xmax, ymn =rastIn@extent@ymin, ymx =   rastIn@extent@ymax)
  list <- data.frame()
  for (i in 1:gen) {
    spatBlocks <- (stats::rnorm(1, mean=7, sd=3))
    df <- df[, 1:2]
    df <- as.data.frame(df)
    blocks <- RSpatial::classifySpatiallyByBlocks(df = df, lon = "lon", lat = "lat", size = length(df$lon), blockSize = spatBlocks)
    df <- cbind(df, blocks)
    # testing is simply loc with AVN values attatched
    testing <-sortByAVN(df)
    # generate a new sample/genration
    sampleIndices <- sample(x = 1:length(df$lat), size = length(df$lon)*0.9, replace = FALSE, prob = NULL)
    sampledPoints <- df[sampleIndices, ]
    newGen <- RSpatial::createNextGen(df = cbind(testing["lon"], testing["lat"]), avnValues = as.vector(testing[["AVNs"]]), mutationRate = 50, explore = 30 , size = length(testing$lon), randoms = runif(length(testing$lon),0,100))
    blocks <- RSpatial::classifySpatiallyByBlocks(df = newGen, lon = "lon", lat = "lat", size = length(newGen$lon), blockSize = spatBlocks)
    newGen <- cbind(newGen, blocks)
    sampleNewGenIndices <- sample(x = 1:length(newGen$lat), size = length(testing$lon) - length(sampleIndices), replace = FALSE, prob = NULL)
    newGen <- newGen[sampleNewGenIndices, ]
    sampledPoints <- sampledPoints[,1:3]
    colnames(sampledPoints) <- c("lon", "lat", "blocks")
    newGen <- rbind(sampledPoints, newGen)
    list <- rbind(list, newGen)
  }
  if (!adjust)
    tempVec <- rasterMaker(list, rast@extent@xmin, rast@extent@ymax, rast@ncols, rast@nrows, length(newGen$lon), gen, degree, block)
  else
    tempVec <- rasterMakerAdjust(list, rast@extent@xmin, rast@extent@ymax, rast@ncols, rast@nrows, length(newGen$lon), gen, degree, block, hori, vert)
  
  rast <- setValues(rast, tempVec)
  return(rast)
}

getValue <- function(rast, x, y, degree) {
  return(rast[((rast@extent@ymax - roundT(y, degree)) * (rast@ncols * (10^degree))) + (abs((roundT(x, degree)) - rast@extent@xmin) * (10^degree)) + 1])
}

appendValues <- function(df, rast) {
  rastOut <- simulateDistribution(df, 500, 0, TRUE, FALSE, 0, 0, rast)
  values <- rep(0, length(df$lon))
  for (i in 1:length(values))
    values[i] <- getValue(rastOut, df$lon[i], df$lat[i], 0)
  return(as.data.frame(cbind(df, values)))
}