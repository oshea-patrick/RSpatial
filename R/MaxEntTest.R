# SDM Example Script
library(raster)
library(ENMeval)
require(spocc)
library(rgbif)
library(fields)
library(spam)
library(knitr)
library( spThin )
library(tictoc)
#testing12345
# Get climate data
clim = stack(list.files('/usr/share/data/wc2.0/bio2.5', pattern="*.tif", full.names = T))
ext = extent(c(-140, -60, 25, 65))
Env = crop(clim, ext)

# Download species distribution
#occ = occ('Ambystoma opacum', from = 'gbif', limit=10000)
occ = occ('Ursus americanus', from = 'gbif', limit = 20000)
occdf = occ2df(occ)
occdf <- na.omit(occdf)
loc = cbind(occdf$longitude, occdf$latitude)
loc = loc[loc[,1]<= -60,]
loc = na.omit(loc)
colnames(loc) <- c('longitude', 'latitude')

#thinning
p = proc.time()
test <- poThin(df = as.data.frame(loc) ,spacing = 100,dimension = 7091,lon = "longitude", lat = 'latitude')
proc.time() - p

loc <- loc[-test,]
loc <- as.data.frame(loc)
matrix_2d <- rdist.earth(
  matrix(c(loc$lon, loc$lat), nrow=length((loc$lon)), ncol=2),
  matrix(c(loc$lon, loc$lat), nrow=length((loc$lon)), ncol=2),
  miles = FALSE,
  R = NULL)

#ENMeval

#Sets up background sampling
xcoors = seq(ext[1],ext[2], by = 0.1)
ycoors = seq(ext[3], ext[4], by = 0.1)
backgroundXCoors = sample(xcoors, 10000, replace = T, prob = NULL)
backgroundYCoors = sample(ycoors, 10000, replace = T, prob = NULL)
background = cbind(backgroundXCoors, backgroundYCoors)
colnames(loc) <-c('lon', 'lat')

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
points(loc, pch=20, cex=0.2)


# do_sdm_function

do_sdm_test <- function(extr,
                        clim,
                        bg.ext,
                        whichbest = 'AICc',
                        nclus = 4,
                        method ='block',
                        parallel=FALSE){
  require('ENMeval')
  bio = clim;
  set.ex = extr
  #bg.ext = rad_bg(set.ex[,4:3], bio, radius=200, n = 20) # Try larger radius
  #set.eval = ENMevaluate(set.ex[,4:3], bio, n.bg=10000, rasterPreds=TRUE, parallel=TRUE, numCores = nclus, method = 'block')
  in.fc = c("L", "Q", "H", "P", "T")
  fc = vector();
  for (i in 1:length(in.fc)){
    comb = combn(in.fc, i)
    fc1 = do.call(paste, as.data.frame(t(comb), stringsAsFactors=FALSE));
    fc = c(fc, fc1)
  }

  fc = gsub(" ", "", fc)
  fc = fc[grep("[LQ]", fc)]
  set.eval = ENMevaluate(set.ex[,4:3],
                         bio,
                         bg.coords=bg.ext[,4:3],
                         rasterPreds=TRUE,
                         parallel=parallel,
                         fc = fc,
                         #fc = fc[1:3],
                         numCores = nclus,
                         method = method,

                         clamp =TRUE,
                         RMvalues=c(0.5,1,1.5,2,2.5,3,3.5,4)
                         #RMvalues = c(0.5, 2)
  );
  set.eval@results
  #return(set.eval@results)
  if(whichbest == 'CV.AUC'){
    cv.AUC = set.eval@results[,'Var.AUC']/set.eval@results[,"Mean.AUC"]
    best=which(cv.AUC == min(cv.AUC));

  }
  if(whichbest == 'Mean.AUC'){
    best = which(set.eval@results[,whichbest]==max(na.omit(set.eval@results[,whichbest])))

  }
  if(whichbest == 'AICc' | whichbest=='Mean.AUC.DIFF') {
    best = which(set.eval@results[,whichbest]==min(na.omit(set.eval@results[,whichbest])))
  }
  if(whichbest == 'delta.AICc') {
    best = which(set.eval@results[,whichbest]==min(na.omit(set.eval@results[,whichbest])))
  }
  if(whichbest == 'om.rt') {
    print("Optimizing ommission rate and predictive power...")
    setsort = set.eval@results[order(set.eval@results[,'Mean.ORmin']),]
    setsort2 = setsort[order(setsort[,'Mean.AUC'], decreasing=TRUE),]
    top = setsort2[1,]
    best = which(as.character(set.eval@results[,1]) == as.character(setsort2[1,1]))
  }
  #return(list(best, setsort2, set.eval))
  if(whichbest == 'binaryAUC'){
    print("USING binaryAUC as optimality criteria")
    keep = vector()
    for(i in 1:nlayers(set.eval@predictions)){
      pr = set.eval@predictions[[i]]
      occ <- na.omit(extract(pr, set.ex[,4:3]));
      bg <- na.omit(extract(pr, bg.ext[,4:3]));
      ev <- evaluate(as.numeric(occ>=min(occ)), as.numeric(bg>=min(occ)))
      th = threshold(ev)
      p = pr >= th$no_omission
      ex = extract(p, set.ex[,4:3])
      ex.bg = extract(p, bg.ext[,4:3])
      ev2 <- evaluate(ex, ex.bg)
      keep[[i]] = slot(ev2, 'auc');
    }
    best = which(keep == max(keep));
  }
  #Pick best (Lowest AICc) from results
  pr.set <- predict(set.eval@models[[best]], bio)
  #plot(pr.set)
  ev.set <- evaluate(set.ex[,4:3], set.eval@bg.pts, set.eval@models[[best]], bio)
  thr.set <- threshold(ev.set)

  return(list(pr.set, ev.set, thr.set, set.eval, best));
}

thin_v1 <- function(df, radius, numUniques) {
  #numeric array initialized to 0 to hold the number of points within scanning distance of each point
  nearby <- rep(0, 1, length(df$longitude))

  #Sort dataframe by longitude(x), hopefully done in nlogn
  df <-df[order(df$longitude),]

  # double nested loop to populate number of points with the radius:dist of each point
  for (i in 1:length(df$longitude)) {
    j = i+1
    while (j < length(df$longitude)) {
      if (rdist.earth(
        matrix(c(df$longitude[i], df$latitude[i]), nrow=1, ncol=2),
        matrix(c(df$longitude[j], df$latitude[j]), nrow=1, ncol=2),
        miles = FALSE,
        R = NULL) <= radius) {
        nearby[i]=nearby[i]+1
        nearby[j]=nearby[j]+1
      }
      else {
        i=j;
        break;
      }
      j=j+1
    }
    #end while
  }
  #end for

  #combine the data and return a new dataframe
  out <- cbind(df,nearby)
  #nearby <-nearby[order(nearby$nearby),]

  #get mean of nearby column for data saving purposes
  mean = 0
  for (i in 1:length(out$nearby))
    mean = mean + out$nearby[i]
  mean = mean*1.0 / length(out$nearby)

  #begin cleaning up data
  savedByMean = which(out$nearby <= numUniques)
  out <- out[savedByMean,]

  print("Completed")
  return(out)
}

thin_v2 <- function(df, radius) {
  #keeps track of what will be deleted from the points
  to_be_deleted <- c()

  #Sort dataframe by longitude(x), hopefully done in nlogn
  df <-df[order(df$longitude),]

  # double nested loop to populate number of points with the radius:dist of each point
  for (i in 1:length(df$longitude)) {
    j = i+1
    while (j < length(df$longitude)) {
      if (rdist.earth(
        matrix(c(df$longitude[i], df$latitude[i]), nrow=1, ncol=2),
        matrix(c(df$longitude[j], df$latitude[j]), nrow=1, ncol=2),
        miles = FALSE,
        R = NULL) <= radius) {
        to_be_deleted <- append(x = to_be_deleted, values = j, after = length(to_be_deleted))
      }
      else {
        i=j;
        break;
      }
      j=j+1
    }
    #end while
  }
  #end for

  #begin cleaning up data
  out <- df[-to_be_deleted,]

  print("Completed")
  return(out)
}

thin_v3 <- function(df, spacing) {
  tic('')
  #list of #s to be removed from df

  to_be_removed <- c()

  #creates 2d matrix
  matrix_2d <- rdist.earth(
    matrix(c(df$longitude, df$latitude), nrow=length((df$longitude)), ncol=2),
    matrix(c(df$longitude, df$latitude), nrow=length((df$longitude)), ncol=2),
    miles = FALSE,
    R = NULL)

  #sets diags to NA as a point's distance to itself is 0
  diag(matrix_2d) <- NA

  # loops through cols
  for (col in 1:length(matrix_2d[,1])) {
    # loops through row of each col
    for (row in 1: length(matrix_2d[col,])) {

      if (!is.na(matrix_2d[col,row]) && matrix_2d[col,row] > spacing) {
        #do this

        # loops through the matches that have a spacing over a certain amount
        for (match in 1:length(matrix_2d[row,])) {
          if (!is.na(matrix_2d[row, match]) && matrix_2d[row,match] <= spacing)  {
            #set row and col of the original match to NA
            matrix_2d[row,] <- NA
            matrix_2d[, row] <- NA

            #save the number that will not be included in df anymore
            #toDo create a vector or matrix
            to_be_removed <- append(to_be_removed, row)
            break
          }
        }

      }
    }
  }
  print(to_be_removed)
  print(paste("Finished Thinning in ", toc() , " and removed ", length(to_be_removed), " occurences."))
  return (df[-to_be_removed,])

}

