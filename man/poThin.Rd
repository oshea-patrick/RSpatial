\name{poThin}
\alias{poThin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
poThin
}
\description{
This Algorithm spatially thins a dataset by the parameter, spacing. After the thinning, all the points will be at least spacing apart. This algorthm is O(n^3).

The algorithm is as follows:

1) Create a distance matrix between all the points

2) Initialize diagonals to -1 (-1 will be used in the algorithm as an unecessary comaprison and thus for pruning)

3) For each point, i.e. each row in the matrix, see if any point is less than the spacing parameter in realation to said point by looking down the column of that point

---a) If no point is found, do nothing
  
---b) If a point if found, do (4).
  
4) For each point in the row of the point that was less than the spacing parameter, see if any other point exists that is greater than the spacing parameter

---a) If no point is found then do nothing (There are not a lot of point nearby this point and thus it may be important)
  
---b) Otherwise set all indices in the row and column of this point to -1 and save the index of the point so it can be removed

}
\usage{
poThin(df, spacing, dimension, "lon", "lat")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
This is a DataFrame Object with col names denoted by the variables "lon" and "lat".
}
\item{spacing}{
This is the spacing that all points should be apart (in kms).
}
\item{dimensiom}{
The number of records that the thinning algorithm will be run on.
}
\item{lon}{
This is the name of the column that contains all of the longitude data.
}
\item{lat}{
This is the name of the column that contains all of the latitude data.
}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
This returns an array containing the indices of the DataFrame, df, that need to be deleted for it to be spatially thinned.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Patrick J. O'Shea
}
\note{
This function is written in C++.

Large (> 1,000,000 occ) datasets may have some RAM issues.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# creating data
lon <- seq(1,100, by = .1)
lat <- seq(1,100, by = .1)
loc <- cbind(lon, lat)
loc <- as.data.frame(loc)
# Names the data so it's easier to see
colnames(loc) <- c('lon', 'lat')
# Corrects data format
loc <- as.data.frame(loc)

# Gets Indices to be Thinned
thinned <-
  poThin(
    df = loc,
    spacing = 50,
    dimension = length(loc$lon),
    lon = "lon",
    lat = "lat"
  )
# Removes Indices Resulting in Thinned Data
loc <- loc[-thinned,]
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
