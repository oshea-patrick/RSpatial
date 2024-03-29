\name{poThinP}
\alias{poThinP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
poThinP
}
\description{
This Algorithm spatially thins a dataset by the parameter, spacing. After the thinning, all the points will be at least spacing apart. This algorthm is O(n^3). This is the parallel version of poThin.
}
\usage{
poThin(df, spacing, "lon", "lat", 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
This is a DataFrame Object with col names denoted by the variables "lon" and "lat".
}
\item{spacing}{
This is the spacing that all points should be apart (in kms).
}
\item{lon}{
This is the name of the column that contains all of the longitude data.
}
\item{lat}{
This is the name of the column that contains all of the latitude data.
}
\item{numCores}{
The Number of cores the function may use.
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
This function uses both R and C++.
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
loc <- poThinP(loc, 50, "lon", "lat", 2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
