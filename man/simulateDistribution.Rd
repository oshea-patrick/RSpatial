\name{simulateDistribution}
\alias{simulateDistribution}
\title{
simulateDistribution
}
\description{
This creates a raster that shows the simulated likelihood of points being found at a certain position (Points may not line up with raster).
}
\usage{
simulateDistribution(df, gen, degree, block, adjust, vert, hori, rastIn)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
This is a DataFrame Object with col names denoted by the variables "lon" and "lat".
}
  \item{gen}{
The number of generations to run the simulation for.
}
  \item{degree}{
The degree of pixels there should be, 10^degree.
}
  \item{block}{
A boolean whether or not to pad each point with a block of points.
}
  \item{adjust}{
This is a boolean used to adjust the raster if it doesn't quite line up...
}
  \item{vert}{
Vertical adjustment in pixels, positive or negative.
}
  \item{hori}{
Horizontal adjustment in pixels, positive or negative.
}
  \item{rastIn}{
A raster to model the returned raster after.
}
}
\details{

}
\value{
Returns a raster.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Patrick J. O'Shea
}
\note{
Written in R and C++.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x=1
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
