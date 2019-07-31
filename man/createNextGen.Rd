\name{createNextGen}
\alias{createNextGen}
createNextGen
\title{
createNextGen
}
\description{
Simulates a new generation of points based on a current dataFrame of points and AVN values associated with them.
}
\usage{
createNextGen(df, avnValues, mutationRate, explore, size, randoms)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
This is a dataframe consisting of two columns, "lon" and "lat", corresponding to latitude and longitude.
}
  \item{avnValues}{
This is a Numeric Vector of AVN values that correspond to the points in df.
}
  \item{mutationRate}{
This is the mutation rate at which points with "explore". If mutated, a point will randomly be cast in a direction using the variable explore. Mutation rate is an integer from 1 to 100.
}
  \item{explore}{
Explore is the amount of kM that a point will be cast at maximum if mutated.
}
  \item{size}{
This is the size of the DataFrame, df.
}
  \item{randoms}{
This is a Numeric Vector of random numbers which are used as Seeds by srand() in C++. This must have the same length as size.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a DataFrame of points with columns "lat" and "lon".
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Patrick J. O'Shea
}
\note{
Written in C++.
}
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
