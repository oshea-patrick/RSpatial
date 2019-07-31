\name{isInsideConvexHull}
\alias{isInsideConvexHull}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
isInsideConvexHull
}
\description{
Checks to see if a Point resides within a convex hull, which is presumably in clockwise order.
}
\usage{
isInsideConvexHull(hulls, checkx, checky, size)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{hulls}{
  This is a dataframe consisting of two columns, "lon" and "lat", corresponding to latitude and longitude.
}
  \item{checkx}{
  The x coordinate of the point being checked.
}
  \item{checky}{
  The y coordinate of the point being checked.
}
  \item{size}{
  The size of the dataframe, hulls.
}
}
\details{
Written in C++.
}
\value{
Returns a boolean value, TRUE or FALSE.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Patrick J. O'Shea
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x = 1
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
