\name{classifySpatiallyByBlocks}
\alias{classifySpatiallyByBlocks}
\title{
classifySpatiallyByBlocks
}
\description{
This function takes a dataframe and classifies points into spatial blocks.
}
\usage{
classifySpatiallyByBlocks(df, lon, lat, size, blockSize)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
This is a dataframe consisting of two columns, lon and lat, corresponding to latitude and longitude.
}
  \item{lon}{
The name of the longitude column in df.
}
  \item{lat}{
The name of the latitude column in df.
}
  \item{size}{
The size of df.
}
  \item{blockSize}{
The size that each block should be in degrees of latitude by longitude.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
This returns a Numeric Vector consiting of which block each point in df was classifed into.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Patrick J. O'Shea
}
\note{
This is written in C++.
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
