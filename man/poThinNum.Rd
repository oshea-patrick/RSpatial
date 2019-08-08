\name{poThinNum}
\alias{poThinNum}
poThinNum
\title{
poThinNum
}
\description{
Thins a dataframe to get close to a desired number of records rather than using a spacing parameter.
}
\usage{
poThinNum(df, lon, lat, num, maxRounds)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
  This is a DataFrame Object with col names denoted by the variables "lon" and "lat".
}
\item{lon}{
This is the name of the column that contains all of the longitude data.
}
\item{lat}{
This is the name of the column that contains all of the latitude data.
}
  \item{num}{
  This is the Number of desired records after thinning.
}
  \item{maxRounds}{
  This is the maximum number of repitions the algorithm will run for. (This uses a while loop so as to prevent an infinite loop and save time, this parameter was created)
}
}
\details{
}
\value{
This returns a thinned DataFrame.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Patrick O'Shea
}
\note{
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
