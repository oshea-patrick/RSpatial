\name{poThin}
\alias{poThin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
poThin
}
\description{
This Algorithm spatially thins a dataset by the parameter, spacing. After the thinning, all the points will be at least spacing apart. This algorthm is O(n^3).
}
\usage{
poThin(df, 100, 5000, "lon", "lat")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
This is a DataFrame Object with col names denoted by the variables "lon" and "lat".
}
\item{spacing}{
This is the spacing that all points should be apart (in kms).
}
\item{records}{
The number of records that the thinning algorithm will be run on.
}
\item{longitude_col_name}{
This is the name of the column that contains all of the longitude data.
}
\item{latitude_col_name}{
This is the name of the column that contains all of the latitude data.
}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
This returns a DataFrame object that has been thinned.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Patrick J. O'Shea
}
\note{
This function is written in C++.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
