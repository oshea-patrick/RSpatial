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
poThin(df, 100, "lon", "lat")
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
