\name{plot.CGr}
\alias{plot.CGr}
\title{
Plots a CGr object
}
\description{
Creates a plot of a series of replicated data. It shows the summary of the series.
Optionally it shows the limits of the range of analysis and
the estimated local minimum computed as the center of gravity.
}
\usage{
\method{plot}{CGr}(x, ...)
}
\arguments{
  \item{x}{
	a CGr object
}
  \item{\dots}{
	additional parameters
}
}
\value{
No return value, called for graphical display.
}
\author{
H. Gasca-Aragon
}

\seealso{
See Also as \code{\link{CGr}}, \code{\link{print.CGr}}
}
\examples{
require(ECG)
N<- 1000
set.seed(12345)
d1<- 1-sin(seq(1:(5/2*N))/N*pi-pi*3/4)+rnorm(5/2*N, 0, 0.01)
d2<- 1-sin(seq(1:(5/2*N))/N*pi-pi*3/4)+rnorm(5/2*N, 0, 0.01)
dat<- data.frame(x=1:length(d1), 
	y1=100*(d1-min(d1))/(max(d1)-min(d1)),
	y2=100*(d2-min(d2))/(max(d2)-min(d2))
)
CGres<- CGr(dat, columns=c(2,3))
\donttest{plot(CGres)}
}
\keyword{ CGr }
\keyword{ plot }
