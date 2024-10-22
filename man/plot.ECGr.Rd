\name{plot.ECGr}
\alias{plot.ECGr}
\title{
Plots an ECGr object
}
\description{
Creates a plot for a series of replicated observations with the extrapolated center of gravity method.
}
\usage{
\method{plot}{ECGr}(x, add = TRUE, ...)
}
\arguments{
  \item{x}{
	an ECGr object.
}
  \item{add}{
	a logical value, if true then the plot is added to an existing one, 
	otherwise it creates a new plot.
}
  \item{\dots}{
	additional parameters.
}
}
\details{
	It plots the sequence of approximations for each fraction of the dataset,
	and it shows the estimated value while extrapolated at zero fraction.
}
\value{
No return value. Used for graphical display.
}
\author{
H. Gasca-Aragon
}

\seealso{
 See Also as \code{\link{ECGr}}, \code{\link{print.ECGr}}
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

# used alone
ECGres<- ECGr(dat, columns=c(2,3))
\donttest{plot(ECGres, add = FALSE)}

# used in colaboration with CGr
CGres<- CGr(dat, columns=c(2,3))
\donttest{
# display the Center of Gravity approach for replicated observations
plot(CGres, verbose = TRUE)
# then add the extrapolation sequence
plot(ECGres)
}
}
\keyword{ ECGr }
\keyword{ plot }
