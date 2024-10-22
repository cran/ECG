\name{plot.ECGdata}
\alias{plot.ECGdata}
\title{
Plots an ECGdata object
}
\description{
Creates a plot for a series of single observations with the extrapolated center of gravity method.
}
\usage{
\method{plot}{ECGdata}(x, xlim=range(x$frame$solution[,1]), ylim=c(0, max(x$frame$solution[,5])), 
 xlab=expression(nu[i]), ylab=expression(f[i]), add=TRUE, ...)
}
\arguments{
  \item{x}{
	an ECGdata object
}
  \item{xlim}{
	a range with the x limits for plotting purposes
}
  \item{ylim}{
	a range with the y limits for plotting purposes
}
  \item{xlab}{
	a string or expression with the label for the x axis
}
  \item{ylab}{
	a string or expression with the label for the y axis
}
  \item{add}{
	a logical value, if true then the plot is added to an existing one, 
	otherwise it creates a new plot.
}
  \item{\dots}{
	additional parameters
}
}
\details{
	It plots the sequence of approximations for each fraction of the dataset,
	and it shows the estimated value while extrapolated at zero fraction.
}
\value{
No return value.
}

\author{
H. Gasca-Aragon
}

\seealso{
 See Also as \code{\link{ECGdata}}, \code{\link{print.ECGdata}}, \code{\link{plot.ECGdata}}
}
\examples{
require(ECG)
N<- 1000
set.seed(12345)
d1<- 1-sin(seq(1:(5/2*N))/N*pi-pi*3/4)+rnorm(5/2*N, 0, 0.01)
dat<- data.frame(x=1:length(d1), 
	y=100*(d1-min(d1))/(max(d1)-min(d1)))

CGres <- CGdata(dat)
ECGres<- ECGdata(dat)


\donttest{
plot(CGres, min(dat$x), max(dat$x))
plot(ECGres, add=FALSE)
}
}
\keyword{ ECGdata }
