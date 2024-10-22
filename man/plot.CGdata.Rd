\name{plot.CGdata}
\alias{plot.CGdata}
\title{
Plots a CGdata object
}
\description{
Creates a plot of a series of single observations. 
}
\usage{
\method{plot}{CGdata}(x, from=x$input$from, to=x$input$to, xlab = expression(nu(cm^-1)),
 ylab = "Transmittance", add = FALSE, verbose = FALSE, ...)
}
\arguments{
  \item{x}{
	a CGdata object
}
  \item{from}{
	lower limit point on the predictor value to be plotted
}
  \item{to}{
	upper limit point on the predictor value to be plotted
}
  \item{xlab}{
	a string or expression with the label for the x axis
}
  \item{ylab}{
	a string or expression with the label for the y axis
}
  \item{add}{
	a logical value, if true then the plot is added to an existing one,
	otherwise it creates a new plot
}
  \item{verbose}{
	a logical value, if true then the plot displays intermediate steps to
	estimate the extreme point located between the from and to values
}
  \item{\dots}{
	additional parameters
}
}

\value{No return value, called for graphical display side effects}

\author{
H. Gasca-Aragon
}

\seealso{
See Also as \code{\link{CGdata}}, \code{\link{print.CGdata}}
}
\examples{
require(ECG)

N<- 1000

set.seed(12345)
d1<- 1-sin(seq(1:(5/2*N))/N*pi-pi*3/4)+rnorm(5/2*N, 0, 0.01)
dat<- data.frame(x=1:length(d1), 
	y=100*(d1-min(d1))/(max(d1)-min(d1)))

CGres<- CGdata(dat)

\donttest{
plot(CGres, min(dat$x), max(dat$x), verbose = TRUE)
}

}
\keyword{ CGdata }
\keyword{ plot }
