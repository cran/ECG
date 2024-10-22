\name{print.CGdata}
\alias{print.CGdata}
\title{
Displays the content of a CGdata object
}
\description{
Displays the content of a CGdata object.
}
\usage{
\method{print}{CGdata}(x, signifDigits=x$input$signifDigits, alpha=x$input$alpha, verbose=FALSE, ...)
}
\arguments{
  \item{x}{
	a CGdata object.
}
  \item{signifDigits}{
	a numeric value for the number of significant digits.
}
  \item{alpha}{
	a probability value taken as the significance level for building a 
	symmetric confidence interval assuming a t distribution for \cr
	\code{x$frame$used.data.points}-1 degrees of freedom.
}
  \item{verbose}{
	a boolean, if FALSE the description of the form \cr
	(value, expanded uncertainty) is provided.
	if TRUE the description is complemented providing:\cr
	the estimated coverage factor,\cr
	the estimated degrees of freedom,\cr
	the estimated coverage probability.\cr
}
\item{\dots}{additional parameters}
}

\value{No return value, called for displaying purposes/side effect.}

\author{
H. Gasca-Aragon
}

\seealso{
objects to See Also as \code{\link{CGdata}}, 
}
\examples{
require(ECG)

N<- 1000
set.seed(12345)
d1<- 1-sin(seq(1:(5/2*N))/N*pi-pi*3/4)+rnorm(5/2*N, 0, 0.01)
dat<- data.frame(x=1:length(d1), 
	y=100*(d1-min(d1))/(max(d1)-min(d1)))

CGres<- CGdata(dat)
CGres
print(CGres)
}
\keyword{ CGdata }
\keyword{ print }
