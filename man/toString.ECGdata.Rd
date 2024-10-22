\name{toString.ECGdata}
\alias{toString.ECGdata}
\title{
Converts a ECGdata object into a string description
}
\description{
Converts a ECGdata object into a string description for displaying purposes.
}
\usage{
\method{toString}{ECGdata}(x, signifDigits=x$input$signifDigits, alpha=x$input$alpha, verbose=FALSE, ...)
}
\arguments{
  \item{x}{
	an ECGdata object
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

\value{
	a string description of the ECGdata object.
}

\author{
H. Gasca-Aragon
}

\examples{
require(ECG)
X.ref<- list(x=pi/2, u=0)
N<- 1000
d0<- 1-sin(seq(1:(5/2*N))/N*pi-pi*3/4)

set.seed(12345)
d1<- d0+rnorm(5/2*N, 0, 0.01)
dat<- data.frame(x=1:length(d1), 
	y=100*(d1-min(d1))/(max(d1)-min(d1)))

ECGres<- ECGdata(dat)
str.res <- toString(ECGres)
print(str.res)
}
\keyword{ toString }
\keyword{ ECGdata }