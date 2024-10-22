\name{CGdata}
\alias{CGdata}
\title{
Creates a CGdata object
}
\description{
Builds a CGdata (center of gravity) object for a series of single observations.
}
\usage{
CGdata(data, from=min(data$x), to=max(data$x), responseFraction = 0.5, 
	useConstantDelta = FALSE, fixedResponseFraction = 0.5, 
	useFixedResponseFraction = FALSE, replaceOutliers = TRUE, 
	responseLowerLimit = min(data$y), responseUpperLimit = max(data$y),
	alpha = 0.05, signifDigits = 2, ...)
}
\arguments{
  \item{data}{
  a data frame structure containing (x, y) columns.
}
  \item{from}{
  a numeric value with the initial value of \code{x} to search for a local 
	minimum.
}
  \item{to}{
  a numeric value with the final value of \code{x} to search for a local minimum.
}
  \item{responseFraction}{
  a real value with the fraction (0,1) of the maximum height to be considered 
in the analysis.
}
  \item{useConstantDelta}{
  a logic value, if true then it assumes the values of \code{x} increments at 
	constant rate, otherwise it computes each increment.
}
  \item{fixedResponseFraction}{
  a numeric with the fraction of height to be used as a reference to normilize, 
	default value is 0.5.
}
  \item{useFixedResponseFraction}{
  a logic value, if TRUE then it uses the value of \code{fixedResponseFraction} 
	to normalize all the computations, otherwise it uses the value of 
	\code{responseFraction} to normalize, default value is TRUE.
}
  \item{replaceOutliers}{
  a logic value, if true then it uses the value of \code{responseLowerLimit} and 
	\code{responseUpperLimit} to replace outlier values. Default value is TRUE.
}
  \item{responseLowerLimit}{
  a real value to be used as the default to replace outlier values lower than 
	expected.
}
  \item{responseUpperLimit}{
  a real value to be used as the default to replace outlier values larger than 
	expected.
}
  \item{alpha}{
  a real value, it defines the level of error type I used to estimate the 
	coverage factor \code{kp} and the expanded uncertainty, its default value
	is 0.05.
}
  \item{signifDigits}{
  an integer value, it defines the number of significant digits to be used for 
	displaying the result and its uncertainty, default value is 2.
}
  \item{\dots}{
  additional parameters.
}
}

\value{
  \item{x}{numeric, the estimated value}
  \item{u}{numeric, the estimated uncertainty associated to x}
  \item{moments}{numeric vector, the estimated mean, variance, skweness and 
	kurtosis}
  \item{input}{list, contains the input parameters
  }
  \item{frame}{list, contains the reference values of the analysis. This 
  information is used to build a verbosed version of its plot. The content of 
  the list is:
  \code{y.x.band.min} the local maximum found in the lower region of the analysis 
  region. \cr
  \code{y.x.band.max} the local maximum found it the upper region of the analysis 
  region. \cr
  \code{x.min.y} the value of \code{x} where the local minumum \code{y} occurrs.\cr
  \code{x.max} the value of \code{x} where the local maximum \code{y.x.max} occurs.\cr
  \code{x.min} the value of \code{x} where the locel maximum \code{y.x.min} occurs.\cr
  \code{y.x.max} the maximum height in the upper region of the analysis.\cr
  \code{y.x.min} the maximum height in the lower region of the analysis.\cr
  \code{h} the value of the index of \code{x} associated with \code{f.i} 
  fraction of the data in the lower region of analysis.\cr
  \code{k} the value of the index of \code{x} associated with \code{f.i} 
  fraction of the data in the upper region of analysis.\cr
  \code{x.h} the value of \code{x} associated with \code{f.i} fraction 
  of the data in the lower region of analysis.\cr
  \code{x.k} the value of \code{x} associated with \code{f.i} fraction
  of the data in the upper region of analysis.\cr

  \code{used.data.points} the number of datapoints of \code{x} used to obtain the 
  estimates, this is equal to k-h+1.\cr
  }
}

\author{
H. Gasca-Aragon
}
\examples{
require(ECG)

N<- 1000
set.seed(12345)
d1<- 1-sin(seq(1:(5/2*N))/N*pi-pi*3/4)+rnorm(5/2*N, 0, 0.01)
dat<- data.frame(x=1:length(d1), 
	y=100*(d1-min(d1))/(max(d1)-min(d1)))

CGres <- CGdata(dat)
CGres
}
\keyword{ CGdata }
