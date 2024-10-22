\name{CGr}
\alias{CGr}
\title{
Creates a CGr object
}
\description{
Builds a CGr (center of gravity) object for a series of replicated observations.
}
\usage{
CGr(data, from=min(data$x), to=max(data$x), columns, 	responseFraction = 0.50, 
useConstantDelta=FALSE, fixedResponseFraction=0.5, useFixedResponseFraction=FALSE, 
replaceOutliers=TRUE, responseLowerLimit=min(data[, columns]), 
responseUpperLimit=max(data[, columns]), alpha=0.05, 
kp=if(length(columns)<=1) qnorm(1-alpha/2) else 
	qt(1-alpha/2, length(columns)-1), 
signifDigits=2, useRobustStatistics = TRUE, ...)
}
\arguments{
  \item{data}{
  a data frame structure containing (date, x, y1, ..., yn) columns, it may 
contain some other columns.
}
  \item{from}{
  a numeric value with the initial value of x to search for a local minimum.
}
  \item{to}{
  a numeric value with the final value of x to search for a local minimum.
}
  \item{columns}{
  a vector of indexes of the columns to be considered in the profile.
}
  \item{responseFraction}{ numeric, fraction of the maximum height to be 
considered in the analysis,
}
  \item{useConstantDelta}{boolean flag, if true constant increment in the x 
values is assumed, otherwise the difference is computed for each increment of x.
}
  \item{fixedResponseFraction}{
  a numeric with the fraction of hieght to be used as a reference to normilize.
}
  \item{useFixedResponseFraction}{
  a logical value, if true then it uses the value of f.fixed to normalize all the
 computations, otherwise it uses the values of extrapolation sequence of 
 fractions to normalize.
}
  \item{replaceOutliers}{
  a logic value, if true then it uses the value of \code{responseLowerLimit} and 
	\code{responseUpperLimit} to replace outlier values. Default value is TRUE.
}
  \item{responseLowerLimit}{
  a real value to be used as the default to replace outlier values lower than 
	expected, its default value is 0.
}
  \item{responseUpperLimit}{
  a real value to be used as the default to replace outlier values larger than 
	expected, its default value is 1.
}
  \item{alpha}{
  a real value, define the level of significance for building confidence interval.
}
  \item{kp}{
  a real value, it defines the coverage factor to be used to estimate the 
	expanded uncertainty. It is build based on the level of significance 
	\code{alpha} and assumes a T distribution of the error terms with the 
	degrees of freedom equals to the number of \code{columns} provided 
	minus one, its default value is qnorm(1-\code{alpha}/2) for one column 
	otherwise qt(1-\code{alpha}/2, length(\code{columns})-1).
}
  \item{signifDigits}{
  number of significant digits used to display the result.
}
  \item{useRobustStatistics}{
  a logical value, if true then median and mad are used to estimate location and dispersion otherwise the mean and standard deviation are used.
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

  \item{input}{list, contains the current input parameters, including the default values
	additional parameters passed through \code{\dots} are not included.
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
d2<- 1-sin(seq(1:(5/2*N))/N*pi-pi*3/4)+rnorm(5/2*N, 0, 0.01)
dat<- data.frame(x=1:length(d1), 
	y1=100*(d1-min(d1))/(max(d1)-min(d1)),
	y2=100*(d2-min(d2))/(max(d2)-min(d2))
)

CGres<- CGr(dat, columns=c(2,3))
CGres
}
\keyword{ CGdata }
\keyword{ CGr }
