\name{ECGr}
\alias{ECGr}
\title{
Creates an ECGr object
}
\description{
Builds an ECGr object to estimate an extrapolation of the local minimum in the response for a series of replicated observations.
}
\usage{
ECGr(data, from=min(data$x), to=max(data$x), columns, useConstantDelta=FALSE, 
	maxResponseFraction=0.5, minResponseFraction=0.05, 
	byResponseFraction=-0.05, fixedResponseFraction=0.5, 
	useFixedResponseFraction = FALSE, replaceOutliers = TRUE, 
	responseLowerLimit = min(data[, columns]), 
	responseUpperLimit = max(data[, columns]),
	alpha=0.05, kp=if(length(columns)<=1) qnorm(1-alpha/2) else 
		qt(1-alpha/2, length(columns)-1), 
	signifDigits = 2, useRobustStatistics=TRUE, ...)
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
  \item{useConstantDelta}{
  a logical value, if true then it uses the mean value of the differences in x,
  otherwise, it uses the differences in x to estimate the expected value.
in the analysis.
}
  \item{maxResponseFraction}{
  a real value with the fraction (0,1) of the maximum height to be considered 
in the analysis.
}
  \item{minResponseFraction}{
  a real value with the fraction (0,1) of the minimum height to be considered 
in the analysis.
}
  \item{byResponseFraction}{
  a real value with the fraction (0,1) of the decrement of height to be 
considered in the analysis. The extrapolation analysis uses the sequence:
maxResponseFraction, maxResponseFraction+byResponseFraction, ..., 
minResponseFraction
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
	\code{alpha} and assumes normal distribution of the error terms, its 
	default value is qnorm(1-\code{alpha}/2).
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
  \item{input}{list, contains the input parameters
  }
  \item{frame}{list, contains the reference values of the analysis. This 
  information is used to build a verbosed version of its plot. The content of 
  the list is:\cr
  \code{y} average of the response series. Depending on the \code{useRobustStatistics} value the average can be the mean or the median of the series indicated in the \code{columns} parameter.
  \code{u.y} average uncertainty of the response series. Depending on the \code{useRobustStatistics} value the average can be the standard deviation or the median absolute deviation of the series indicated in the \code{columns} parameter.
  \code{kp} the updated coverage factor considering the reduced degrees of freedom from using the model used. 

  \code{x.summary} the estimated location from the average of the series.\cr 
  \code{u.x.summary} the estimated uncertainty associated to the estimated location from the average of the series.\cr 

  \code{details} the matrix containing the results for considered fractions in the analysis. The columns are:\cr
the estimated location, the estimated uncertainty, the minimum response value found,\cr
the minimum value of the location estimates, the maximum value of the location estimates,\cr
the estimated coverage factor.\cr
  \code{used.data.points} the number of data points used in the estimations.\cr
  }
}

\author{
H. Gasca-Aragon
}
\seealso{
See Also as \code{\link{ECGdata}}, \code{\link{print.ECGr}}, \code{\link{plot.ECGr}}
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
ECGres<- ECGr(dat, columns=c(2,3))
ECGres
}
\keyword{ ECGr }
\keyword{ ECGdata }
