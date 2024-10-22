\name{ECGdata}
\alias{ECGdata}
\title{
Creates a ECGdata object
}
\description{
Builds a ECGdata object to estimate an extrapolation of the local minimum 
in the response for a series of single observations.
}
\usage{
ECGdata(data, from=min(data$x), to=max(data$x), useConstantDelta = FALSE, 
maxResponseFraction = 0.5, minResponseFraction = 0.05, 
byResponseFraction = -0.05, fixedResponseFraction = 0.5, 
useFixedResponseFraction = FALSE, replaceOutliers = TRUE, 
responseLowerLimit=min(data$y), responseUpperLimit=max(data$y),
 alpha = 0.05,  kp = qnorm(1-alpha/2), signifDigits = 2,  
 useRobustStatistics = TRUE, ...)
}
\arguments{
  \item{data}{
  a data frame structure containing (x, y) columns.
}
  \item{from}{
  a numeric value with the initial value of x to search for a local minimum.
}
  \item{to}{
  a numeric value with the final value of x to search for a local minimum.
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
\details{
The data at each step is a subset of the previous step hence the estimates are 
correlated. However by specifying useFixedResponseFraction=FALSE they are 
normalized against distinct fraction of the height.
}
\value{
  \item{x}{numeric, the estimated value}
  \item{u}{numeric, the estimated uncertainty associated to x}
  \item{input}{list, contains the input parameters
  }
  \item{frame}{list, contains the reference values of the analysis. This 
  information is used to build a verbosed version of its plot. The content of 
  the list is:\cr
  \code{kp} the updated coverage factor considering the reduced degrees of freedom from using the model used. 
  \code{y.x.band.min} the local maximum found in the lower region of the analysis 
  region. \cr
  \code{x.max} the value of \code{x} where the local maximum \code{y.x.max} occurs.\cr
  \code{x.min} the value of \code{x} where the local maximum \code{y.x.min} occurs.\cr
  \code{solutions} a matrix with the solutions found for each analyzed fraction of the data.\cr
The contained columns are:\cr the location estimate,\cr the standard uncertainty, \cr
the response fraction used,\cr the value of the minimum response in the considered band, \cr
the maximum response in the considered band, and \cr the number of data points used.\cr
  \code{type} an integer representing the degree of the polynomial model (0=constant model, 1=polynomial model of first degree, 2=polynomial model of second degree).\cr
  \code{model} a linear model summary object.\cr
  }
}

\author{
H. Gasca-Aragon
}

\seealso{
See Also as \code{\link{CGdata}}, \code{\link{print.ECGdata}}, \code{\link{plot.ECGdata}}
}
\examples{
require(ECG)
N<- 1000
set.seed(12345)
d1<- 1-sin(seq(1:(5/2*N))/N*pi-pi*3/4)+rnorm(5/2*N, 0, 0.01)
dat<- data.frame(x=1:length(d1), 
	y=100*(d1-min(d1))/(max(d1)-min(d1)))

ECGdata(dat)
ECGdata
}
\keyword{ ECGdata }
