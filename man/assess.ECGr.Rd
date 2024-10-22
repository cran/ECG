\name{assess.ECGr}
\alias{assess.ECGr}
\title{
Evaluates the ECGr object estimation
}
\description{
Evaluates the ECGr object estimation against a reference value by the En index.
}
\usage{
\method{assess}{ECGr}(x, y, x.B = list(u=0, dof=Inf), alpha=x$input$alpha)
}
\arguments{
  \item{x}{
	a Estimator object
}
  \item{y}{
	a list with the reference value (x, u, dof)
}
  \item{x.B}{
	a list with the estimate of uncertainty type B for the x object and its degrees of freedom
}
\item{alpha}{
numeric, the level of significance used to compute the coverage factor and the expanded uncertainty
}
}
\value{
  the numeric value of the En index evaluation. This is a normalized error value with expanded uncertainty equals 1.0.
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
d2<- d0+rnorm(5/2*N, 0, 0.01)
dat<- data.frame(x=1:length(d1), 
	y1=100*(d1-min(d1))/(max(d1)-min(d1)),
	y2=100*(d2-min(d2))/(max(d2)-min(d2)))
ECGres<- ECGr(dat, min(dat$x), max(dat$x), columns=c(2,3),
	responseLowerLimit=0, responseUpperLimit=100)
assess(ECGres, X.ref)
}
\keyword{ En performance index }
\keyword{ ECGr }
