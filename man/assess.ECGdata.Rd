\name{assess.ECGdata}
\alias{assess.ECGdata}
\title{
Evaluates the ECGdata object estimation
}
\description{
Evaluates the ECGdata object estimation against the reference value by the En index.
}
\usage{
\method{assess}{ECGdata}(x, y, x.B = list(u=0, dof=Inf), alpha=x$input$alpha)
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
	the numeric value of the En index evaluation.
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

ECGres<- ECGdata(dat)

X.ref <- list(x=1250, u=0, dof=Inf)
assess(ECGres, y=X.ref)

}
\keyword{ En performance index }
\keyword{ ECGdata }
