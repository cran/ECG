\name{assess.CGr}
\alias{assess.CGr}
\title{
Evaluates the CGr object estimation
}
\description{
Evaluates the CGr object estimation against a reference value by the En index.
}
\usage{
\method{assess}{CGr}(x, y, x.B = list(u=0, dof=Inf), alpha=x$input$alpha)
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
X.ref<- list(x=1250, u=0, dof=Inf)
N<- 1000
d0<- 1-sin(seq(1:(5/2*N))/N*pi-pi*3/4)
set.seed(12345)
d1<- d0+rnorm(5/2*N, 0, 0.01)
d2<- d0+rnorm(5/2*N, 0, 0.01)
dat<- data.frame(x=1:length(d1), 
	y1=100*(d1-min(d1))/(max(d1)-min(d1)),
	y2=100*(d2-min(d2))/(max(d2)-min(d2)))

CGres<- CGr(dat, columns=c(2,3))
assess(CGres, X.ref)
}
\keyword{ En performance index }
\keyword{ CGr }
