\name{assess.CGdata}
\alias{assess.CGdata}
\title{
Evaluates the CGdata object estimation
}
\description{
Evaluates the CGdata object estimation against a reference value by the En index.
}
\usage{
\method{assess}{CGdata}(x, y, x.B=list(u=0, dof=Inf), alpha=x$input$alpha)
}
\arguments{
  \item{x}{
	a CGdata object
}
  \item{y}{
	a list with the reference value (x, u)
}
  \item{x.B}{
	a list with the estimate of uncertainty type B for the x object and its degrees of freedom
}
\item{alpha}{
	a numeric, the level of significance to compute the coverage factor and expanded uncertainty
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

N<- 1000
set.seed(12345)
d1<- 1-sin(seq(1:(5/2*N))/N*pi-pi*3/4)+rnorm(5/2*N, 0, 0.01)
dat<- data.frame(x=1:length(d1), 
	y=100*(d1-min(d1))/(max(d1)-min(d1)))

CGres<- CGdata(dat)

assess(CGres, y=list(x=pi/2, u=0))

}
\keyword{ En performance index }
\keyword{ CGdata }
