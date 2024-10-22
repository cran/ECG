\name{assess}
\alias{assess}
\title{
Generic method for assessing an estimate against a reference value
}
\description{
Generic method for assessing an estimate against a reference value by the En index.
}
\usage{
assess(x, y, x.B = list(u=0, dof=Inf), alpha=0.05)
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
\keyword{ En performance index }
\keyword{ asses }
