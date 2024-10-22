\name{ECG-package}
\alias{ECG-package}
\alias{ECG}
\docType{package}
\title{
Center of Gravity Methods
}
\description{
Estimates the local minimum of a series or readings by the center of gravity and the extrapolated center of gravity.
}
\details{
\tabular{ll}{
Package: \tab ECG\cr
Type: \tab Package\cr
Version: \tab 0.5.1\cr
Date: \tab 2024-10-01\cr
License: \tab GPL (>= 3)\cr
}
Define a series of readings of a series as a data frame object containing:
(date of reading, independent variable, replicated observations for control, replicated observations for unknown)
Then estimate the minimum by analysing its profile with the CGr method,
Optionally estimate the extrapolated minimum by analysisng its profile with the ECGr method.
}
\author{
H. Gasca-Aragon

Maintainer: H. Gasca-Aragon <hugo_gasca_aragon@hotmail.com>
}
\references{
JCGM 100:2008. \emph{Guide to the expression of uncertainty of measurement}.

Cameron et al. \emph{Precision in Condensed Phase Vibrational Spectroscopy}, Applied Spectroscopy, Vol 36, Number 3, 1982.
}
