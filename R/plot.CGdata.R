plot.CGdata <-
function(x, from=x$input$from, to=x$input$to, xlab=expression(nu(cm^-1)), 
	ylab="Transmittance", add=FALSE, verbose=FALSE, ...) {

	ss<- x$input$data$x>=from & x$input$data$x<=to
	dss<- x$input$data[ss,]
	if (add) {
		points(dss$x, dss$y, ...)
	} else {
		plot(dss$x, dss$y, xlab=xlab, ylab=ylab, ...)
	}
	if (verbose) {
#	  print(x$frame)
		# observed local minimum
		abline(v=x$frame$x.min.y, col=2, lty=2, lwd=2)

		# local maxima containing the observed local minimum
		lines(rep(x$frame$x.min,2), c(0,x$frame$y.x.min), lty=2)
		lines(rep(x$frame$x.max,2), c(0,x$frame$y.x.max), lty=2)

		# response at the local maxima
		lines(c(0, x$frame$x.min), rep(x$frame$y.x.min, 2), lty=2)
		lines(c(0, x$frame$x.max), rep(x$frame$y.x.max, 2), lty=2)

		# response at local minimum
		lines(c(0, x$frame$x.min.y), rep(x$frame$y.x.band.min, 2), lty=2)

		# response at fractional data
		lines(c(0, x$frame$x.k), rep(x$frame$y.x.band.max, 2), lty=2, col=4)

		# window of predictor values below the fractional data
		lines(rep(x$frame$x.h, 2), c(0, x$frame$y.x.band.max), lty=2, col=4)
		lines(rep(x$frame$x.k, 2), c(0, x$frame$y.x.band.max), lty=2, col=4)

		# estimated CG value
		abline(v=x$x, lty=2, col=3)
		abline(h=0)
	}
}
