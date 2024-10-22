toString.ECGdata <- 
function(x, signifDigits=x$input$signifDigits, alpha=x$input$alpha, 
	verbose=FALSE, ...) {
      vals<- .Internal.format_signif(c(x$x, 
		x$frame$kp*x$u), signifDigits)

      res <- paste0("Method of Extrapolated Center of Gravity: value = (", 
		vals[1], " \u00B1 ", vals[2], ")"
		)

	if (verbose) {
		res <- paste0(res, "\nanalysis range=(", x$input$from, ",", 
			x$input$to, ")\n")
#		res <- paste0(res, "number of data points used=", x$frame$used.data.points, "\n")
	}

	return( res )
}
