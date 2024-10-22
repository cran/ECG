toString.ECGr <- 
function(x, signifDigits=x$input$signifDigits, alpha=x$input$alpha, verbose=FALSE, ...) {
      vals<- .Internal.format_signif(c(x$x, 
		x$frame$kp*x$u), signifDigits)

      res <- paste0("Method of Minimum Band: value = ", 
		vals[1], " \u00B1 ", vals[2] )	

	return( res )
}

