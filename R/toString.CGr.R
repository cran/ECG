toString.CGr <- 
function(x, signifDigits=x$input$signifDigits, alpha=x$input$alpha, verbose=FALSE, ...) {
      vals<- .Internal.format_signif(c(x$x, 
		x$input$kp*x$u), x$input$signifDigits)

      res <- paste0("Method of Center of Gravity with replicates: value = (", 
		vals[1], " \u00B1 ", vals[2], ")"
		)

	return( res )
}
