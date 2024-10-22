toString.CGdata <- 
function(x, signifDigits=x$input$signifDigits, alpha=x$input$alpha, 
	verbose=FALSE, ...) {
      vals<- .Internal.format_signif(c(x$x, 
		qt(1-alpha/2, x$dof)*x$u), signifDigits)
      res <- paste0("Method of Center of Gravity: value = (", 
		vals[1], " \u00B1 ", vals[2], ")"
		)
	if (verbose) {
		res <- paste0(res, "\nusing a coverage factor k=", 
			qt(1-alpha/2, x$dof), 
			" based on the t-distribution for df=", x$dof, 
			" degrees of freedom, and defines an interval estimated to ",
			"have a coverage probability of ", round((1-x$alpha)*100, 1), 
			" percent.")
		res <- paste0(res, "\nanalysis range=(", x$input$from, ",", 
			x$input$to, ")\n")
		res <- paste0(res, "\nnumber of data points used=", 
			x$frame$used.data.points, "\n")
	}
	return( res )
}
