print.CGr <-
function(x, signifDigits=x$input$signifDigits, alpha=x$input$alpha, verbose=FALSE, ...) {
	str <- toString.CGr(x, signifDigits, alpha, verbose)
	cat(str, "\n")
}
