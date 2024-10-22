print.CGdata <-
function(x, signifDigits=x$input$signifDigits, alpha=x$input$alpha, verbose = FALSE, ...) {
	str <- toString.CGdata(x, signifDigits, alpha, verbose)
	cat(str, "\n")
}
