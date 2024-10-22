print.ECGdata <-
function(x, signifDigits=x$input$signifDigits, alpha=x$input$alpha, verbose=FALSE, ...) {
	str <- toString.ECGdata(x, signifDigits, alpha, verbose)
	cat(str, "\n")
}
