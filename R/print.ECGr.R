print.ECGr <-
function(x, signifDigits=x$input$signifDigits, alpha=x$input$alpha, verbose=FALSE, ...) {
	str <- toString.ECGr(x, signifDigits, alpha, verbose)
	cat(str, "\n")
}
