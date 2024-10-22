plot.ECGdata <-
function(x, xlim=range(x$frame$solution[,1]), ylim=c(0, max(x$frame$solution[,5])),
 xlab=expression(nu[i]), ylab=expression(f[i]), add=TRUE, ...) {
	if (add) {
		points(x$frame$solution[,1], x$frame$solution[,5])
	} else {
		plot(x$frame$solution[,1], x$frame$solution[,5], 
			xlim=xlim, ylim=ylim, ylab=ylab, xlab=xlab, ...)
	}
	points(x$frame$solution[,1], x$frame$solution[,5], pch=20, ...)
	nn<- length(x$frame$solution[,5])
	points(x$x, min(x$input$data$y))
	points(x$x, min(x$input$data$y), pch=20, col=2)
}
