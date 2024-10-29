plot.CGr <-
function(x, ...) {
	args <- list(...)
	if ("verbose" %in% args) {
		verbose <- args$verbose
	} else {
		verbose <- FALSE
	}
	nn<- length(x$input$columns)
	CG.c<- CGdata(data.frame(x=x$input$data$x, y=x$input$data[,x$input$columns[1]]), 
		x$input$from, x$input$to, x$input$responseFraction, 
		use.constant.delta=x$input$use.constant.delta, f.fixed=x$input$f.fixed, 
		use.f.fixed=x$input$use.f.fixed, ...)
	plot(CG.c, type="l", lwd=2, col=8, ...)
	for(kk in 2:nn) {
		CG.c<- CGdata(data.frame(x=x$input$data$x, y=x$input$data[,x$input$columns[kk]]), 
			x$input$from, x$input$to, x$input$responseFraction, 
			use.constant.delta=x$input$use.constant.delta, 
			f.fixed=x$input$f.fixed, use.f.fixed=x$input$use.f.fixed, ...)
		plot(CG.c, type="l", lwd=2, add=TRUE, col=8)
	}

	CG.c<- CGdata(data.frame(x=x$input$data$x, y=x$input$data$y), x$input$from, x$input$to, 
		x$input$responseFraction, use.constant.delta=x$input$use.constant.delta, 
		f.fixed=x$input$f.fixed, use.f.fixed=x$input$use.f.fixed, ...)
	plot(CG.c, type="l", lwd=1, add=TRUE, verbose=verbose)
}
