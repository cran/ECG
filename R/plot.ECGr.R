plot.ECGr <-
function(x, add=TRUE, ...) {
	nn<- length(x$input$columns)

	for(i in 1:nn) {
		ecg.c<- ECGdata(data.frame(x=x$input$data$x, y=x$input$data[,x$input$columns[i]]), 
			x$input$from, x$input$to, 
			useConstantDelta=x$input$useConstantDelta, 
			maxResponseFraction=x$input$maxResponseFraction, 
			minResponseFraction=x$input$minResponseFraction, 
			byResponseFraction=x$input$byResponseFraction, 
			fixedResponseFraction=x$input$fixedResponseFraction, 
			useFixedResponseFraction=x$input$useFixedResponseFraction, 
			alpha=x$input$alpha, signifDigits=x$input$signifDigits,
			useRobustStatistics=x$input$useRobustStatistics, ...)
		plot(ecg.c, add=add, col=8)
	}
	ecg.c<- ECGdata(data.frame(x=x$input$data$x, y=x$frame$y), 
		x$input$from, x$input$to, 
			useConstantDelta=x$input$useConstantDelta, 
			maxResponseFraction=x$input$maxResponseFraction, 
			minResponseFraction=x$input$minResponseFraction, 
			byResponseFraction=x$input$byResponseFraction, 
			fixedResponseFraction=x$input$fixedResponseFraction, 
			useFixedResponseFraction=x$input$useFixedResponseFraction, 
			alpha=x$input$alpha, signifDigits=x$input$signifDigits,
			useRobustStatistics=x$input$useRobustStatistics, ...)
	plot(ecg.c, add=TRUE, col=2)
}
