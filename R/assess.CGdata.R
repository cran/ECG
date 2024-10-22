assess.CGdata <-
function(x, y, x.B=list(u=0, dof=Inf), alpha=x$input$alpha) {
	edof <- (x$u^2 + x.B$u^2 + y$u^2)^2/(x$u^4/x$dof +x.B$u^4/x.B$dof+ y$u^4/y$dof)
	kp <- qt(1-alpha/2, edof)
	res<- (x$x-y$x)/(kp*sqrt(x$u^2 + x.B$u^2 + y$u^2))
	return( res )
}
