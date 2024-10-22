assess.CGr <-
function(x, y, x.B=list(u=0, dof=Inf), alpha=x$input$alpha) {
#  print(paste0("x$u=", x$u))
#  print(paste0("x$dof=", x$dof))
  
	edof <- (x$u^2 + x.B$u^2 + y$u^2)^2/(x$u^4/x$dof +x.B$u^4/x.B$dof+ y$u^4/y$dof)
#	print(paste0("dof=", edof))
	kp <- qt(1-alpha/2, edof)
#	print(paste0("kp=",kp))
	res<- (x$x-y$x)/(kp*sqrt(x$u^2 + x.B$u^2 + y$u^2))
#	print(paste0("En=",res))
	return( res )
}
