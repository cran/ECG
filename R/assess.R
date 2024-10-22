assess <-
function(x, y, x.B = list(u=0, dof=Inf), alpha=0.05) {
	UseMethod("assess", x)
}
