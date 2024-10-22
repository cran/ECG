ECGdata <-
function(data, from=min(data$x), to=max(data$x), useConstantDelta=FALSE, 
	maxResponseFraction=0.5, minResponseFraction=0.05, 
	byResponseFraction=-0.05, fixedResponseFraction=0.5, 
	useFixedResponseFraction=FALSE, replaceOutliers=TRUE, 
	responseLowerLimit=min(data$y), responseUpperLimit=max(data$y),
	alpha=0.05, kp=qnorm(1-alpha/2), signifDigits=2,
	useRobustStatistics=TRUE, ...) {

	res<- .Internal.build.ECGdata(data=data, from=from, to=to, 
		useConstantDelta=useConstantDelta, 
		maxResponseFraction=maxResponseFraction, 
		minResponseFraction=minResponseFraction, 
		byResponseFraction=byResponseFraction, 
		fixedResponseFraction=fixedResponseFraction, 
		useFixedResponseFraction=useFixedResponseFraction, 
		replaceOutliers=replaceOutliers,
		responseLowerLimit=responseLowerLimit,
		responseUpperLimit=responseUpperLimit,
		alpha=alpha, kp=kp, signifDigits=signifDigits,
		useRobustStatistics=useRobustStatistics, ...)
	return( res )
}
