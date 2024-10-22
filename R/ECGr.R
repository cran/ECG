ECGr <-
function(data, from=min(data$x), to=max(data$x), columns, 
	useConstantDelta=FALSE, 
	maxResponseFraction=0.5, minResponseFraction=0.05, 
	byResponseFraction=-0.05, fixedResponseFraction=0.5, 
	useFixedResponseFraction = FALSE, replaceOutliers = TRUE, 
	responseLowerLimit = min(data[, columns]), 
	responseUpperLimit = max(data[, columns]),
	alpha=0.05, kp=if(length(columns)<=1) qnorm(1-alpha/2) else 
		qt(1-alpha/2, length(columns)-1), 
	signifDigits = 2, useRobustStatistics=TRUE, ...) {
	
	res<- .Internal.build.ECGr(data, from, to, columns, 
		useConstantDelta=useConstantDelta, 
		maxResponseFraction=maxResponseFraction, 
		minResponseFraction=minResponseFraction, 
		byResponseFraction=byResponseFraction, 
		fixedResponseFraction=fixedResponseFraction, 
		useFixedResponseFraction = useFixedResponseFraction, 
		replaceOutliers = replaceOutliers, 
		responseLowerLimit = responseLowerLimit, 
		responseUpperLimit = responseUpperLimit,
		alpha=alpha, kp=kp, signifDigits = signifDigits, 
		useRobustStatistics=useRobustStatistics, ...)
	return(res)
}
