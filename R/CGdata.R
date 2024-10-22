#2345678901234567890123456789012345678901234567890123456789012345678901234567890
CGdata <-
function(data, from=min(data$x), to=max(data$x), responseFraction = 0.5, 
	useConstantDelta = FALSE, fixedResponseFraction = 0.5, 
	useFixedResponseFraction = FALSE, replaceOutliers = TRUE, 
	responseLowerLimit = min(data$y), responseUpperLimit = max(data$y),
	alpha = 0.05, signifDigits = 2, ...) {
  
#  print(paste0("CGdata(responseFraction)=", responseFraction))

	res<- .Internal.build.CGdata(data, from=from, to=to, 
		responseFraction=responseFraction, 
		useConstantDelta=useConstantDelta, 
		fixedResponseFraction=fixedResponseFraction,
		useFixedResponseFraction=useFixedResponseFraction,
		replaceOutliers = replaceOutliers, 
		responseLowerLimit = responseLowerLimit,
		responseUpperLimit = responseUpperLimit, alpha = alpha,
		signifDigits = signifDigits, ...)
	return(res)
}
