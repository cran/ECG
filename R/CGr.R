CGr <-
function(data, from=min(data$x), to=max(data$x), columns, 
	responseFraction = 0.50, useConstantDelta=FALSE,
	fixedResponseFraction=0.5, useFixedResponseFraction=FALSE, 
	replaceOutliers=TRUE, responseLowerLimit=min(data[, columns]), 
	responseUpperLimit=max(data[, columns]),
	alpha=0.05, 
	kp=if(length(columns)<=1) qnorm(1-alpha/2) else 
			qt(1-alpha/2, length(columns)-1), 
	signifDigits=2, useRobustStatistics = TRUE, ...) {
  
#  print(paste0("CGr(responseFraction)=", responseFraction))

	res<- .Internal.build.CGr(data = data, from = from, to = to, 
			columns = columns, 
			responseFraction=responseFraction, 
			useConstantDelta=useConstantDelta,
			fixedResponseFraction=fixedResponseFraction, 
			useFixedResponseFraction=useFixedResponseFraction, 
			replaceOutliers=replaceOutliers, 
			responseLowerLimit=responseLowerLimit, 
			responseUpperLimit=responseUpperLimit,
			alpha=alpha,
			kp=kp,
			signifDigits=signifDigits,
			useRobustStatistics=useRobustStatistics,
			...)
	return(res)
}
