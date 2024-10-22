.Internal.format_signif <-
function(x, n = 2){
maxSignifDigits <- 20
minSignifDigits <- -20
ndigs <- n - ceiling(log(abs(x[2]), 10))
ndigs[ndigs > maxSignifDigits] <- maxSignifDigits
ndigs[ndigs < minSignifDigits] <- minSignifDigits
res <- round(x, ndigs)
return(res)
}

.Internal.ws <- 
function(u, dof, a){
	res <- sum(a*u^2)^2/sum( (a*u^2)^2/dof )
	return(res)
}

#2345678901234567890123456789012345678901234567890123456789012345678901234567890
#        1         2         3         4         5         6         7         8

.Internal.build.CGdata <-
function(data, from=min(data$x), to=max(data$x), responseFraction=0.5, 
	useConstantDelta=FALSE, fixedResponseFraction=0.5, 
	useFixedResponseFraction=FALSE, replaceOutliers = TRUE, 
	responseLowerLimit = min(data$y), responseUpperLimit = max(data$y),
	alpha = 0.05, signifDigits = 2, ...) {

	## d data frame structure is for a single observation (x=nu, y=T)
	if (!all(!is.na(match(names(data),c("x","y"))))) { 
		stop("data frame must contain single observation data (x, y)") 
	}
	ss<- data$x>=from & data$x<=to # subset indexes
	dss<- data[ss,] # subset to search
	if (replaceOutliers) {
		dss$y[dss$y > responseUpperLimit] <- responseUpperLimit
		dss$y[dss$y < responseLowerLimit] <- responseLowerLimit
	}
	y.x.band.min<- min(dss$y) 
	# single data point with the minimum value of y in the subset

	## this can return multiple values
	## this can be improved by selecting the mean of median depending on the 
	## use.robust.stats flag
	# single value median of x where the response is the minimum
	x.min.y<- median(dss$x[dss$y==y.x.band.min])

	## this is handled by extracting the min and max accordingly
	# subset indexes within dss above the location of the minimum up to 'to'
	ssu<- dss$x>=max(x.min.y) & dss$x<to
	# subset indexes within dss below the location of the minimum down to 'from'
	ssl<- dss$x>from & dss$x<=min(x.min.y)
	## 
	# single point the maximum of y on the upper subset ssu
	y.x.max<- max(dss$y[ssu])
	# single point the maximum of y on the lower subset ssl
	y.x.min<- max(dss$y[ssl])

	## this can return multple values we use the min and max accordingly
	# minimum single value of x where y is the maximum within the above subset ssu
	x.max<- min(dss$x[ssu][dss$y[ssu]==y.x.max])
	# maximum single value of x where y is the maximum within the lower subset ssl
	x.min<- max(dss$x[ssl][dss$y[ssl]==y.x.min])
	##--------------------------------------------------------------------
	## exclude values below the fraction under analysis 
	## probably present at the provided extreme points (from, to) 
	# subset indexes between the maximum peaks on y
	sss<- data$x>x.min & data$x<x.max
	# subset of datapoints within both local maxima peak points
	dsss<- data[sss,]
	# upper subset indexes within both local peak points, starting from the local mimima point
	sssu<- dsss$x>max(x.min.y) & dsss$x<x.max
	# lower subset indexes within both local peak points, ending at the local minima point
	sssl<- dsss$x>x.min & dsss$x<min(x.min.y)
	##--------------------------------------------------------------------
	# the minimum distance in response from local minima point to the maximum local peaks
	delta.y.0<- min( c(y.x.max-y.x.band.min, y.x.min-y.x.band.min) )
	
#	print(paste0("delta.y.0=", delta.y.0))

	# subset indexes for the points on the lower subset 
	# (from the lower position where the maximum response occurs to 
	# the location where the minumum response is observed)
	# and response below the responseFraction of interest
	sssh<- dsss$y[sssl]<=(y.x.band.min+delta.y.0*responseFraction)

# |----------------|-----------------------|---------------------|-----------|
# from	       x.min			x.min.y		x.max		     to
#			y.x.min			y.x.band.min	y.x.max
#                  |----------sss--------------------------------|
#			                         |------sss:sssu-------|
#                  |-------sss:sssl--------|
#                  |FFF|-------sss:sssh----|
#                                          |---sss:sssk------|FFF|
# if (sum(sssh)==0)
#----------------------------------h-------|
# else
#------------------| +
#                  |FFF|
#                      ^ min(c(1:length(sssh))[sssh])
# if (sum(sssk)==0)
#----------------------------------k-------|
# else
#------------------------------------------| +
#                                          |---sss:sssk------|FFF|
#                                                            ^ max(c(1:length(sssk))[sssk])

	# subset indexes for the points on the upper subset 
	# (from the location where the minumum response is observed to
	# the upper position where the maximum response occurs)
	# and response below the responseFraction of interest
	sssk<- dsss$y[sssu]<=(y.x.band.min+delta.y.0*responseFraction)

	if (sum(sssh) == 0) { 
	# el punto minimo (x.min.y) esta en el extremo izquierdo de la fraccion de datos, no hay algo menor
		h <- sum(data$x <= x.min.y) 
	} else {
	# el punto minimo esta dentro del intervalo
		h<- sum(data$x<=x.min)+min(c(1:length(sssh))[sssh])
	}

	if (sum(sssk) == 0) { 
	# el punto maximo esta en el extremo derecho, no hay algo mayor 
		k <- sum(data$x <= x.min.y)
	} else { 
	# el punto maximo esta dentro del intervalo, 
		k<- sum(data$x<=x.min.y)+max(c(1:length(sssk))[sssk]) 
	}

	## we need to add the last term since we have built ssh for x<min(x.min.y) via ssl 
	## and also since we have built ssk for x>max(x.min.y) via ssu
	## which excludes the points between the same minimum values y.min
	## sum(data$x>=min(x.min.y) & data$x<=max(x.min.y))

	## x.h is the largest smallest X value meeting the condition
	## x.k is the smallest X value meeting the condition
	x.h<- data$x[h]
	x.k<- data$x[k]

	d.x<- data$x[h:k]-data$x[h]
	d.nu<- d.x[-1]-d.x[-length(d.x)]
	if (useConstantDelta) {
		# assuming constant delta x
		d.nu<- mean(d.nu)
	}
	if (useFixedResponseFraction) {
		Delta.y0<- y.x.band.min+delta.y.0*fixedResponseFraction
	} else {
		Delta.y0<- y.x.band.min+delta.y.0*responseFraction
	}
	dp<- ((data$y[h:(k-1)]+data$y[(h+1):k])/2-Delta.y0)/sum((data$y[h:(k-1)]+data$y[(h+1):k])/2-Delta.y0)

	x.cg<- data$x[h+1] + sum(d.nu*c(h:(k-1)-h-1/2)*dp)
	v.cg<- abs(sum( (data$x[h+1]+d.nu*(c(h:(k-1))-h-1/2) - x.cg)^2*dp ))
	s.cg<- sum( (data$x[h+1]+d.nu*(c(h:(k-1))-h-1/2) - x.cg)^3*dp )
	k.cg<- abs(sum( (data$x[h+1]+d.nu*(c(h:(k-1))-h-1/2) - x.cg)^4*dp ))

	# standard uncertainty of the mean value
	dof <- abs(k-h)
	u.x.cg<- sqrt( v.cg/(dof+1) )
	
#	print(y.x.band.min)
#	print(delta.y.0)
	if (is.null(responseFraction)) stop("responseFraction is null")
#	print(responseFraction)
#	print(paste0("y.x.band.max=",(y.x.band.min+delta.y.0*responseFraction)))


	res<- list(x=x.cg, u=u.x.cg, dof=dof,
		moments=c(mean=x.cg, var=v.cg, skewness=s.cg, kurtosis=k.cg),
		input=list(
			data=data, 
			from=from,
			to=to,
			responseFraction=responseFraction[1], 
			useConstantDelta=useConstantDelta, 
			fixedResponseFraction=fixedResponseFraction, 
			useFixedResponseFraction=useFixedResponseFraction,
			replaceOutliers = replaceOutliers,
			responseLowerLimit = responseLowerLimit,
			responseUpperLimit = responseUpperLimit,
			alpha = alpha,
			signifDigits = signifDigits
		),
		frame=list(
			y.x.band.min=y.x.band.min[1], 
			y.x.band.max=(y.x.band.min+delta.y.0*responseFraction)[1],
			x.min.y=x.min.y,
			x.max=x.max,
			x.min=x.min,
			y.x.max=y.x.max,
			y.x.min=y.x.min,
			h=h,
			k=k,
			x.h=x.h,
			x.k=x.k,
      		used.data.points= dof+1,
			kp = qt(1-alpha/2, dof)
		)
	)
	class(res)<- "CGdata"
	return(res)
}

.Internal.build.CGr <-
function(data, from=min(data$x), to=max(data$x), columns, 
	responseFraction = 0.50, useConstantDelta=FALSE,
	fixedResponseFraction=0.5, useFixedResponseFraction=FALSE, 
	replaceOutliers=TRUE, responseLowerLimit=min(data[, columns]), 
	responseUpperLimit=max(data[, columns]), alpha=0.05, 
	kp=if(length(columns)<=1) qnorm(1-alpha/2) else 
			qt(1-alpha/2, length(columns)-1), 
	signifDigits=2, useRobustStatistics = TRUE, ...) {

	ss<- data$x>=from & data$x<=to
	dss<- data[ss,]
	nn<- length(columns)
## 2017-04-28: the number of data points used in the estimation is now reported
#	CG.res<- matrix(NA, nn+1, 6)
## 2024-10-20: the reported columns include estimated moments (variance, skewness, kutosis)
	CG.res<- matrix(NA, nn+1, 9)
	
	dof <- nn-1 
	
	kp <- if(dof <= 1) qnorm(1-alpha/2) else 
			qt(1-alpha/2, dof)

	for( i in 1:nn ) {
		CG.c<- CGdata(data.frame(x=dss$x, y=dss[,columns[i]]), from=from, 
			to=to, responseFraction=responseFraction, 
			useConstantDelta=useConstantDelta, 
			fixedResponseFraction=fixedResponseFraction, 
			useFixedResponseFraction=useFixedResponseFraction, 
			replaceOutliers=replaceOutliers,
			responseLowerLimit=responseLowerLimit,
			responseUpperLimit=responseUpperLimit,
			alpha=alpha, signifDigits=signifDigits,
			...)
## CG.res[i>1,] contains the center of gravity of the ith series
##    print( CG.c$frame$used.data.points )
		CG.res[i+1,]<- c(CG.c$x, CG.c$u, 
#			CG.c$input$responseFraction, 
			responseFraction, 
			CG.c$frame$y.x.band.min, 
			CG.c$frame$y.x.band.max, 
			CG.c$frame$used.data.points, 
			CG.c$moments[2:4])
	}
	u.b<- sd(CG.res[-1,1])/sqrt(nn)

## 2017-04-28: correction, the uncertainty of a single value was reported instead of the uncertainty of the mean value.
## the ECG method was correctly computed.
	if (useRobustStatistics==TRUE) {
		dss$y<- apply(dss[,columns], 1, median)
		dss$u.y<- apply(dss[,columns], 1, mad)/sqrt(nn)
	} else {
		dss$y<- apply(dss[,columns], 1, mean)
		dss$u.y<- apply(dss[,columns], 1, sd)/sqrt(nn)
	}
## CG.res[1,] contains the center of gravity of the summary of all the series
	CG.c<- CGdata(data.frame(x=dss$x, y=dss$y), from=from, to=to, 
		responseFraction=responseFraction, 
		useConstantDelta=useConstantDelta, 
		fixedResponseFraction=fixedResponseFraction, 
		useFixedResponseFraction=useFixedResponseFraction, 
		replaceOutliers=replaceOutliers,
		responseLowerLimit=responseLowerLimit,
		responseUpperLimit=responseUpperLimit,
		alpha=alpha, signifDigits=signifDigits,
		...) 
	CG.res[1,]<- c(CG.c$x, CG.c$u, 
#		CG.c$input$responseFraction, 
		responseFraction, 
		CG.c$frame$y.x.band.min, 
		CG.c$frame$y.x.band.max, 
		mean(CG.res[-1, 6]),
		mean(CG.res[-1, 7]),
		mean(CG.res[-1, 8]),
		mean(CG.res[-1, 9]))

	x.cg<- CG.res[1,1]
#-------------------------------------------------------------------------------
# uncertainty estimation
# uncertainty due to the dispersion of the mean
# uncertainty due to reproducibility and repeatability
#-------------------------------------------------------------------------------
	u.x.cg<- CG.res[1,2]/sqrt(nn)
	u.x.rep<- sqrt( var(CG.res[-1,1])/nn )
	y.x.band.min<- CG.res[1,4]
	y.x.band.max<- CG.res[1,5]
## x.m1 contains the summary of the estimated center of gravity of each series
	if (useRobustStatistics ==TRUE) {
		x.m1<- median(CG.res[-1,1])
	} else {
		x.m1<- mean(CG.res[-1,1])
	}

	res<- list( 
			x=x.m1, 
			u=sqrt(u.x.cg^2+u.x.rep^2),
			dof = dof,

			x.summary=x.cg, 
			u.x.summary=u.x.cg,
			u.x.rep=u.x.rep, 

			frame=list(
				y.x.band.min=y.x.band.min,
				y.x.band.max=y.x.band.max,

				x.min.y=CG.c$frame$x.min.y,
				x.max=CG.c$frame$x.max,
				x.min=CG.c$frame$x.min,
				y.x.max=CG.c$frame$y.x.max,
				y.x.min=CG.c$frame$y.x.min,
				h=CG.c$frame$h,
				k=CG.c$frame$k,
				x.h=CG.c$frame$x.h,
				x.k=CG.c$frame$x.k,

	      used.data.points=CG.res[,6],
				moments = CG.res[, 7:9]
			),
			input=list(
				data=dss, 
				from=from, 
				to=to, 
				columns=columns, 
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
				useRobustStatistics=useRobustStatistics
			)
	)

	class(res)<- "CGr"
	return(res)
}


.Internal.build.ECGdata <-
function(data, from=min(data$x), to=max(data$x), useConstantDelta=FALSE, 
	maxResponseFraction=0.5, minResponseFraction=0.05, 
	byResponseFraction=-0.05, fixedResponseFraction=0.5, 
	useFixedResponseFraction = FALSE, replaceOutliers = TRUE, 
	responseLowerLimit = min(data$y), responseUpperLimit = max(data$y),
	alpha=0.05, signifDigits = 2, 
	useRobustStatistics=TRUE, ...) {

	if (!all(match(names(data),c("x","y")))) { 
		stop("data frame must contain single observation data (x, y)") 
	}
##  the number of data points is exported into the Extrapolated method
	solutions<- matrix(0, 
		ceiling(maxResponseFraction/abs(byResponseFraction)), 6)
	i<- 1
	responseFractionSequence <- seq(maxResponseFraction, 
		minResponseFraction, by=byResponseFraction)
	for (responseFraction in responseFractionSequence) {

		CGres<- CGdata(data, from, to, responseFraction, useConstantDelta,
			fixedResponseFraction=fixedResponseFraction, 
			useFixedResponseFraction=useFixedResponseFraction, 
			replaceOutliers = replaceOutliers,
			responseLowerLimit = responseLowerLimit, 
			responseUpperLimit = responseUpperLimit,
			alpha = alpha, signifDigits = signifDigits, ...)

#    print( CGres$frame$used.data.points )
    res <- c(CGres$x, CGres$u, 
#            CGres$input$responseFraction,
            responseFraction,
            CGres$frame$y.x.band.min, 
            CGres$frame$y.x.band.max, 
		        CGres$frame$used.data.points)
#    print( c(dim(solutions), length(res)) )
    
		solutions[i,]<- c(CGres$x, CGres$u, 
#		  CGres$input$responseFraction, 
		  responseFraction, 
		  CGres$frame$y.x.band.min, 
			CGres$frame$y.x.band.max, 
			CGres$frame$used.data.points)
		i<- i+1
	}
	solutions<- solutions[1:(i-1),]
#  print( dim(solutions) )

	# the covariance matrix must be positive semi-definite
	# this solution needs a proof
##	my.solutions<<- solutions
#	solutions<- solutions[solutions[-length(solutions[,2]),2]>solutions[-1,2],]

	f<- solutions[,3]
	f2<- f^2
	x<- solutions[,1]

	nn<- length(f)
#	dof <- max(solutions[,6])

	{
		## ignore the correlation and the noise of the signal
		# try a quadratic model
		if (nn>3){
			lm.res<- lm(x~f+f2)
			my.betas <- summary(lm.res)$coef
#			print(my.betas)
#			if (my.betas[3, 1]) {
#			}
			betas<- summary(lm.res)$coef[,1]
			x.ecg<- summary(lm.res)$coef[1,1]
			u.x.ecg<- summary(lm.res)$coef[1,2]*sqrt(nn)
			type<- 2
		} else if (nn>2) {
			# there is no enough information for a quadratic model
			# try a linear model
#			lm.res<- lm(x~f, weights=1/solutions[,2]^2)
			lm.res<- lm(x~f)
			betas<- summary(lm.res)$coef[,1]
			x.ecg<- summary(lm.res)$coef[1,1]
			u.x.ecg<- summary(lm.res)$coef[1,2]*sqrt(nn)
			type<- 1
		} else {
			# there is no enough information for a lineal model
			# try a constant model
			type<- 0
			if (useRobustStatistics) {
				betas<- c(median(x))
				x.ecg<- median(x)
				u.x.ecg<- mad(x)
				lm.res<- NA
			} else {
				lm.res<- lm(x~1)
				betas<- summary(lm.res)$coef[,1]
				x.ecg<- summary(lm.res)$coef[1,1]
				u.x.ecg<- summary(lm.res)$coef[1,2]*sqrt(nn)
			}
		}

		dof <- nn-length(betas)

		res<- list(x=x.ecg, 
			u=u.x.ecg, 
			dof=dof,
			input=list(
				data=data,
				from=from, 
				to=to, 
				useConstantDelta=useConstantDelta,
				maxResponseFraction=maxResponseFraction,
				minResponseFraction=minResponseFraction,
				byResponseFraction=byResponseFraction,
				fixedResponseFraction=fixedResponseFraction,
				useFixedResponseFraction=useFixedResponseFraction,
				replaceOutliers=replaceOutliers,
				responseLowerLimit=responseLowerLimit,
				responseUpperLimit=responseUpperLimit,
				alpha=alpha, signifDigits=signifDigits,
				useRobustStatistics=useRobustStatistics
			),

			frame=list(
				kp=qt(1-alpha/2, dof),
				y.x.band.min=median(solutions[,4]), 
				x.min=min(x), 
				x.max=max(x), 
				solution=solutions, 
				type=type,
				model=summary(lm.res)
			)
		)

	}
	class(res)<- "ECGdata"
	return( res )
}

.Internal.build.ECGr <-
function(data, from=min(data$x), to=max(data$x), columns, useConstantDelta=FALSE, 
	maxResponseFraction=0.5, minResponseFraction=0.05, 
	byResponseFraction=-0.05, fixedResponseFraction=0.5, 
	useFixedResponseFraction = FALSE, replaceOutliers = TRUE, 
	responseLowerLimit = min(data[, columns]), 
	responseUpperLimit = max(data[, columns]),
	alpha=0.05, kp=if(length(columns)<=1) qnorm(1-alpha/2) else 
		qt(1-alpha/2, length(columns)-1), 
	signifDigits = 2, useRobustStatistics=TRUE, ...) {

	ss<- data$x>=from & data$x<=to
	dss<- data[ss,]

	nn<- length(columns)
	dof = nn-1

	if (useRobustStatistics) {
		y<- apply(dss[,columns], 1, median)
		u.y<- apply(dss[,columns], 1, mad)/sqrt(nn)
	} else {
		y<- apply(dss[,columns], 1, mean)
		u.y<- apply(dss[,columns], 1, sd)/sqrt(nn)
	}

## the number of data points used in the estimation  
  used.data.points<- matrix(0, nn, ceiling(maxResponseFraction/abs(byResponseFraction)))

#  print( dim(used.data.points) )

  ecg.res<- matrix(0, nn+1, 6)
	for(i in 1:nn) {
		ecg.c<- ECGdata(data.frame(x=dss$x, y=dss[,columns[i]]), from, to,
			useConstantDelta=useConstantDelta, 
			maxResponseFraction=maxResponseFraction, 
			minResponseFraction=minResponseFraction, 
			byResponseFraction=byResponseFraction, 
			fixedResponseFraction=fixedResponseFraction, 
			useFixedResponseFraction=useFixedResponseFraction, 
			alpha=alpha, signifDigits=signifDigits,
			useRobustStatistics=useRobustStatistics, ...)

#    print( paste("ECGdata.solution", dim(ecg.c$frame$solution)) )
		ecg.res[i,]<- c(ecg.c$x, ecg.c$u, ecg.c$frame$y.x.band.min, 
			ecg.c$frame$x.min, ecg.c$frame$x.max, ecg.c$frame$kp)
    used.data.points[i,]<- c(ecg.c$frame$solution[,6])
	}
	ecg.c<- ECGdata(data.frame(x=dss$x, y=y), from, to, 
		useConstantDelta=useConstantDelta, 
		maxResponseFraction=maxResponseFraction, 
		minResponseFraction=minResponseFraction, 
		byResponseFraction=byResponseFraction, 
		fixedResponseFraction=fixedResponseFraction, 
		useFixedResponseFraction=useFixedResponseFraction, 
		alpha=alpha, signifDigits=signifDigits,
		useRobustStatistics=useRobustStatistics, ...)

	ecg.res[nn+1,]<- c(ecg.c$x, ecg.c$u, ecg.c$frame$y.x.band.min, 
		ecg.c$frame$x.min, ecg.c$frame$x.max, ecg.c$frame$kp)

	if (useRobustStatistics) {
		x.ecg<- median(ecg.res[-(nn+1),1])
		u.x.ecg<- sqrt((mad(ecg.res[-(nn+1),1])^2+mean(ecg.res[-(nn+1),2]^2))/length(ecg.res[-(nn+1),1]))
	} else {
		x.ecg<- mean(ecg.res[-(nn+1),1])
		u.x.ecg<- sqrt((var(ecg.res[-(nn+1),1])+mean(ecg.res[-(nn+1),2]^2))/length(ecg.res[-(nn+1),1]))
	}

	res<- list(x=x.ecg, 
		u=u.x.ecg,
		dof = dof,
		input=list(
			data=dss,
			from=from, 
			to=to, 
			columns=columns, 
			useConstantDelta=useConstantDelta, 
			maxResponseFraction=maxResponseFraction, 
			minResponseFraction=minResponseFraction, 
			byResponseFraction=byResponseFraction, 
			fixedResponseFraction=fixedResponseFraction, 
			useFixedResponseFraction=useFixedResponseFraction, 
			alpha=alpha, kp=kp, signifDigits=signifDigits,
			useRobustStatistics=useRobustStatistics
		),
		frame=list(
			y=y, 
			u.y=u.y, 
			kp=mean(ecg.res[,6]),
			x.summary=ecg.res[nn+1,1], 
			u.x.summary=ecg.res[nn+1,2], 
			details=ecg.res,
		      used.data.points=used.data.points
		)
	) 

	class(res)<- "ECGr"
	return( res )
}
