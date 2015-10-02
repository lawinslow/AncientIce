
bootRes <- function(x.res, x.fit, data0, vars, Type=c("Tobit", "OLS"), n.boot=5, upper=Inf, lower=-Inf, parallel=FALSE){
	
	Type <- match.arg(Type)
	
	require(forecast)
	
	if(parallel){
		library(doParallel)
		library(foreach)
		
		if(Sys.info()["sysname"]=="Windows"){
			nC <- floor(detectCores()*0.75)
			registerDoParallel(cores=nC)
		}else{
			registerDoParallel()
		}
	}
	
	data0 <- data0[,c("doy",vars)]
	data0 <- data0[complete.cases(data0),]
	
	# Get a few convenient values
	N <- length(x.res) # length of time series (not including NA's in response/ predictor ...)
	x.reg <- data0[,vars] # get the predictor values.
	
	# Identify ARIMA model
	aa <- auto.arima(x.res, stationary=TRUE) # from forecast package. 
	
	aa.coef <- aa$coef # pull out the auto arima coefficients
	ars <- grepl("ar",names(aa.coef)) # where are the ar coeffs?
	mas <- grepl("ma",names(aa.coef)) # where are the ma coeffs?
	aa.mod <- list(ar=aa.coef[ars], ma=aa.coef[mas]) # create list of ar and ma coeffs – note that I'm worried about this being a crappy way to auto convert the auto.arima fit into the model for arima.sim. Needs to be checked.
	aa.sd <- sqrt(aa$sigma2) # the MLE of the process se
	
	# Function that does 1 iteration of bootstrapping
	# Notice use of scoping
	
	if(Type=="Tobit"){
		boot.heart <- function(){
			
			# Simulate from fit
			new.res <- as.numeric(arima.sim(model=aa.mod, n=N, sd=aa.sd))
	
			# Add sim res to x.fit
			new.doy <- pmax(pmin(x.fit+new.res, upper), lower) # but create censored DoY's
	
			# Fit new tobit
			new.vglm <- vglm(new.doy~x.reg, tobit(Lower=lower, Upper=upper))
	
			# return estimate
			return(summary(new.vglm)@coef3[3,"Estimate"])
		}
	}else{
		boot.heart <- function(){
			# Simulate from fit
			new.res <- as.numeric(arima.sim(model=aa.mod, n=N, sd=aa.sd))
	
			# Add sim res to x.fit
			new.doy <- x.fit+new.res
	
			# Fit new tobit
			new.ols <- lm(new.doy~x.reg)
	
			# return estimate
			return(summary(new.ols)$coef[2,"Estimate"])
		}
	}

	
	# Execute Bootstrapping
	if(parallel){
		ests <- foreach(i=1:n.boot, .combine=c) %dopar% boot.heart()
	}else{
		ests <- rep(NA, n.boot)
		for(i in 1:n.boot){
			ests[i] <- boot.heart()
		}
	}
	
	# Return bootstrapped mean, sd
	c(mean(ests, na.rm=TRUE), sd(ests, na.rm=TRUE))
}


