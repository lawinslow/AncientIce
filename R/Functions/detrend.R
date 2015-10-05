
detrend <- function(x, method=c("ols","tobit")){
	method <- match.arg(method)
	n <- length(x)
	time <- 1:n
	
	if(method=="tobit"){
		residuals(vglm(x~time, tobit(Lower=min.suwa, Upper=max.suwa), na.action=na.exclude))[,1]
	}else{
		residuals(lm(x~time, na.action=na.exclude))
	}
	
}