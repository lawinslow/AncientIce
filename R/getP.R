
# RDB
# Function to extract p values from a data.frame of before and after estimates of regression coefficients,
# 2 elements in column "period" are "before" or "after" 
# 2 elements in colmn "estimate" are the regression coefficients from the 2 periods
# 2 elements in column "stdE" are the standard errors of the regression coefficients
getP <- function(x){
	betaBefore <- x[x[,"period"]=="before","estimate"]
	betaAfter <- x[x[,"period"]=="after","estimate"]
	
	seBetaBefore <- x[x[,"period"]=="before","stdE"]
	seBetaAfter <- x[x[,"period"]=="after","stdE"]
	
	Z <- (betaBefore-betaAfter)/sqrt(seBetaBefore+seBetaAfter)
	
	if(Z <0){
		x[,"diff.Pval"] <- pnorm(Z, lower.tail=TRUE)*2
	}else{
		x[,"diff.Pval"] <- pnorm(Z, lower.tail=FALSE)*2
	}
	
	x
}