
findBP <- function(x, y, bpOpts=NULL, meth=c("OLS","Tobit"), lower=-Inf, upper=Inf, outOpt=c("AIC","bp.date"), fullOut=FALSE, min.dist=10, pop.size=5E2){
	# ============================
	# = Explanation of Arguments =
	# ============================
	# x = year
	# y = doy
	# bpOpts = values of x that are candidate BP's
	# meth = use OLS or Tobit for regression; if Tobit, you should supply the lower and/or upper censoring limit
	# lower; upper; = the lower and upper censoring limits to be used in the Tobit
	# fullOut = if true, will return a matrix for alternative models and their AIC's, and BP's (if any)
		# alternative models include:
			# y ~ x
			# y ~ x + I(x^2)
			# 1 BP model (continuous segmented regression)
			# 2 BP model (continuous segmented regression)
	# min.dist = for the 2 BP alternative model, what is the minimum distance between the first and second BP (value in x) that is permissible?
		# e.g., if min.dist == 2, then the first and second breakpoint must be >= 2 apart

	
	# ====================
	# = Checks and Setup =
	# ====================
	# Sample Size
	N <- length(y)
	
	# Check for x y same length
	stopifnot(N==length(x))
	
	# Define breakpoint options if not given
	if(is.null(bpOpts)){
		bp.opts <- 2:(N-1)
	}
	
	# Match arguments
	outOpt <- match.arg(outOpt)
	meth <- match.arg(meth)
	
	# Check that Tobit is reasonable if selected
	if(meth=="Tobit"){
		require(VGAM)
		if(lower==-Inf & upper==Inf){
			warning("Method is Tobit, but no upper or lower limit set; if no censoring, OLS is faster.")
		}
	}
	
	# If want to check for 2 BP's, need the rgenoud package for optimization
	if(fullOut){
		require(rgenoud)
	}
	
	
	# ==========
	# = OLS BP =
	# ==========
	aic.dat <- rep(NA, length(bp.opts)) # store aic's
	for(i in 1:length(bp.opts)){
		t.bp <- bp.opts[i]
		t.bp.year <- x[t.bp]
		
		if(meth=="OLS"){
			t.mod <- lm(y ~ x + pmax(I(x-t.bp.year),0)) # continuous segmented regression with 1 BP
		}else if(meth=="Tobit"){
			t.mod <- vglm(y ~ x + pmax(I(x-t.bp.year),0), tobit(Lower=lower, Upper=upper)) # Tobit CSR
		}
		
		aic.dat[i] <- AIC(t.mod) # grab AIC
		
		# if(which.min(aic.dat)==i){ # uncomment to save the best BP model
		# 	bp1.mod <- t.mod
		# }
	}
	bp1.1 <- x[bp.opts[which.min(aic.dat)]]
	bp1.aic <- min(aic.dat, na.rm=TRUE)
	
	# If full output (other models, AIC and BP, etc) not desired, return either the AIC or the date
	if(!fullOut){
		if(outOpt=="AIC"){
			min(aic.dat, na.rm=TRUE)
		}else if(outOpt=="bp.date"){
			dat.bp
		}
	}
	
	
	# If full output desired, need to test alternatives
	bp2.nll <- function(bps){
		if(bps[2]<=bps[1]){
			return(9E9)
		}

		if((bps[2]-bps[1])<min.dist){
			return(9E9)
		}

		t.bp.year1 <- x[bps[1]]
		t.bp.year2 <- x[bps[2]]
		
		if(meth=="OLS"){
			bp2.mod <- lm(y ~ x + pmax(I(x-t.bp.year1), 0) + pmax(I(x-t.bp.year2), 0))
		}else if(meth=="Tobit"){
			bp2.mod <- vglm(y ~ x + pmax(I(x-t.bp.year1), 0) + pmax(I(x-t.bp.year2), 0), tobit(Lower=lower, Upper=upper))
		}
		
		AIC(bp2.mod)
	}
	
	if(fullOut){
		# Fit 2 BP Model
		dom <- matrix(c(2, N-2, 3, N-1), ncol=2, byrow=TRUE)
		bp2.fit <- genoud(bp2.nll, 2, pop.size=pop.size, max.generations=50, data.type.int=TRUE, Domains=dom, boundary.enforcement=2, print.level=0)
		bp2.aic <- bp2.fit$value
		bp2.bps <- x[bp2.fit$par]
		
		# Fit linear model
		lin.mod <- lm(y ~ x)
		lin.aic <- AIC(lin.mod)
		
		# Fit 2 poly
		poly2.mod <- lm(y ~ x + I(x^2))
		poly2.aic <- AIC(poly2.mod)
		
		# Create output matrix
		out.mat <- matrix(
			c(
				lin.aic, NA, NA,
				poly2.aic, NA, NA,
				bp1.aic, bp1.1, NA,
				bp2.aic, bp2.bps[1], bp2.bps[2]
			),
			nrow=3, dimnames=list(c("AIC","BP1","BP2"), c("lin","poly","bp1","bp2"))
		)
		
		return(out.mat)
	}

	
	
}






