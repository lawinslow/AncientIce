
# Note: Why not use the mean of the bootstrapped runs as the mean value reported in the text?
# http://stats.stackexchange.com/questions/71357/why-not-report-the-mean-of-a-bootstrap-distribution


# ==================
# = Load Functions =
# ==================
func.location <- "R/Functions"
invisible(sapply(paste(func.location, list.files(func.location), sep="/"), source, .GlobalEnv))


# ==================
# = Load Libraries =
# ==================
library(VGAM)
library(plyr)


# =============
# = Load Data =
# =============
load("./Results/tornioBP.RData")
load("./Results/Suwa_BeforeAfter.RData")


# ===============
# = Set Options =
# ===============
n.boot <- 1E3 # number of bootstrap iterations
useParallel <- TRUE # whether to use parallel in ddply
useDetrend <- TRUE # whether or not to detrend DoY and covariate time series

suwa.before.i <- suwa[,"year"] >= 1581 & suwa[,"year"] <= 1655 # T/F index vector for early period in Suwa
suwa.after.i <- suwa[,"year"] >= 1923 & suwa[,"year"] <= 1997 # T/F for late period

suwa.preds <- c("year", "co2", "enso", "airt.march.kyoto", "aod") # Suwa covariates
tornio.preds <- c("year", "co2", "nao.djfm", "air.t.stock", "aod", "sunspots") # Tornio Covariates
# tornio.preds <- c("year", "co2", "nao.djfm", "air.t.mam", "aod", "sunspots") # Tornio Covariates, using local air temp, which has much shorter time series that stockholm temp



# ========
# = SUWA =
# ========
# ===============================================
# = Suwa: Run Tobit before and after breakpoint =
# ===============================================
suwa.1 <- suwa[suwa.before.i,]
suwa.2 <- suwa[suwa.after.i,]


# ============================
# = Rescale and Detrend Suwa =
# ============================
# Rescale and (optionally) Detrend Suwa Covariates
for(i in 1:length(suwa.preds)){
	
	if(useDetrend){
		suwa.1[,suwa.preds[i]] <- scale(detrend(suwa.1[,suwa.preds[i]]))
		suwa.2[,suwa.preds[i]] <- scale(detrend(suwa.2[,suwa.preds[i]]))
	}else{
		suwa.1[,suwa.preds[i]] <- scale((suwa.1[,suwa.preds[i]]))
		suwa.2[,suwa.preds[i]] <- scale((suwa.2[,suwa.preds[i]]))
	}

}

# Detrend Suwa DoY Time Series
if(useDetrend){
	suwa.1[,"doy"] <- detrend(suwa.1[,"doy"], method="tobit")
	suwa.2[,"doy"] <- detrend(suwa.2[,"doy"], method="tobit")
}else{
	
}


# ========================================
# = Set Up Objects to Store Suwa Results =
# ========================================
iceTobit.s1 <- data.frame("water"=NA, "period"=NA, "variable"=NA, "estimate"=NA, "stdE"=NA, "Z"=NA)
iceTobit.s2 <- data.frame("water"=NA, "period"=NA, "variable"=NA, "estimate"=NA, "stdE"=NA, "Z"=NA)


# =================================================
# = Do Suwa Before-After Regression w/ Covariates =
# =================================================
for(i in 1:length(suwa.preds)){
	t.ts.formula <- as.formula(paste("doy~", paste(suwa.preds[i], collapse="+"), sep=""))
	
	# do the before period
	t.ts1 <- vglm(t.ts.formula, tobit(Lower=min.suwa, Upper=max.suwa), data=suwa.1)
	t.ts1.s <- summary(t.ts1)@coef3
	iceTobit.s1[i,] <- c("suwa", "before", rownames(t.ts1.s)[3], t.ts1.s[3,"Estimate"], t.ts1.s[3,"Std. Error"], t.ts1.s[3,"z value"])
	
	# Bootstrap before period
	x.res.s1 <- as.numeric(residuals(t.ts1)[,1])
	x.fit.s1 <- as.numeric(fitted(t.ts1))
	boot.s1 <- bootRes(x.res=x.res.s1, x.fit=x.fit.s1, data0=suwa.1, vars=suwa.preds[i], upper=max.suwa, lower=min.suwa, parallel=useParallel, n.boot=n.boot)
	# iceTobit.s1[i,4] <- boot.s1[1] # use bootstrap mean
	iceTobit.s1[i,5] <- boot.s1[2] # use bootstrap "se" (really sd)
	
	# do the after period
	t.ts2 <- vglm(t.ts.formula, tobit(Lower=min.suwa, Upper=max.suwa), data=suwa.2)
	t.ts2.s <- summary(t.ts2)@coef3
	iceTobit.s2[i,] <- c("suwa", "after", rownames(t.ts2.s)[3], t.ts2.s[3,"Estimate"], t.ts2.s[3,"Std. Error"], t.ts2.s[3,"z value"])
	
	# Bootstrap the after period
	x.res.s2 <- as.numeric(residuals(t.ts2)[,1])
	x.fit.s2 <- as.numeric(fitted(t.ts2))
	boot.s2 <- bootRes(x.res=x.res.s2, x.fit=x.fit.s2, data0=suwa.2, vars=suwa.preds[i], upper=max.suwa, lower=min.suwa, parallel=useParallel, n.boot=n.boot)
	
	# iceTobit.s2[i,4] <- boot.s2[1] # use bootstrap mean
	iceTobit.s2[i,5] <- boot.s2[2] # use bootstrap se/sd

}


# ========================
# = Collect Suwa Results =
# ========================
iceTobit.s <- rbind(iceTobit.s1, iceTobit.s2)
iceTobit.s[,"estimate"] <- as.numeric(iceTobit.s[,"estimate"])
iceTobit.s[,"stdE"] <- as.numeric(iceTobit.s[,"stdE"])
iceTobit.s[,"Z"] <- as.numeric(iceTobit.s[,"Z"])


# =====================
# = Get Suwa P-Values =
# =====================
iceTobit.s <- ddply(iceTobit.s, "variable", getP)



# ==========
# = TORNIO =
# ==========
# =======================================
# = Define Tornio Before and After Data =
# =======================================
tornio.1 <- tornio[tornio.bp.i,]
tornio.2 <- tornio[!tornio.bp.i,]


# =====================================================
# = Rescale and (optionally) Detrend Tornio and Cov's =
# =====================================================
# Rescale and Detrend Tornio Covariates
for(i in 1:length(tornio.preds)){
	if(useDetrend){
		tornio.1[,tornio.preds[i]] <- scale(detrend(tornio.1[,tornio.preds[i]]))
		tornio.2[,tornio.preds[i]] <- scale(detrend(tornio.2[,tornio.preds[i]]))
	}else{
		tornio.1[,tornio.preds[i]] <- scale(tornio.1[,tornio.preds[i]])
		tornio.2[,tornio.preds[i]] <- scale(tornio.2[,tornio.preds[i]])
	}

}

# Optionally Detrend Tornio DoY time series
if(useDetrend){
	tornio.1[,"doy"] <- detrend(tornio.1[,"doy"])
	tornio.2[,"doy"] <- detrend(tornio.2[,"doy"])
}else{
	
}


# ==========================================
# = Set Up Objects to Store Tornio Results =
# ==========================================
iceTobit.t1 <- data.frame("water"=NA, "period"=NA, "variable"=NA, "estimate"=NA, "stdE"=NA, "Z"=NA)
iceTobit.t2 <- data.frame("water"=NA, "period"=NA, "variable"=NA, "estimate"=NA, "stdE"=NA, "Z"=NA)


# ================================================
# = Run Regression for Each of Tornio Predictors =
# ================================================
for(i in 1:length(tornio.preds)){
	t.tt.formula <- as.formula(paste("doy~", paste(tornio.preds[i], collapse="+"), sep=""))
	
	# do the before period
	t.tt1 <- lm(t.tt.formula, data=tornio.1)
	t.tt1.s <- summary(t.tt1)$coef
	iceTobit.t1[i,] <- c("tornio", "before", rownames(t.tt1.s)[2], t.tt1.s[2,"Estimate"], t.tt1.s[2,"Std. Error"], t.tt1.s[2,"t value"]) 
	
	# Bootstrap the after period
	x.res.t1 <- as.numeric(residuals(t.tt1))
	x.fit.t1 <- as.numeric(fitted(t.tt1 ))
	boot.t1 <- bootRes(x.res=x.res.t1, x.fit=x.fit.t1, data0=tornio.1, Type="OLS", vars=tornio.preds[i], upper=max.tornio, lower=min.tornio, parallel=useParallel, n.boot=n.boot)
	
	
	# do the after period
	t.tt2 <- lm(t.tt.formula, data=tornio.2)
	t.tt2.s <- summary(t.tt2)$coef
	iceTobit.t2[i,] <- c("tornio", "after", rownames(t.tt2.s)[2], t.tt2.s[2,"Estimate"], t.tt2.s[2,"Std. Error"], t.tt2.s[2,"t value"]) 
	
	# Bootstrap the after period
	x.res.t2 <- as.numeric(residuals(t.tt2))
	x.fit.t2 <- as.numeric(fitted(t.tt2))
	boot.t2 <- bootRes(x.res=x.res.t2, x.fit=x.fit.t2, data0=tornio.2, Type="OLS", vars=tornio.preds[i], upper=max.tornio, lower=min.tornio, parallel=useParallel, n.boot=n.boot)
	
}


# ==========================
# = Collect Tornio Results =
# ==========================
iceTobit.t <- rbind(iceTobit.t1, iceTobit.t2)
iceTobit.t[,"estimate"] <- as.numeric(iceTobit.t[,"estimate"])
iceTobit.t[,"stdE"] <- as.numeric(iceTobit.t[,"stdE"])
iceTobit.t[,"Z"] <- as.numeric(iceTobit.t[,"Z"])


# =======================
# = Get Tornio P-Values =
# =======================
iceTobit.t <- ddply(iceTobit.t, "variable", getP)


# ===========================
# = Combine Suwa and Tornio =
# ===========================
iceTobit <- rbind(iceTobit.s, iceTobit.t) # combine
iceTobit <- iceTobit[order(iceTobit[,"water"], iceTobit[,"variable"]),] # order


# ==============================
# = Control for multiple tests =
# ==============================
# Use every method you can think of
# NOTE: BH refers to Benjamini & Hochberg (1995), and unlike the other corrections, seeks to maintain the false discovery rate
# From Wikipedia:
	# The FDR criterion adapts so that the same number of false discoveries (V) will mean different things, depending on the total number of discoveries (R). This contrasts the family wise error rate criterion. For example, if inspecting 100 hypotheses (say, 100 genetic mutations or SNPs for association with some phenotype in some population):
		# If we make 4 discoveries (R), having 2 of them be false discoveries (V) is often unbearable. Whereas,
		# If we make 50 discoveries (R), having 2 of them be false discoveries (V) is often bearable.
	
	# The FDR criterion is scalable in that the same proportion of false discoveries out of the total number of discoveries (Q), remains sensible for different number of total discoveries (R). For example:
		# If we make 100 discoveries (R), having 5 of them be false discoveries (q=5%) can be bearable.
		# Similarly, if we make 1000 discoveries (R), having 50 of them be false discoveries (as before, q=5%) can still be bearable.
# Thus FDR is about maintaining the same % of false discoveries, whereas  family wise error rate (FWER) is about the same number of false discoveries (FWER is the probability of making a certain number of false discoveries, usually 1).
# As a result, the BH method is always going to be the least conservative p-value correction



# Check Tornio
cbind(iceTobit.t[-7], adjP(iceTobit.t[,7])) # note that under no form of correction can a pair of tornio coefficients be considered different

# Check Suwa
cbind(iceTobit.s[-7], adjP(iceTobit.s[,7])) # for all but the most conservative corrections, we retain all 3 pairs of significantly different suwa coefficients after correction



# =======================================
# = Paired t-test for before and afters =
# =======================================
d <- mean(iceTobit.t[iceTobit.t[,"period"]=="after","estimate"] - iceTobit.t[iceTobit.t[,"period"]=="before","estimate"])

tornio2 <- tornio
for(i in 1:length(tornio.preds)){
	tornio2[,tornio.preds[i]] <- scale(tornio2[,tornio.preds[i]])
}
# tornio2 <- as.data.frame(scale(tornio))
summary(lm(doy ~ aod:tornio.bp.i, data=tornio))
summary(lm(doy ~ aod + aod:tornio.bp.i, data=tornio2))
summary(lm(doy ~ co2*tornio.bp.i, data=tornio2))

summary(lm(doy ~ (aod+co2) + (aod+co2):tornio.bp.i, data=tornio2))

summary(aov(doy ~ (aod+co2) + (aod+co2):tornio.bp.i, data=tornio2))


# ================
# = Save Results =
# ================
save(iceTobit, iceTobit.t, iceTobit.s, tornio.preds, suwa.preds, file="Results/deltaDrivers.RData")


# ============================================
# = Write results of tobit analysis to table =
# ============================================
write.table(iceTobit, file="Results/tobit_coefficients.csv", sep=",", col.names=TRUE, row.names=FALSE)
