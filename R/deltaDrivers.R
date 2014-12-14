# ===============
# = Set Options =
# ===============
n.boot <- 40

# ==================
# = Load Libraries =
# ==================
library(VGAM)
library(plyr)

# =============
# = Load Data =
# =============
load("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/Results/tornioBP.RData")
load("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/Results/suwaBP.RData")


# ==================
# = Load Functions =
# ==================
source("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/R/bootRes.R")
source("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/R/getP.R")


# ===============================================
# = Suwa: Run Tobit before and after breakpoint =
# ===============================================
# suwa.1 <- suwa[suwa.bp.i,]
# suwa.2 <- suwa[!suwa.bp.i,]
suwa.before.i <- suwa[,"year"] >= 1581 & suwa[,"year"] <= 1681
suwa.after.i <- suwa[,"year"] >= 1897 & suwa[,"year"] <= 1997

suwa.1 <- suwa[suwa.before.i,]
suwa.2 <- suwa[suwa.after.i,]

suwa.preds <- c("year", "co2", "enso", "air.t.as", "aod")

for(i in 1:length(suwa.preds)){
	suwa.1[,suwa.preds[i]] <- scale(suwa.1[,suwa.preds[i]])
	suwa.2[,suwa.preds[i]] <- scale(suwa.2[,suwa.preds[i]])
}


iceTobit.s1 <- data.frame("water"=NA, "period"=NA, "variable"=NA, "estimate"=NA, "stdE"=NA, "Z"=NA)
iceTobit.s2 <- data.frame("water"=NA, "period"=NA, "variable"=NA, "estimate"=NA, "stdE"=NA, "Z"=NA)
for(i in 1:length(suwa.preds)){
	t.ts.formula <- as.formula(paste("doy~", paste(suwa.preds[i], collapse="+"), sep=""))
	
	# do the before period
	t.ts1 <- vglm(t.ts.formula, tobit(Lower=min.suwa, Upper=max.suwa), data=suwa.1)
	t.ts1.s <- summary(t.ts1)@coef3
	iceTobit.s1[i,] <- c("suwa", "before", rownames(t.ts1.s)[3], t.ts1.s[3,"Estimate"], t.ts1.s[3,"Std. Error"], t.ts1.s[3,"z value"])
	
	# Bootstrap before period
	x.res.s1 <- as.numeric(residuals(t.ts1)[,1])
	x.fit.s1 <- as.numeric(fitted(t.ts1))
	boot.s1 <- bootRes(x.res=x.res.s1, x.fit=x.fit.s1, data0=suwa.1, vars=suwa.preds[i], upper=max.suwa, lower=min.suwa, parallel=TRUE, n.boot=n.boot)
	# iceTobit.s1[i,4] <- boot.s1[1] # use bootstrap mean
	iceTobit.s1[i,5] <- boot.s1[2] # use bootstrap "se" (really sd)
	
	# do the after period
	t.ts2 <- vglm(t.ts.formula, tobit(Lower=min.suwa, Upper=max.suwa), data=suwa.2)
	t.ts2.s <- summary(t.ts2)@coef3
	iceTobit.s2[i,] <- c("suwa", "after", rownames(t.ts2.s)[3], t.ts2.s[3,"Estimate"], t.ts2.s[3,"Std. Error"], t.ts2.s[3,"z value"])
	
	# Bootstrap the after period
	x.res.s2 <- as.numeric(residuals(t.ts2)[,1])
	x.fit.s2 <- as.numeric(fitted(t.ts2))
	boot.s2 <- bootRes(x.res=x.res.s2, x.fit=x.fit.s2, data0=suwa.2, vars=suwa.preds[i], upper=max.suwa, lower=min.suwa, parallel=TRUE, n.boot=n.boot)
	
	# iceTobit.s2[i,4] <- boot.s2[1] # use bootstrap mean
	iceTobit.s2[i,5] <- boot.s2[2] # use bootstrap se/sd

	
}
iceTobit.s <- rbind(iceTobit.s1, iceTobit.s2)
iceTobit.s[,"estimate"] <- as.numeric(iceTobit.s[,"estimate"])
iceTobit.s[,"stdE"] <- as.numeric(iceTobit.s[,"stdE"])
iceTobit.s[,"Z"] <- as.numeric(iceTobit.s[,"Z"])

iceTobit.s <- ddply(iceTobit.s, "variable", getP)



# ===============================================
# = Tornio: Run Tobit before and after breakpoint =
# ===============================================
# tornio.1 <- tornio[tornio.bp.i,]
# tornio.2 <- tornio[!tornio.bp.i,]
tornio.before.i <- tornio[,"year"] >= 1803 & tornio[,"year"] <= 1866
tornio.after.i <- tornio[,"year"] >= 1937 & tornio[,"year"] <= 2000

tornio.1 <- tornio[tornio.before.i,]
tornio.2 <- tornio[tornio.after.i,]

tornio.preds <- c("year", "co2", "nao.djfm", "air.t.mam", "aod", "sunspots")
for(i in 1:length(tornio.preds)){
	tornio.1[,tornio.preds[i]] <- scale(tornio.1[,tornio.preds[i]])
	tornio.2[,tornio.preds[i]] <- scale(tornio.2[,tornio.preds[i]])
}


iceTobit.t1 <- data.frame("water"=NA, "period"=NA, "variable"=NA, "estimate"=NA, "stdE"=NA, "Z"=NA)
iceTobit.t2 <- data.frame("water"=NA, "period"=NA, "variable"=NA, "estimate"=NA, "stdE"=NA, "Z"=NA)
for(i in 1:length(tornio.preds)){
	t.tt.formula <- as.formula(paste("doy~", paste(tornio.preds[i], collapse="+"), sep=""))
	
	# do the before period
	t.tt1 <- vglm(t.tt.formula, tobit(Lower=min.tornio, Upper=max.tornio), data=tornio.1)
	t.tt1.s <- summary(t.tt1)@coef3
	iceTobit.t1[i,] <- c("tornio", "before", rownames(t.tt1.s)[3], t.tt1.s[3,"Estimate"], t.tt1.s[3,"Std. Error"], t.tt1.s[3,"z value"]) 
	
	# Bootstrap the after period
	x.res.t1 <- as.numeric(residuals(t.tt1)[,1])
	x.fit.t1 <- as.numeric(fitted(t.tt1 ))
	boot.t1 <- bootRes(x.res=x.res.t1, x.fit=x.fit.t1, data0=tornio.1, vars=tornio.preds[i], upper=max.tornio, lower=min.tornio, parallel=TRUE, n.boot=n.boot)
	
	
	# do the after period
	t.tt2 <- vglm(t.tt.formula, tobit(Lower=min.tornio, Upper=max.tornio), data=tornio.2)
	t.tt2.s <- summary(t.tt2)@coef3
	iceTobit.t2[i,] <- c("tornio", "after", rownames(t.tt2.s)[3], t.tt2.s[3,"Estimate"], t.tt2.s[3,"Std. Error"], t.tt2.s[3,"z value"])
	
	# Bootstrap the after period
	x.res.t2 <- as.numeric(residuals(t.tt2)[,1])
	x.fit.t2 <- as.numeric(fitted(t.tt2))
	boot.t2 <- bootRes(x.res=x.res.t2, x.fit=x.fit.t2, data0=tornio.2, vars=tornio.preds[i], upper=max.tornio, lower=min.tornio, parallel=TRUE, n.boot=n.boot)
	
}
iceTobit.t <- rbind(iceTobit.t1, iceTobit.t2)
iceTobit.t[,"estimate"] <- as.numeric(iceTobit.t[,"estimate"])
iceTobit.t[,"stdE"] <- as.numeric(iceTobit.t[,"stdE"])
iceTobit.t[,"Z"] <- as.numeric(iceTobit.t[,"Z"])


iceTobit.t <- ddply(iceTobit.t, "variable", getP)



# ===========================
# = Combine Suwa and Tornio =
# ===========================
iceTobit <- rbind(iceTobit.s, iceTobit.t) # combine
iceTobit <- iceTobit[order(iceTobit[,"water"], iceTobit[,"variable"]),] # order

# ================
# = Save Results =
# ================
save(iceTobit, iceTobit.t, iceTobit.s, file="/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/Results/deltaDrivers.RData")


# ============================================
# = Write results of tobit analysis to table =
# ============================================
write.table(iceTobit, file="/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/Results/tobit_coefficients.csv", sep=",", col.names=TRUE, row.names=FALSE)
