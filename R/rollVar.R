
rl.var.torn <- rollapplyr(Torn.1803[,"iceoff_julian"], by=1, width=30, FUN=sd, na.rm=TRUE)
rl.var.torn.day <- rollapplyr(Torn.1803[,1], by=1, width=30, FUN=max)

rl.var.suwa.early <- rollapplyr(Suwa.Early[,"DOY"], by=1, width=30, FUN=sd, na.rm=TRUE)
rl.var.suwa.early.day <- rollapplyr(Suwa.Early[,1], by=1, width=30, FUN=max)

rl.var.suwa.late <- rollapplyr(Suwa.Late[,"DOY"], by=1, width=30, FUN=sd, na.rm=TRUE)
rl.var.suwa.late.day <- rollapplyr(Suwa.Late[,1], by=1, width=30, FUN=max)


# dev.new(width=3.5, height=6)
png("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/Figures/rollVariance_30yr_suwa&torn.png", res=200, width=3.5, height=6, units="in")
par(mfrow=c(3,1), mar=c(2.5, 2.5, 0.5, 0.5), ps=11, mgp=c(1, 0.35, 0), tcl=-0.25)

plot(rl.var.torn.day, rl.var.torn, type="l", xlab="Year", ylab="stdev of Torn")

plot(rl.var.suwa.early.day, rl.var.suwa.early, type="l", xlab="Year", ylab="stdev of Early Suwa")

plot(rl.var.suwa.late.day, rl.var.suwa.late, type="l", xlab="Year", ylab="stdev of Late Suwa")
dev.off()




# ================
# = rollaring AC =
# ================
rl.ar1.torn <- rollapplyr(Torn.1803[,"iceoff_julian"], by=1, width=30, FUN=Ar1, na.rm=TRUE)
rl.ar1.torn.day <- rollapplyr(Torn.1803[,1], by=1, width=30, FUN=max)

rl.ar1.suwa.early <- rollapplyr(Suwa.Early[,"DOY"], by=1, width=30, FUN=Ar1, na.rm=TRUE)
rl.ar1.suwa.early.day <- rollapplyr(Suwa.Early[,1], by=1, width=30, FUN=max)

rl.ar1.suwa.late <- rollapplyr(Suwa.Late[,"DOY"], by=1, width=30, FUN=Ar1, na.rm=TRUE)
rl.ar1.suwa.late.day <- rollapplyr(Suwa.Late[,1], by=1, width=30, FUN=max)


# dev.new(width=3.5, height=6)
png("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/Figures/roll_autocorr_30yr_suwa&torn.png", res=200, width=3.5, height=6, units="in")
par(mfrow=c(3,1), mar=c(2.5, 2.5, 0.5, 0.5), ps=11, mgp=c(1, 0.35, 0), tcl=-0.25)

plot(rl.ar1.torn.day, rl.ar1.torn, type="l", xlab="Year", ylab="AR1 of Torn")

plot(rl.ar1.suwa.early.day, rl.ar1.suwa.early, type="l", xlab="Year", ylab="AR1 of Early Suwa")

plot(rl.ar1.suwa.late.day, rl.ar1.suwa.late, type="l", xlab="Year", ylab="AR1 of Late Suwa")
dev.off()







Ar1 <-function(x, na_permit=FALSE, detrend=TRUE, act=FALSE){
	# na_use <- ifelse(na_permit, "complete.obs", "everything")
	na_use <- ifelse(na_permit, "na.or.complete", "everything") #CHANGED I changed the na_permit argument to better handle missing values -- not only will it handle missing values by removing them and seeing what pairwise comparisons can still be made, but if there are no pairwise comparison, it will simply return NA (instead of an error if complete.obs was used) [23-May-2013]
	if(detrend & length(which(!is.na(x)))>2){
		na_act <- ifelse(na_permit, "na.exclude", "na.fail")
		tx <- 1:length(x)
		lm_x <- lm(x~tx, na.action=na_act)
		p_lmx <- summary(lm_x)$coef[2,4]
		if(p_lmx<=1){ #if the slope of the regression is significant #CHANGED v0.3.1 -- always detrend
			xd <- residuals(lm_x) # xd is the detrended version of x
		}else{
			xd <- x # otherwise xd is simply x, and autocorrelation will be computed as if detrend==FALSE
		}
		ac <- cor(xd[-1],xd[-length(xd)], use=na_use)
	}else{
		ac <- cor(x[-1], x[-length(x)], use=na_use)
	}
	if(act){
		ac <- AcTime(ac)
	}
	return(ac)
	# act <- AcTime(ac)
	# return(act)
}





