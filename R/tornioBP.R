

# =============
# = Load Data =
# =============
tornio <- read.table("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/Data/tornio.tsv", sep="\t", header=TRUE)
max.tornio <- max(tornio[,"doy"], na.rm=TRUE)
min.tornio <- min(tornio[,"doy"], na.rm=TRUE)
tornio[,"year2"] <- 1:nrow(tornio)


# ==================
# = Load Functions =
# ==================
func.location <- "/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/R/Functions"
invisible(sapply(paste(func.location, list.files(func.location), sep="/"), source, .GlobalEnv))


# ==========================
# = Tornio BP Alternatives =
# ==========================
AIC(lm(doy ~ year, data=t.tornio)) # No BP, just trend AIC = 2155.825
AIC(lm(doy ~ year + I(year^2), data=t.tornio)) # No BP, just trend AIC = 2154.881


# ==========
# = Tornio =
# ==========
# =========================================
# = Tornio: Calculate breakpoint with OLS =
# =========================================
bp.opts.t <- 1:nrow(tornio)
# r2.tornio <- rep(NA, length(bp.opts.t))
aic.tornio <- rep(NA, length(bp.opts.t))
for(i in 1:length(bp.opts.t)){
	t.bp.t <- bp.opts.t[i]
	t.bp.year.t <- tornio[t.bp.t,"year"]
	t.tornio <- tornio
	t.tornio[,"bp"] <- (1:nrow(t.tornio))>=t.bp.t
	tobit.tornio.year <- lm(doy ~ year + pmax(I(year-t.bp.year.t),0), data=t.tornio) # 1867 (best AIC = 1240.113)
	aic.tornio[i] <- extractAIC(tobit.tornio.year)[2]
}


# =================================
# = Define Breakpoint and Indices =
# =================================
tornio.bp <- tornio[bp.opts.t[which.min(aic.tornio)],"year"]
tornio.bp
tornio.bp.i <- tornio[,"year"] < tornio.bp # indices in units of year2

# ==================
# = Tornio: 2 BP's =
# ==================
bp.opts.t2 <- t(combn(1:nrow(tornio), 2))
# bp.opts.t2 <- bp.opts.t2[bp.opts.t2[,2]-bp.opts.t2[,1]>=20,]
aic.tornio2 <- rep(NA, nrow(bp.opts.t2))
t.bp.pb <- txtProgressBar(min=1, max=nrow(bp.opts.t2), style=3)
for(i in 1:nrow(bp.opts.t2)){
# for(i in 1:50){
	t.bp.t <- bp.opts.t2[i,]
	t.bp.year.t1 <- tornio[t.bp.t[1],"year"]
	t.bp.year.t2 <- tornio[t.bp.t[2],"year"]
	# t.tornio <- tornio
	# t.tornio[,"bp1"] <- (1:nrow(t.tornio))>=t.bp.t[1]
	# t.tornio[,"bp2"] <- (1:nrow(t.tornio))>=t.bp.t[2]
	tobit.tornio.year <- lm(doy ~ year + pmax(I(year-t.bp.year.t1),0) + pmax(I(year-t.bp.year.t2),0), data=tornio) # 1867
	aic.tornio2[i] <- extractAIC(tobit.tornio.year)[2]
	setTxtProgressBar(t.bp.pb, i)
}
close(t.bp.pb)

tornio[bp.opts.t2[which.min(aic.tornio2),],"year"]

torn.25 <- (bp.opts.t2[,2] - bp.opts.t2[,1]) >= 25
torn.50 <- (bp.opts.t2[,2] - bp.opts.t2[,1]) >= 50
torn.100 <- (bp.opts.t2[,2] - bp.opts.t2[,1]) >= 100
torn.200 <- (bp.opts.t2[,2] - bp.opts.t2[,1]) >= 200

min(aic.tornio2) # best 2-breakpoint model AIC is 1236.805
min(aic.tornio2[torn.25]) # best 2BP model, where BP's are >= 25 years apart, is 1239.815
min(aic.tornio2[torn.50]) # best 2BP model, where BP's are >= 50 years apart, is 1239.815
min(aic.tornio2[torn.100]) # best 2BP model, where BP's are >= 100 years apart, is 1239.815

torn.best.2BP.25.yr <- tornio[bp.opts.t2[torn.25,][which.min(aic.tornio2[torn.25]),],"year"]


# ================
# = Save Results =
# ================
save(tornio.bp, tornio.bp.i, aic.tornio, tornio, min.tornio, max.tornio, bp.opts.t, aic.tornio2,bp.opts.t2, file="/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/Results/tornioBP.RData")

