

# =============
# = Load Data =
# =============
tornio <- read.table("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/Data/tornio.tsv", sep="\t", header=TRUE)
max.tornio <- max(tornio[,"doy"], na.rm=TRUE)
min.tornio <- min(tornio[,"doy"], na.rm=TRUE)
tornio[,"year2"] <- 1:nrow(tornio)


# ==========
# = Tornio =
# ==========
# =========================================
# = Tornio: Calculate breakpoint with OLS =
# =========================================
bp.opts.t <- 1:nrow(tornio)
r2.tornio <- rep(NA, length(bp.opts.t))
max.sofar <- NA
for(i in 1:length(bp.opts.t)){
	t.bp.t <- bp.opts.t[i]
	t.bp.year.t <- tornio[t.bp.t,"year"]
	t.tornio <- tornio
	t.tornio[,"bp"] <- (1:nrow(t.tornio))>=t.bp.t
	# tobit.tornio.year <- lm(doy ~ year2:bp, data=t.tornio) # 1886
	# tobit.tornio.year <- lm(doy ~ year2*bp, data=t.tornio) # 1807
	tobit.tornio.year <- lm(doy ~ year + pmax(I(year-t.bp.year.t),0), data=t.tornio) # 1807
	r2.tornio[i] <- summary(tobit.tornio.year)$r.squared
	
	max.sofar <- which.max(r2.tornio)
	if(max.sofar!=i){
		max.tobit.tornio.year <- tobit.tornio.year
	}
}

# =================================
# = Define Breakpoint and Indices =
# =================================
tornio.bp <- tornio[bp.opts.t[which.max(r2.tornio)],"year"]
tornio.bp
tornio.bp.i <- tornio[,"year"] < tornio.bp # indices in units of year2


# ================
# = Save Results =
# ================
save(tornio.bp, tornio.bp.i, r2.tornio, tornio, min.tornio, max.tornio, bp.opts.t, file="/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/Results/tornioBP.RData")

