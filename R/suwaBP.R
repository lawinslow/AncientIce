



suwa <- read.table("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/Data/suwa.tsv", sep="\t", header=TRUE)
max.suwa <- max(suwa[,"doy"], na.rm=TRUE)
min.suwa <- min(suwa[,"doy"], na.rm=TRUE)
suwa.no.ice <- suwa[,"no.ice"]==1L & !is.na(suwa[,"no.ice"])
suwa[suwa.no.ice ,"doy"] <- max.suwa
suwa[,"year2"] <- 1:nrow(suwa)


# =========================
# = Breakpoint with Tobit =
# =========================
# =========================================
# = Suwa: Calculate Breakpoint with Tobit =
# =========================================
bp.opts <- 10:(nrow(suwa)-10)
resid.vars <- rep(NA, length(bp.opts))
s.bp.pb <- txtProgressBar(min=1, max=length(bp.opts), style=3)
for(i in 1:length(bp.opts)){
	t.bp <- bp.opts[i]
	t.bp.year <- suwa[t.bp,"year"]
	t.suwa <- suwa
	t.suwa[,"bp"] <- (1:nrow(t.suwa))>=t.bp
	
	# tobit.suwa.year <- vglm(doy~year2*bp, tobit(Lower=min.suwa, Upper=max.suwa), data=t.suwa) # 1807
	# tobit.suwa.year <- vglm(doy ~ year:bp, tobit(Lower=min.suwa, Upper=max.suwa), data=t.suwa) # 1842
	tobit.suwa.year <- vglm(doy ~ year + pmax(I(year-t.bp.year), 0), tobit(Lower=min.suwa, Upper=max.suwa), data=t.suwa) # 1812
	
	
	resid.vars[i] <- exp(coef(tobit.suwa.year)[2])
	# nlls[i] <- tobit.suwa.year@criterion$loglikelihood
	
	min.sofar <- which.min(resid.vars)
	if(min.sofar!=i){
		min.tobit.suwa.year <- tobit.suwa.year
	}
	setTxtProgressBar(s.bp.pb, i)
}
suwa.bp <- suwa[bp.opts[which.min(resid.vars)],"year"]
suwa.bp
suwa.bp.i <- suwa[,"year"] < suwa.bp

save(suwa.bp, suwa.bp.i, resid.vars, suwa, file="/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/Resuts/suwaBP.RData")

