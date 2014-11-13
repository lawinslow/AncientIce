
# ==================
# = Load Libraries =
# ==================
library(VGAM)


# =============
# = Load Data =
# =============
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
bp.opts.s <- 10:(nrow(suwa)-10)
resid.vars <- rep(NA, length(bp.opts.s))
s.bp.pb <- txtProgressBar(min=1, max=length(bp.opts.s), style=3)
for(i in 1:length(bp.opts.s)){
	t.bp <- bp.opts.s[i]
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


# =================================
# = Define Breakpoint and Indices =
# =================================
suwa.bp <- suwa[bp.opts.s[which.min(resid.vars)],"year"]
suwa.bp
suwa.bp.i <- suwa[,"year"] < suwa.bp # indices in units of year2


# ================
# = Save Results =
# ================
save(suwa.bp, suwa.bp.i, resid.vars, suwa, max.suwa, min.suwa, bp.opts.s, file="/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/Results/suwaBP.RData")

