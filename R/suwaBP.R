
# ==================
# = Load Libraries =
# ==================
library(VGAM)
library(DEoptim)
library(rgenoud)


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
AIC(vglm(doy ~ year, tobit(Lower=min.suwa, Upper=max.suwa), data=suwa)) # No breakpoint AIC = 3536.38
AIC(vglm(doy ~ year + I(year^2), tobit(Lower=min.suwa, Upper=max.suwa), data=suwa)) # No breakpoint +year^2 AIC = 3515.407

# =========================================
# = Suwa: Calculate Breakpoint with Tobit =
# =========================================
bp.opts.s <- 10:(nrow(suwa)-10)
s.bp.pb <- txtProgressBar(min=1, max=length(bp.opts.s), style=3)
aic.suwa <- rep(NA, length(bp.opts.s))
for(i in 1:length(bp.opts.s)){
	t.bp <- bp.opts.s[i]
	t.bp.year <- suwa[t.bp,"year"]
	# t.suwa <- suwa
	# t.suwa[,"bp"] <- (1:nrow(t.suwa))>=t.bp
	
	tobit.suwa.year <- vglm(doy ~ year + pmax(I(year-t.bp.year), 0), tobit(Lower=min.suwa, Upper=max.suwa), data=suwa) # 1812

	aic.suwa[i] <- AIC(tobit.suwa.year) # Best AIC w/ 1 BP: 3513.241 (just a little over 2 AIC points better than year + year^2)

	setTxtProgressBar(s.bp.pb, i)
}


# =================================
# = Define Breakpoint and Indices =
# =================================
suwa.bp <- suwa[bp.opts.s[which.min(aic.suwa)],"year"]
suwa.bp
suwa.bp.i <- suwa[,"year"] < suwa.bp # indices in units of year2


# ===============
# = Suwa 2 BP's =
# ===============
bp.opts.s2 <- t(combn(1:nrow(suwa), 2))
# s.bp.pb2 <- txtProgressBar(min=1, max=nrow(bp.opts.s2), style=3)
# aic.suwa2 <- rep(NA, nrow(bp.opts.s2))
# suwa2.skip <- !((bp.opts.s2[,2]-bp.opts.s2[,1])>=100 & bp.opts.s2[,1]>=10 & bp.opts.s2[,2]<=(nrow(suwa)-10))
# for(i in 1:nrow(bp.opts.s2)){
# 	if(suwa2.skip[i]){
# 		setTxtProgressBar(s.bp.pb2, i)
# 		next
# 	}
# 	t.bp <- bp.opts.s2[i,]
# 	t.bp.year1 <- suwa[t.bp[1],"year"]
# 	t.bp.year2 <- suwa[t.bp[2],"year"]
# 	# t.suwa <- suwa
# # 	t.suwa[,"bp1"] <- (1:nrow(t.suwa))>=t.bp[1]
# # 	t.suwa[,"bp2"] <- (1:nrow(t.suwa))>=t.bp[2]
# #
# 	tobit.suwa.year <- vglm(doy ~ year + pmax(I(year-t.bp.year1), 0) + pmax(I(year-t.bp.year2), 0), tobit(Lower=min.suwa, Upper=max.suwa), data=suwa) # 1812
#
# 	aic.suwa2[i] <- AIC(tobit.suwa.year)
#
# 	setTxtProgressBar(s.bp.pb2, i)
# }

suwa.2bp.nll.25 <- function(bps){
	if(bps[2]<=bps[1]){
		return(9E9)
	}

	penalty <- 10 - (bps[2] - bps[1])*0.4 # no penalty at 25 years
	penalty <- max(penalty, 0)
	
	t.bp.year1 <- suwa[bps[1],"year"]
	t.bp.year2 <- suwa[bps[2],"year"]
	
	tobit.suwa.year <- vglm(doy ~ year + pmax(I(year-t.bp.year1), 0) + pmax(I(year-t.bp.year2), 0), tobit(Lower=min.suwa, Upper=max.suwa), data=suwa) # 1812

	AIC(tobit.suwa.year) + penalty
}


suwa.2bp.nll.50 <- function(bps){
	if(bps[2]<=bps[1]){
		return(9E9)
	}
	
	if((bps[2]-bps[1])<50){
		return(9E9)
	}
	
	t.bp.year1 <- suwa[bps[1],"year"]
	t.bp.year2 <- suwa[bps[2],"year"]
	
	tobit.suwa.year <- vglm(doy ~ year + pmax(I(year-t.bp.year1), 0) + pmax(I(year-t.bp.year2), 0), tobit(Lower=min.suwa, Upper=max.suwa), data=suwa) # 1812

	AIC(tobit.suwa.year) #+ penalty
}


suwa.2bp.nll.100 <- function(bps){
	if(bps[2]<=bps[1]){
		return(9E9)
	}

	if((bps[2]-bps[1])<100){
		return(9E9)
	}
	
	t.bp.year1 <- suwa[bps[1],"year"]
	t.bp.year2 <- suwa[bps[2],"year"]
	
	tobit.suwa.year <- vglm(doy ~ year + pmax(I(year-t.bp.year1), 0) + pmax(I(year-t.bp.year2), 0), tobit(Lower=min.suwa, Upper=max.suwa), data=suwa) # 1812

	AIC(tobit.suwa.year) #+ penalty
}

# suwa.2bp.optim.de <- DEoptim(suwa.2bp.nll, c(10,10), c(nrow(suwa)-10, nrow(suwa)-10), control=DEoptim.control(itermax=5, NP=500))
dom <- matrix(c(10,nrow(suwa)-11, 11, nrow(suwa)-10), ncol=2, byrow=TRUE)
suwa.2bp.optim.gen.25 <- genoud(suwa.2bp.nll.25, 2, pop.size=500, max.generations=50, data.type.int=TRUE, Domains=dom, boundary.enforcement=2) # with soft penalty for BP < 25 yrs apart, BP's are 1828 (386) and 1853 (411); AIC = 3510.898

suwa.2bp.optim.gen.50 <- genoud(suwa.2bp.nll.50, 2, pop.size=500, max.generations=50, data.type.int=TRUE, Domains=dom, boundary.enforcement=2)

suwa.2bp.optim.gen.100 <- genoud(suwa.2bp.nll.100, 2, pop.size=500, max.generations=50, data.type.int=TRUE, Domains=dom, boundary.enforcement=2)



# ================
# = Save Results =
# ================
save(suwa.bp, suwa.bp.i, aic.suwa, suwa, max.suwa, min.suwa, bp.opts.s, suwa.2bp.optim.gen.25, file="/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/Results/suwaBP.RData")

