
# ==================
# = Load Libraries =
# ==================
library(VGAM)
#library(rgenoud)

# ==========
# = Set WD =
# ==========
#setwd("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce") # for ryan


# ==================
# = Load Functions =
# ==================
func.location <- "R/Functions"
invisible(sapply(paste(func.location, list.files(func.location), sep="/"), source, .GlobalEnv))


# =============
# = Load Data =
# =============
suwa <- read.table("Data/suwa.tsv", sep="\t", header=TRUE)
max.suwa <- max(suwa[,"doy"], na.rm=TRUE)
min.suwa <- -Inf #min(suwa[,"doy"], na.rm=TRUE)

suwa.no.ice <- suwa[,"no.ice"]==1L & !is.na(suwa[,"no.ice"]) # indexes years known to not freeze (which also means that we can't know the "doy" [feeze date] b/c it doesn't exist)
suwa[suwa.no.ice ,"doy"] <- max.suwa # for no freeze years; note that the answer is the same if we replace with (max.suwa) vs. with (max.suwa+100) [or any other positive increase]

# suwa.nodoy.freeze <- suwa[,"no.ice"]==0L & !is.na(suwa[,"no.ice"]) & is.na(suwa[,"doy"]) # indexes years
# suwa[suwa.nodoy.freeze,"doy"] <- min.suwa


suwa[,"year2"] <- 1:nrow(suwa)


# =========================
# = Breakpoint with Tobit =
# =========================
AIC(vglm(doy ~ year, tobit(Lower=min.suwa, Upper=max.suwa), data=suwa)) # No breakpoint AIC = 3540.767 (lower limit = -Inf); AIC = 4222.201 (lower limit = -53)
AIC(vglm(doy ~ year + I(year^2), tobit(Lower=min.suwa, Upper=max.suwa), data=suwa)) # No breakpoint +year^2 AIC = 3519.85 (lower limit = -Inf); AIC = 4110.219 (lower limit = -53)

# =========================================
# = Suwa: Calculate Breakpoint with Tobit =
# =========================================
# bp.opts.s <- 10:(nrow(suwa)-10)
# s.bp.pb <- txtProgressBar(min=1, max=length(bp.opts.s), style=3)
# aic.suwa <- rep(NA, length(bp.opts.s))
# for(i in 1:length(bp.opts.s)){
# 	t.bp <- bp.opts.s[i]
# 	t.bp.year <- suwa[t.bp,"year"]
# 	# t.suwa <- suwa
# 	# t.suwa[,"bp"] <- (1:nrow(t.suwa))>=t.bp
#
# 	tobit.suwa.year <- vglm(doy ~ year + pmax(I(year-t.bp.year), 0), tobit(Lower=min.suwa, Upper=max.suwa), data=suwa) # 1812
#
# 	aic.suwa[i] <- AIC(tobit.suwa.year) # Best AIC w/ 1 BP: 3517.579 (just a little over 2 AIC points better than year + year^2)
#
# 	setTxtProgressBar(s.bp.pb, i)
# }

suwa.doy <- suwa[,"doy"]
suwa.year <- suwa[,"year"]
suwa.bp.fit <- function(bps){
	a1 <- suwa.year[bps[1]]
	x1 <- pmax(suwa.year-a1, 0)
	
	a2 <- suwa.year[bps[2]]
	x2 <- as.integer(suwa.year>a2)
	
	vglm(suwa.doy ~ suwa.year + x1 + x2, tobit(Lower=min.suwa, Upper=max.suwa))
}
suwa.bp.nll <- function(bps){		
	fit <- suwa.bp.fit(bps)
	return(AIC(fit))
}


dom <- matrix(c(10,nrow(suwa)-11, 11, nrow(suwa)-10), ncol=2, byrow=TRUE)
suwa.bp.genoud <- genoud(suwa.bp.nll, 2, pop.size=50, max.generations=50, data.type.int=TRUE, Domains=dom, boundary.enforcement=2) 

# (test1 <- suwa.bp.nll(c(358,458))) # corresponds to 1800 and 1900
# (test2 <- suwa.bp.nll(c(369,458))) # corresponds to 1811 and 1900
# (test3 <- suwa.bp.nll(c(369,431))) # corresponds to 1811 and 1873)
# (test4 <- suwa.bp.nll(c(375,451))) # corresponds to 1817 and 1893); from a quick optimization routine
test4.fit <- suwa.bp.fit(bps=c(375, 451))
# png("~/Desktop/residualsTobitSuwa.png");qqnorm(resid(test4.fit, type="pearson")[,1]);qqline(resid(test4.fit, type="pearson")[,1]);dev.off()

suwa.ci <- data.frame("year"=suwa.year, "doy"=suwa.doy)
suwa.se.fit <- data.frame(predict(test4.fit, se.fit=TRUE)$se.fit)
suwa.se.fit[,"year"] <- suwa.year[as.integer(row.names(suwa.se.fit))]
suwa.se.fit[,"fitted"] <- predict(test4.fit, se.fit=TRUE)$fitted.values[,1]
suwa.se.fit[,"se"] <- suwa.se.fit[,2]
suwa.se.fit[,"upr"] <- suwa.se.fit[,"fitted"] + se.fit[,1]*1.96
suwa.se.fit[,"lwr"] <- suwa.se.fit[,"fitted"] - se.fit[,1]*1.96

suwa.ci <- merge(suwa.ci, suwa.se.fit, by="year", all=TRUE)

png("~/Desktop/suwa.ci.png", width=8*72, height=6*72)
plot(suwa.year, suwa.doy, pch=20)
lines(suwa.ci[,"year"], suwa.ci[,"fitted"], col="black", lwd=2)
lines(suwa.ci[,"year"], suwa.ci[,"upr"], col="blue", lwd=2)
lines(suwa.ci[,"year"], suwa.ci[,"lwr"], col="blue", lwd=2)
dev.off()




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

	AIC(tobit.suwa.year) # + penalty
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
suwa.2bp.optim.gen.25 <- genoud(suwa.2bp.nll.25, 2, pop.size=500, max.generations=50, data.type.int=TRUE, Domains=dom, boundary.enforcement=2) 
# OLD result: with soft penalty for BP < 25 yrs apart, BP's are 1828 (386) and 1853 (411); AIC = 3515.235

suwa.2bp.optim.gen.50 <- genoud(suwa.2bp.nll.50, 2, pop.size=500, max.generations=50, data.type.int=TRUE, Domains=dom, boundary.enforcement=2) 
# OLD result: with 50 year hard limit, BP's are 1811 (369) and 1973 (531); AIC = 3516.003

suwa.2bp.optim.gen.100 <- genoud(suwa.2bp.nll.100, 2, pop.size=500, max.generations=50, data.type.int=TRUE, Domains=dom, boundary.enforcement=2) 
# OLD result: with 100 year hard limit, BP's are 1811 (369) and 1973 (531); AIC = 3516.003



# ================
# = Save Results =
# ================
save(suwa.bp, suwa.bp.i, aic.suwa, suwa, max.suwa, min.suwa, suwa.no.ice, bp.opts.s, suwa.2bp.optim.gen.25,suwa.2bp.nll.50, suwa.2bp.nll.100, file="./AncientIce/Results/suwaBP.RData")


# =================================
# = Manuscript Summary Statistics =
# =================================
summary(vglm(doy ~ year + pmax(I(year-suwa.bp), 0), tobit(Lower=min.suwa, Upper=max.suwa), data=suwa))
# year coeff = 0.0095219 
# slope in second half = 0.0274956

0.046321*10 # 0.46 days per decade

(0.046321 + 0.140514)*10 # 1.86835 days per decade
