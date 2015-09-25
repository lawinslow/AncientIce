
# ==================
# = Load Libraries =
# ==================
library(VGAM)
library(rgenoud)

# ==========
# = Set WD =
# ==========
# setwd("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce") # for ryan


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
# AIC(vglm(doy ~ year, tobit(Lower=min.suwa, Upper=max.suwa), data=suwa)) # No breakpoint AIC = 3540.767 (lower limit = -Inf); AIC = 4222.201 (lower limit = -53)
# AIC(vglm(doy ~ year + I(year^2), tobit(Lower=min.suwa, Upper=max.suwa), data=suwa)) # No breakpoint +year^2 AIC = 3519.85 (lower limit = -Inf); AIC = 4110.219 (lower limit = -53)

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

suwa.bp.fit <- function(bps, method=c("VGAM", "bayes")){
	method <- match.arg(method)
	
	a1 <- suwa.year[suwa.bp[1]]
	x1 <- pmax(suwa.year-a1, 0)
	a2 <- suwa.year[suwa.bp[2]]
	x2 <- as.integer(suwa.year>a2)
	
	if(method=="VGAM"){
		
	}else if(method=="bayes"){
		bayes.tobit <- MCMCtobit(suwa.doy ~ suwa.year + x1 + x2, below=-Inf, above=max.suwa, mcmc=3000, verbose=0, chains=3, thin=5, burnin=2E3)

	}
		vglm(suwa.doy ~ suwa.year + x1 + x2, tobit(Lower=min.suwa, Upper=max.suwa), na.action=na.exclude)
}
suwa.bp.nll <- function(bps, ...){		
	fit <- suwa.bp.fit(bps, ...)
	if(class(fit)=="mcmc"){
		return(summary(fit)[[1]]["sigma2","Mean"])
	}else{
		return(AIC(fit))
	}
	
}


dom <- matrix(c(10,nrow(suwa)-11, 11, nrow(suwa)-10), ncol=2, byrow=TRUE)
suwa.bp.genoud <- genoud(suwa.bp.nll, 2, pop.size=50, max.generations=50, data.type.int=TRUE, Domains=dom, boundary.enforcement=2, method="VGAM") 

# (test1 <- suwa.bp.nll(c(358,458))) # corresponds to 1800 and 1900
# (test2 <- suwa.bp.nll(c(369,458))) # corresponds to 1811 and 1900
# (test3 <- suwa.bp.nll(c(369,431))) # corresponds to 1811 and 1873)
# (test4 <- suwa.bp.nll(c(375,451))) # corresponds to 1817 and 1893); from a quick optimization routine
# test4.fit <- suwa.bp.fit(bps=c(375, 451))
# png("~/Desktop/residualsTobitSuwa.png");qqnorm(resid(test4.fit, type="pearson")[,1]);qqline(resid(test4.fit, type="pearson")[,1]);dev.off()


suwa.bp <- suwa.bp.genoud$par
suwa.bp.fit.out <- suwa.bp.fit(bps=suwa.bp)
# resid(suwa.bp.fit.out, type="pearson")

# =================================
# = Bayesian Parameter Estimation =
# =================================
a1 <- suwa.year[suwa.bp[1]]
x1 <- pmax(suwa.year-a1, 0)
a2 <- suwa.year[suwa.bp[2]]
x2 <- as.integer(suwa.year>a2)
# bayes.tobit <- MCMCtobit(suwa.doy ~ suwa.year + x1 + x2, below=-Inf, above=max.suwa, mcmc=3000, verbose=0, chains=3, thin=5, burnin=2E3)
# summary(bayes.tobit)
# plot(bayes.tobit)



suwa.ci <- data.frame("year"=suwa.year, "doy"=suwa.doy)
newdata <- data.frame("suwa.year"=suwa.year, x1=x1, x=x2)
suwa.se.fit <- data.frame(predict(suwa.bp.fit.out, newdata=newdata, se.fit=TRUE, na.action=na.exclude)$se.fit)
suwa.se.fit[,"year"] <- suwa.year[as.integer(row.names(suwa.se.fit))]
suwa.se.fit[,"fitted"] <- predict(suwa.bp.fit.out, newdata=newdata, se.fit=TRUE)$fitted.values[,1]
suwa.se.fit[,"se"] <- suwa.se.fit[,2]
suwa.se.fit[,"upr"] <- suwa.se.fit[,"fitted"] + suwa.se.fit[,1]*1.96
suwa.se.fit[,"lwr"] <- suwa.se.fit[,"fitted"] - suwa.se.fit[,1]*1.96

suwa.ci <- merge(suwa.ci, suwa.se.fit, by="year", all=TRUE)

png("~/Desktop/suwa.ci.png", width=8*72, height=6*72)
plot(suwa.year, suwa.doy, pch=20)
plot.suwaFit <- function(suwa.ci){
	
	lp <- c("fitted","upr","lwr")
	lc <- c("black","blue","blue")
	
	for(i in 0:1){
		for(j in 1:3){
			lines(suwa.ci[x2==i,"year"], suwa.ci[x2==i,lp[j]], col=lc[j], lwd=2)
		}
	}
	
}

plot.suwaFit(suwa.ci)

dev.off()




# =================================
# = Define Breakpoint and Indices =
# =================================
# suwa.bp <- suwa[bp.opts.s[which.min(aic.suwa)],"year"]
suwa.bp.i <- suwa[,"year"] < suwa.bp[1] # indices in units of year2



# ================
# = Save Results =
# ================
save(suwa.bp, suwa.bp.i, aic.suwa, suwa, max.suwa, min.suwa, suwa.no.ice, file="./AncientIce/Results/suwaBP.RData")


# =================================
# = Manuscript Summary Statistics =
# =================================
summary(vglm(doy ~ year + pmax(I(year-suwa.bp), 0), tobit(Lower=min.suwa, Upper=max.suwa), data=suwa))
# year coeff = 0.0095219 
# slope in second half = 0.0274956

0.046321*10 # 0.46 days per decade

(0.046321 + 0.140514)*10 # 1.86835 days per decade
