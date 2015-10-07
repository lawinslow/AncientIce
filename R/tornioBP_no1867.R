
# ==========
# = Set WD =
# ==========
setwd("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce") # for ryan

# =============
# = Load Data =
# =============
tornio_no1867 <- read.table("./Data/tornio.tsv", sep="\t", header=TRUE)

# MAKE 1756 ICE DATE NA
# tornio_no1867[tornio_no1867[,"year"]==1756, "doy"] <- NA

# MAKE 1867 ICE DATE NA
tornio_no1867[tornio_no1867[,"year"]==1867, "doy"] <- NA

max.tornio_no1867 <- max(tornio_no1867[,"doy"], na.rm=TRUE)
min.tornio_no1867 <- min(tornio_no1867[,"doy"], na.rm=TRUE)
tornio_no1867[,"year2"] <- 1:nrow(tornio_no1867)


# ==================
# = Load Functions =
# ==================
func.location <- "./R/Functions"
invisible(sapply(paste(func.location, list.files(func.location), sep="/"), source, .GlobalEnv))


# ==========================
# = Tornio BP Alternatives =
# ==========================
AIC(lm(doy ~ year, data=tornio_no1867)) # No BP, just trend AIC = 2155.825
AIC(lm(doy ~ year + I(year^2), data=tornio_no1867)) # No BP, just trend AIC = 2154.881


# ==========
# = Tornio =
# ==========
# =========================================
# = Tornio: Calculate breakpoint with OLS =
# =========================================
bp.opts.t <- 1:nrow(tornio_no1867)
# r2.tornio_no1867 <- rep(NA, length(bp.opts.t))
aic.tornio_no1867 <- rep(NA, length(bp.opts.t))
for(i in 1:length(bp.opts.t)){
	t.bp.t <- bp.opts.t[i]
	t.bp.year.t <- tornio_no1867[t.bp.t,"year"]
	t.tornio_no1867 <- tornio_no1867
	t.tornio_no1867[,"bp"] <- (1:nrow(t.tornio_no1867))>=t.bp.t
	x1 <- pmax(I(t.tornio_no1867[,"year"]-t.bp.year.t),0)
	tobit.tornio_no1867.year <- lm(doy ~ year + x1, data=t.tornio_no1867) # 1867 (best AIC = 1240.113)
	aic.tornio_no1867[i] <- extractAIC(tobit.tornio_no1867.year)[2]
}


# =================================
# = Define Breakpoint and Indices =
# =================================
tornio_no1867.bp <- tornio_no1867[bp.opts.t[which.min(aic.tornio_no1867)],"year"]
tornio_no1867.bp
tornio_no1867.bp.i <- tornio_no1867[,"year"] < tornio_no1867.bp # indices in units of year2


# =================================
# = Get CI for BP Time Series Fit =
# =================================
x1 <- pmax(I(t.tornio_no1867[,"year"]-tornio_no1867.bp),0)
tornio_no1867.bp.fit.out <- lm(doy ~ year + x1, data=tornio_no1867)

tornio_no1867.ci <- data.frame("year"=tornio_no1867[,"year"], "doy"=tornio_no1867[,"doy"])
newdata <- data.frame("year"=tornio_no1867[,"year"], x1=x1)

tornio_no1867.se.fit <- data.frame(se.fit=predict(tornio_no1867.bp.fit.out, newdata=newdata, se.fit=TRUE, na.action=na.exclude)$se.fit)
tornio_no1867.se.fit[,"year"] <- tornio_no1867[,"year"][as.integer(row.names(tornio_no1867.se.fit))]
tornio_no1867.se.fit[,"fitted"] <- predict(tornio_no1867.bp.fit.out, newdata=newdata, se.fit=TRUE)$fit
# tornio_no1867.se.fit[,"se"] <- tornio_no1867.se.fit[,2]
tornio_no1867.se.fit[,"upr"] <- tornio_no1867.se.fit[,"fitted"] + tornio_no1867.se.fit[,1]*1.96
tornio_no1867.se.fit[,"lwr"] <- tornio_no1867.se.fit[,"fitted"] - tornio_no1867.se.fit[,1]*1.96

tornio_no1867.ci <- merge(tornio_no1867.ci, tornio_no1867.se.fit, by="year", all=TRUE)

#
# # ================
# # = Save Results =
# # ================
# save(tornio_no1867.bp, tornio_no1867.bp.i, aic.tornio_no1867, tornio_no1867, min.tornio_no1867, max.tornio_no1867, bp.opts.t, aic.tornio_no18672,bp.opts.t2, tornio_no1867.ci, file="./Results/tornio_no1867BP.RData")


# ============================
# = Load Original Breakpoint =
# ============================
load("./Results/tornioBP.RData")


# ==============================================
# = Plot Differences in the Likelihood Surface =
# ==============================================
png("./Figures/tornio.bp_AIC_compare1867.png", width=3.5, height=3.5, res=150, units="in")


# par(mfrow=c(2,1), mar=c(1.75, 1.75, 0.1, 0.1), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.5, 0.1, 0), tcl=-0.1, ps=8, cex=1)
# dev.new(width=3.5, height=3.5)
par(mfrow=c(2,1), mar=c(1, 2.5, 0.5, 1.75), oma=c(0.75,0.1,0.1,0.1), mgp=c(1.25, 0.1, 0), tcl=-0.1, ps=8, cex=1)
plot(tornio[bp.opts.t,"year"], tornio[bp.opts.t,"doy"], type="l", xlab="", ylab="Ice Date")
par(new=TRUE)
plot(tornio[bp.opts.t, "year"], aic.tornio, type="l", col="blue", xaxt="n", yaxt="n", xlab="", ylab="", lwd=2.5)
lines(tornio[bp.opts.t, "year"], aic.tornio, col="white", lwd=1)
axis(side=4)
mtext(bquote(AIC), side=4, line=0.75)
abline(v=tornio.bp, lwd=2, lty="dashed")



plot(tornio_no1867[bp.opts.t,"year"], tornio_no1867[bp.opts.t,"doy"], type="l", xlab="", ylab="Ice Date\n(excluding 1867)", xpd=NA)
mtext("Year", side=1, line=0.75)
par(new=TRUE)
plot(tornio_no1867[bp.opts.t, "year"], aic.tornio_no1867, type="l", col="blue", xaxt="n", yaxt="n", xlab="", ylab="", lwd=2.5)
lines(tornio_no1867[bp.opts.t, "year"], aic.tornio_no1867, col="white", lwd=1)
axis(side=4)
mtext(bquote(AIC), side=4, line=0.75)
abline(v=tornio_no1867.bp, lwd=2, lty="dashed")



dev.off()



# =========================
# = Values for Manuscript =
# =========================
summary(lm(doy ~ year + pmax(I(year-tornio_no1867.bp),0), data=tornio_no1867))

(-0.029729)*10 # -0.29729 days per decade

(-0.029729 + -0.036458)*10 # -0.66187 days per decade




