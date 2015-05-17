
# ==================
# = Load Libraries =
# ==================
library(VGAM)


# ==========
# = Set WD =
# ==========
setwd("/Users/Battrd/Documents/School&Work/WiscResearch") # for ryan


# ================
# = Load Results =
# ================
load("./AncientIce/Results/tornioBP.RData")
load("./AncientIce/Results/suwaBP.RData")


# ===================
# = Suwa Poly Model =
# ===================
poly.suwa <- vglm(doy ~ year + I(year^2), tobit(Lower=min.suwa, Upper=max.suwa), data=suwa)
AIC(poly.suwa)


# =====================
# = Tornio Poly Model =
# =====================
poly.tornio <- lm(doy ~ year + I(year^2), data=tornio)
AIC(poly.tornio)



# ============================
# = Set up Suwa for Plotting =
# ============================
# suwa.Polypred <- fitted(poly.suwa)
suwa[,"poly.pred"] <- predict(poly.suwa, newdata=suwa)[,1]

suwa.y <- suwa
suwa.y[suwa.no.ice ,"doy"] <- NA
!suwa.no.ice
suwa.isolObs.rle <- rle(as.integer(!is.na(suwa.y[,"doy"]) & !suwa.no.ice)) # rle for "isolated" (surrounded by NA) observations
suwa.isolObs.rleLogic <- suwa.isolObs.rle$lengths==1 & suwa.isolObs.rle$values==1
suwa.isolObs <- rep(suwa.isolObs.rleLogic, times=suwa.isolObs.rle$lengths)


# ==============================
# = Set up Tornio for Plotting =
# ==============================
tornio[,"poly.pred"] <- as.numeric(predict(poly.tornio, newdata=tornio))



# ========================================
# = Plot Time Series with Polynomial Fit =
# ========================================
myGray <- rgb(t(col2rgb("black", alpha=TRUE)), alpha=75, maxColorValue=256)


png("./AncientIce/Figures/timeSeriesPoly.png", width=3.5, height=5, res=150, units="in")
# dev.new(width=3.5, height=5)
par(mfrow=c(2,1), mar=c(2, 2.25, 0.25, 0.1), oma=c(0.5, 0, 0, 0), mgp=c(1.25, 0.35, 0), tcl=-0.25, ps=9, cex=1, family="Times")


# Plot Suwa
# Suwa Time Series
plot(suwa.y[,"year"], suwa.y[,"doy"], type="l", xlab="", ylab="Ice Formation Day of Year")

# Suwa 'isolated' years and no-freeze years
points(suwa.y[suwa.no.ice,"year"], rep(max.suwa, sum(suwa.no.ice)), pch=23, bg=myGray, col=NA, cex=0.9)
points(suwa.y[suwa.isolObs,"year"], suwa.y[suwa.isolObs,"doy"], pch=20, cex=0.25)

# Suwa polynomial trend
lines(suwa.y[,"year"], suwa.y[,"poly.pred"], lwd=3)
lines(suwa.y[,"year"], suwa.y[,"poly.pred"], lwd=1, col="white")


# Plot Tornio
# Tornio Time Series
plot(tornio[,"year"], tornio[,"doy"], type="l", xlab="", ylab="Ice Breakup Day of Year")
mtext("Year", side=1, line=1.25)

# Tornio Polynomial trend
lines(tornio[,"year"], tornio[,"poly.pred"], lwd=3)
lines(tornio[,"year"], tornio[,"poly.pred"], lwd=1, col="white")

# Close device
dev.off()