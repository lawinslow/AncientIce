
# ==================
# = Load Libraries =
# ==================
library(VGAM)


# ================
# = Load Results =
# ================
load("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/Results/tornioBP.RData")
load("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/Results/suwaBP.RData")


# =======================
# = Suwa BP Predictions =
# =======================
# Calculate slopes in Suwa ice date using a "continuous" segmented regression
suwa[,"year3"] <- suwa[,"year"] - suwa.bp
ts.year <- vglm(doy ~ year + pmax(I(year-suwa.bp), 0) , tobit(Lower=min.suwa, Upper=max.suwa), data=suwa) # use for 1812
# ts.year <- vglm(doy ~ year:suwa.bp.i, tobit(Lower=min.suwa, Upper=max.suwa), data=suwa) # use for 1886
tsy.pred <- fitted(ts.year)
suwa[,"bp.pred"] <- predict(ts.year, newdata=suwa)[,1]
suwa.y <- suwa
suwa.y[suwa.no.ice ,"doy"] <- NA


# =========================
# = Tornio BP Predictions =
# =========================
# Calculate slopes in Tornio ice date using a "continuous" segmented regression
tornio[,"year3"] <- tornio[,"year"] - tornio.bp
tt.year <- vglm(doy ~ year + pmax(I(year-tornio.bp), 0) , tobit(Lower=min.tornio, Upper=max.tornio), data=tornio) # 1807
# tt.year <- vglm(doy ~ year:tornio.bp.i , tobit(Lower=min.tornio, Upper=max.tornio), data=tornio) # 1886
tty.pred <- fitted(tt.year)
tornio[,"bp.pred"] <- predict(tt.year, newdata=tornio)[,1]


# ===============================================
# = Plot Time Series Overlain w/ BP Predictions =
# ===============================================
# Plot both Suwa and Tornio
# dev.new(width=3.5, height=5)
png("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/Figures/timeSeriesBP.png", width=3.5, height=5, res=150, units="in")
par(mfrow=c(2,1), mar=c(2, 2.25, 0.25, 0.1), oma=c(0.5, 0, 0, 0), mgp=c(1.25, 0.35, 0), tcl=-0.25, ps=9, cex=1, family="Times")

plot(suwa.y[,"year"], suwa.y[,"doy"], type="l", xlab="", ylab="Ice Formation Day of Year")
lines(suwa.y[suwa.bp.i,"year"], suwa.y[suwa.bp.i,"bp.pred"], col="blue", lwd=3)
lines(suwa.y[!suwa.bp.i,"year"], suwa.y[!suwa.bp.i,"bp.pred"], col="red", lwd=3)
abline(v=suwa.bp, lty="dashed", lwd=1)
points(suwa.y[suwa.no.ice,"year"], rep(max.suwa, sum(suwa.no.ice)), pch="*")

plot(tornio[,"year"], tornio[,"doy"], type="l", xlab="", ylab="Ice Breakup Day of Year")
mtext("Year", side=1, line=1.25)
lines(tornio[tornio.bp.i,"year"], tornio[tornio.bp.i,"bp.pred"], col="blue", lwd=3)
lines(tornio[!tornio.bp.i,"year"], tornio[!tornio.bp.i,"bp.pred"], col="red", lwd=3)
abline(v=tornio.bp, lty="dashed", lwd=1)

dev.off()


# =======================================
# = Same as above, but with same x-axis =
# =======================================
# Plot both Suwa and Tornio
dev.new(width=3.5, height=5)
par(mfrow=c(2,1), mar=c(2, 2.25, 0.25, 0.1), oma=c(0.5, 0, 0, 0), mgp=c(1.25, 0.35, 0), tcl=-0.25, ps=9, cex=1, family="Times")

plot(suwa.y[,"year"], suwa.y[,"doy"], type="l", xlab="", ylab="Ice Formation Day of Year")
lines(suwa.y[suwa.bp.i,"year"], suwa.y[suwa.bp.i,"bp.pred"], col="blue", lwd=3)
lines(suwa.y[!suwa.bp.i,"year"], suwa.y[!suwa.bp.i,"bp.pred"], col="red", lwd=3)
abline(v=suwa.bp, lty="dashed", lwd=1)
points(suwa.y[suwa.no.ice,"year"], rep(max.suwa, sum(suwa.no.ice)), pch="*")

plot(tornio[,"year"], tornio[,"doy"], type="l", xlab="", ylab="Ice Breakup Day of Year", xlim=range(suwa.y[,"year"], na.rm=TRUE))
mtext("Year", side=1, line=1.25)
lines(tornio[tornio.bp.i,"year"], tornio[tornio.bp.i,"bp.pred"], col="blue", lwd=3)
lines(tornio[!tornio.bp.i,"year"], tornio[!tornio.bp.i,"bp.pred"], col="red", lwd=3)
abline(v=tornio.bp, lty="dashed", lwd=1)
