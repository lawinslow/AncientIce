
# ==================
# = Load Libraries =
# ==================
library(VGAM)


# ==========
# = Set WD =
# ==========
setwd("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce") # for ryan


# ================
# = Load Results =
# ================
load("./Results/tornioBP.RData")
# load("./AncientIce/Results/suwaBP.RData")
load("./Results/Suwa_BeforeAfter.RData")


# =======================
# = Suwa BP Predictions =
# =======================
# Calculate slopes in Suwa ice date using a "continuous" segmented regression
# suwa[,"year3"] <- suwa[,"year"] - suwa.bp
# ts.year <- vglm(doy ~ year + pmax(I(year-suwa.bp), 0) , tobit(Lower=min.suwa, Upper=max.suwa), data=suwa) # use for 1812
# # ts.year <- vglm(doy ~ year:suwa.bp.i, tobit(Lower=min.suwa, Upper=max.suwa), data=suwa) # use for 1886
# tsy.pred <- fitted(ts.year)
# suwa[,"bp.pred"] <- predict(ts.year, newdata=suwa)[,1]
# suwa.y <- suwa
# suwa.y[suwa.no.ice ,"doy"] <- NA
# !suwa.no.ice
# suwa.isolObs.rle <- rle(as.integer(!is.na(suwa.y[,"doy"]) & !suwa.no.ice)) # rle for "isolated" (surrounded by NA) observations
# suwa.isolObs.rleLogic <- suwa.isolObs.rle$lengths==1 & suwa.isolObs.rle$values==1
# suwa.isolObs <- rep(suwa.isolObs.rleLogic, times=suwa.isolObs.rle$lengths)




# =========================
# = Tornio BP Predictions =
# =========================
# Calculate slopes in Tornio ice date using a "continuous" segmented regression
tornio[,"year3"] <- tornio[,"year"] - tornio.bp
# tt.year <- vglm(doy ~ year + pmax(I(year-tornio.bp), 0) , tobit(Lower=min.tornio, Upper=max.tornio), data=tornio) # 1807
tt.year <- lm(doy ~ year + pmax(I(year-tornio.bp), 0), data=tornio)
# tt.year <- vglm(doy ~ year:tornio.bp.i , tobit(Lower=min.tornio, Upper=max.tornio), data=tornio) # 1886
tty.pred <- fitted(tt.year)
tornio[,"bp.pred"] <- as.numeric(predict(tt.year, newdata=tornio))


# ===============================================
# = Plot Time Series Overlain w/ BP Predictions =
# ===============================================
# Plot both Suwa and Tornio
# dev.new(width=3.5, height=5)

# myRed <- rgb(t(col2rgb("red", alpha=TRUE)), alpha=75, maxColorValue=256)
# myBlue <- rgb(t(col2rgb("blue", alpha=TRUE)), alpha=75, maxColorValue=256)
# sNFcc <- c(myBlue,myRed)[(!suwa.bp.i[suwa.no.ice])+1] # suwa No Freeze color code

png("./Figures/timeSeriesBP.png", width=3.5, height=5, res=150, units="in")
par(mfrow=c(2,1), mar=c(2, 2.25, 0.25, 0.1), oma=c(0.5, 0, 0, 0), mgp=c(1.25, 0.35, 0), tcl=-0.25, ps=9, cex=1)

# plot(suwa.y[,"year"], suwa.y[,"doy"], type="l", xlab="", ylab="Ice Formation Day of Year")
# lines(suwa.y[suwa.bp.i,"year"], suwa.y[suwa.bp.i,"bp.pred"], col="blue", lwd=3)
# lines(suwa.y[!suwa.bp.i,"year"], suwa.y[!suwa.bp.i,"bp.pred"], col="red", lwd=3)
# abline(v=suwa.bp, lty="dashed", lwd=1)
# points(suwa.y[suwa.no.ice,"year"], rep(max.suwa, sum(suwa.no.ice)), pch=23, bg=sNFcc, col=NA, cex=0.9)
# points(suwa.y[suwa.isolObs,"year"], suwa.y[suwa.isolObs,"doy"], pch=20, cex=0.25) # plot observations that have a missing or no-freeze year on either side of it (thus would not show up as line, b/c line must have at least 2 observations in a row)

# suwa.doy <- suwa[,"doy"]
# suwa.year <- suwa[,"year"]
# suwa.doyNA <- suwa.doy
# suwa.doyNA[suwa.doy>=max.suwa] <- NA
# plot(suwa.year, suwa.doyNA, col="lightgray", ylim=c(min(suwa.doy, na.rm=T), max.suwa), type="l")
# points(suwa.year, suwa.doy, pch=20)

plot.suwaFit <- function(ci){
	
	lp <- c("fitted","upr","lwr")
	lc <- c("black","blue","blue")
	
	# for(i in 0:1){
		for(j in 1:3){
			lines(ci[,"year"], ci[,lp[j]], col=lc[j], lwd=2)
		}
	# }
	
}

suwa.pch <- 20

plot(suwa[suwa.early.index,c("year","doy")], ylab="Ice Formation Day of Year", xlab="", xlim=range(suwa[,"year"], na.rm=T), ylim=range(suwa[,"doy"], na.rm=T), pch=suwa.pch)
plot.suwaFit(suwa.ci.early)

points(suwa[suwa[,"period"]=="extremeOnly"&suwa[,"no.ice"]==1,c("year","doy")], pch=suwa.pch)
points(suwa[suwa[,"period"]=="extremeOnly"&suwa[,"no.ice"]==0,c("year","doy")], col="gray", pch=suwa.pch)

points(suwa[suwa.late.index,c("year","doy")], pch=suwa.pch)
plot.suwaFit(suwa.ci.late)

text(min(suwa[,"year"]), max(suwa[,"doy"], na.rm=TRUE), "A", font=2, cex=1)


#
# sy <- suwa.y[,"year"]
# syr <- range(sy)
# syD <- density(sy[suwa.no.ice], from=syr[1], to=syr[2], adjust=1)
# syBPi <- syD$x < suwa.bp
#
# par(new=TRUE)
# plot(syD$x[syBPi], syD$y[syBPi], ylab="", xlab="", xlim=syr, type="l", col=myBlue, ylim=range(stD$y), xaxt="n", yaxt="n", main="")
# lines(syD$x[!syBPi], syD$y[!syBPi], ylab="", xlab="", col=myRed)


# plot(tornio[,"year"], tornio[,"doy"], type="l", xlab="", ylab="Ice Breakup Day of Year")
# mtext("Year", side=1, line=1.25)
# lines(tornio[tornio.bp.i,"year"], tornio[tornio.bp.i,"bp.pred"], col="blue", lwd=3)
# lines(tornio[!tornio.bp.i,"year"], tornio[!tornio.bp.i,"bp.pred"], col="red", lwd=3)
# abline(v=tornio.bp, lty="dashed", lwd=1)



tornio.doy <- tornio[,"doy"]
tornio.year <- tornio[,"year"]
# plot(tornio.year, tornio.doy, col="lightgray", ylim=c(min(tornio.doy, na.rm=T), max.tornio), type="l")
plot(tornio.year, tornio.doy, ylim=c(min(tornio.doy, na.rm=T), max.tornio), pch=suwa.pch, ylab="Ice Breakup Day of Year", xlab="")
# points(tornio.year, tornio.doy, pch=20)

plot.tornioFit <- function(tornio.ci){
	
	lp <- c("fitted","upr","lwr")
	lc <- c("black","blue","blue")
	
	for(j in 1:3){
		lines(tornio.ci[,"year"], tornio.ci[,lp[j]], col=lc[j], lwd=2)
	}
	
}

plot.tornioFit(tornio.ci)

text(min(tornio[,"year"]), max(tornio[,"doy"], na.rm=TRUE), "B", font=2, cex=1)

mtext("Year", side=1, line=1.25)

dev.off()

#
# # =======================================
# # = Same as above, but with same x-axis =
# # =======================================
# # Plot both Suwa and Tornio
# dev.new(width=3.5, height=5)
# par(mfrow=c(2,1), mar=c(2, 2.25, 0.25, 0.1), oma=c(0.5, 0, 0, 0), mgp=c(1.25, 0.35, 0), tcl=-0.25, ps=9, cex=1)
#
# plot(suwa.y[,"year"], suwa.y[,"doy"], type="l", xlab="", ylab="Ice Formation Day of Year")
# lines(suwa.y[suwa.bp.i,"year"], suwa.y[suwa.bp.i,"bp.pred"], col="blue", lwd=3)
# lines(suwa.y[!suwa.bp.i,"year"], suwa.y[!suwa.bp.i,"bp.pred"], col="red", lwd=3)
# abline(v=suwa.bp, lty="dashed", lwd=1)
# points(suwa.y[suwa.no.ice,"year"], rep(max.suwa, sum(suwa.no.ice)), pch="*")
#
# plot(tornio[,"year"], tornio[,"doy"], type="l", xlab="", ylab="Ice Breakup Day of Year", xlim=range(suwa.y[,"year"], na.rm=TRUE))
# mtext("Year", side=1, line=1.25)
# lines(tornio[tornio.bp.i,"year"], tornio[tornio.bp.i,"bp.pred"], col="blue", lwd=3)
# lines(tornio[!tornio.bp.i,"year"], tornio[!tornio.bp.i,"bp.pred"], col="red", lwd=3)
# abline(v=tornio.bp, lty="dashed", lwd=1)
#
