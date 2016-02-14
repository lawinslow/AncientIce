
# ==================
# = Load Libraries =
# ==================
library(VGAM)


# ==========
# = Set WD =
# ==========
if(Sys.info()[["user"]]%in%c("ryanb","Battrd")){
	setwd("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce") # for ryan
}


# ================
# = Load Results =
# ================
load("./Results/tornioBP.RData")
load("./Results/Suwa_BeforeAfter.RData")


# =========================
# = Tornio BP Predictions =
# =========================
# Calculate slopes in Tornio ice date using a "continuous" segmented regression
tornio[,"year3"] <- tornio[,"year"] - tornio.bp
tt.year <- lm(doy ~ year + pmax(I(year-tornio.bp), 0), data=tornio)
tty.pred <- fitted(tt.year)
tornio[,"bp.pred"] <- as.numeric(predict(tt.year, newdata=tornio))


# ===============================================
# = Plot Time Series Overlain w/ BP Predictions =
# ===============================================

# png("./Figures/timeSeriesBP.png", width=3.5, height=5, res=150, units="in")
# tiff("./Figures/timeSeriesBP.tiff", width=3.5, height=5, res=600, units="in", compression = "lzw")
pdf("./Figures/timeSeriesBP.pdf", width=3.5, height=5)
par(mfrow=c(2,1), mar=c(2, 2.25, 0.25, 0.1), oma=c(0.5, 0, 0, 0), mgp=c(1.25, 0.35, 0), tcl=-0.25, ps=9, cex=1)

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

plot(suwa[suwa.early.index,c("year","doy")], ylab="Ice Freeze Day of Year", xlab="", xlim=range(suwa[,"year"], na.rm=T), ylim=range(suwa[,"doy"], na.rm=T), pch=suwa.pch)
plot.suwaFit(suwa.ci.early)

points(suwa[suwa[,"period"]=="extremeOnly"&suwa[,"no.ice"]==1,c("year","doy")], pch=suwa.pch)
points(suwa[suwa[,"period"]=="extremeOnly"&suwa[,"no.ice"]==0,c("year","doy")], col="gray", pch=suwa.pch)

points(suwa[suwa.late.index,c("year","doy")], pch=suwa.pch)
plot.suwaFit(suwa.ci.late)

points(suwa[suwa[,"no.ice"]==1,c("year","doy")], pch=suwa.pch, cex=0.25, col="white")

text(min(suwa[,"year"]), max(suwa[,"doy"], na.rm=TRUE), "A", font=2, cex=1)



tornio.doy <- tornio[,"doy"]
tornio.year <- tornio[,"year"]
plot(tornio.year, tornio.doy, ylim=c(min(tornio.doy, na.rm=T), max.tornio), pch=suwa.pch, ylab="Ice Breakup Day of Year", xlab="")

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
