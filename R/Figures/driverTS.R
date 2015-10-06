

# ==========
# = Set WD =
# ==========
if(Sys.info()[["user"]]%in%c("ryanb","Battrd")){
	setwd("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce") # for ryan
}


# ==================
# = Load Functions =
# ==================
func.location <- "R/Functions"
invisible(sapply(paste(func.location, list.files(func.location), sep="/"), source, .GlobalEnv))


# ==================
# = Load Libraries =
# ==================
library(VGAM)
library(plyr)


# =============
# = Load Data =
# =============
load("./Results/tornioBP.RData")
load("./Results/Suwa_BeforeAfter.RData")
load("./Results/deltaDrivers.RData")


# # ==================
# # = Plot Funky NAO =
# # ==================
# png("~/Desktop/NAO.png", width=4.5, height=2.5, res=300, units="in")
# par(mar=c(2.25,2.0,0.1,0.1), mgp=c(1.25,0.15,0), tcl=-0.15, ps=8, family="Times")
# plot(tornio[,"year"], tornio[,"nao.djfm"], type="l", xlab="year", ylab="NAO (djfm)")
# dev.off()


# ===============================
# = Plot all driver time series =
# ===============================
dev.new(width=5, height=7)
par(mfcol=c(6,2), mar=c(1.5, 1.5, 0.1, 0.1), ps=8, mgp=c(0.75, 0.15, 0), tcl=-0.15, cex=1, family="Times")

plot(suwa[,"year"], suwa[,"doy"], type="l", xlab="", ylab="Ice Formation")
for(i in 1:5){
	if(suwa.preds[i]=="year"){next}
	plot(suwa[,"year"], suwa[,suwa.preds[i]], type="l", xlab="", ylab=suwa.preds[i])
}
plot(1, xaxt="n", yaxt="n", xlab="", ylab="", type="n", bty="n")

par(mar=c(1.75, 1.5, 0.1, 0.1))
plot(tornio[,"year"], tornio[,"doy"], type="l", xlab="", ylab="Ice Breakup")
for(i in 1:6){
	if(tornio.preds[i]=="year"){next}
	plot(tornio[,"year"], tornio[,tornio.preds[i]], type="l", xlab="", ylab=tornio.preds[i])
}




# =====================================================
# = Function To Plot Ice Date and Transformed Drivers =
# =====================================================
plot.driverIce <- function(data, before.i, after.i, driver.col, driver.name, lake.name, before.col="blue", after.col="red"){

	# Functions for Plotting Schema
	plot.befAft.ts <- function(before.y, after.y, ylab=""){
		xlim <- range(both.years)
		ylim <- range(c(before.y, after.y), na.rm=T)
		plot(before.years, before.y, xlim=xlim, ylim=ylim, col=before.col, ylab=ylab, xlab="year", pch=20)
		points(after.years, after.y, col=after.col, pch=20)
	}

	plot.doubleScatter <- function(x.before, y.before, x.after, y.after, ylab="", xlab=""){
		plot(x.before, y.before, ylab=ylab, xlab=xlab, col=before.col)
		plot(x.after, y.after, ylab=ylab, xlab=xlab, col=after.col)
	}


	# Set up Plot
	plot.dim <- matrix(c(0,0,1,1, 2,2,3,4, 5,5,6,7, 8,8,9,10), nrow=4, byrow=T)

	layout(plot.dim)
	par(mar=c(1.75,1.75,0.5,0.1), oma=c(0.1, 0.1, 0.1, 0.1), cex=1, ps=8, mgp=c(0.75,0.05,0), tcl=-0.1)

	# get Suwa before and after years
	before.years <- suwa[before.i, c("year")]
	after.years <- suwa[after.i, c("year")]
	both.years <- c(before.years, after.years)

	# Plot normal suwa ice-on dates
	before.doy <- suwa[before.i, c("doy")]
	after.doy <- suwa[after.i, c("doy")]
	doy <- c(before.doy, after.doy)



	# Plot Ice Time Series
	plot.befAft.ts(before.doy, after.doy, ylab="Ice Date")
	mtext(paste(lake.name), side=3, line=0, font=2)

	# Plot Driver Time Series
	driver.before <- suwa[before.i, c(driver.col)]
	driver.after <- suwa[after.i, c(driver.col)]
	driver <- c(driver.before, driver.after)
	plot.befAft.ts(driver.before, driver.after, ylab="Raw Driver")
	mtext(driver.name, side=3, line=0, font=2)

	# Scatter Plot of Ice vs Driver
	plot.doubleScatter(driver.before, before.doy, driver.after, after.doy, ylab="Ice Date", xlab="Driver")

	# Plot Detrended Driver Time Series
	driver.before.detrend <- detrend(driver.before)
	driver.after.detrend <- detrend(driver.after)
	driver.detrend <- c(driver.before.detrend, driver.after.detrend)
	plot.befAft.ts(driver.before.detrend, driver.after.detrend, ylab="Detrended Driver")

	# Detrend Ice Date
	before.doy.detrend <- detrend(before.doy, "tobit")
	after.doy.detrend <- detrend(after.doy, "tobit")
	doy.detrend <- c(before.doy.detrend, after.doy.detrend)

	# Plot Detrended Ice Date vs Detrended Driver
	plot.doubleScatter(driver.before.detrend, before.doy.detrend, driver.after.detrend, after.doy.detrend, ylab="Detrended Ice Date", xlab="Detrended Driver")

	# Detrended and Scaled Driver Time Series 
	driver.before.detrend.scale <- scale(driver.before.detrend)
	driver.after.detrend.scale <- scale(driver.after.detrend)
	driver.detrend.scale <- c(driver.before.detrend.scale, driver.after.detrend.scale)
	plot.befAft.ts(driver.before.detrend.scale, driver.after.detrend.scale, ylab="Detrended and Scaled Driver")

	# Plot Detrended Ice Date vs Detended and Scaled Driver
	plot.doubleScatter(driver.before.detrend.scale, before.doy.detrend, driver.after.detrend.scale, after.doy.detrend, ylab="Detrended Ice Date", xlab="Detrended and Scaled Driver")
}



# Plot Suwa Ice and Kyoto Air T
plot.driverIce(data = suwa, before.i = suwa.before.i, after.i = suwa.after.i, driver.col = "airt.march.kyoto", driver.name = "Kyoto Air T", lake.name = "Lake Suwa", before.col = "blue", after.col = "red")


