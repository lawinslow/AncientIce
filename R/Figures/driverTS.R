

# ===============
# = Set Options =
# ===============
n.boot <- 1E3


# ==================
# = Load Functions =
# ==================
func.location <- "/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/R/Functions"
invisible(sapply(paste(func.location, list.files(func.location), sep="/"), source, .GlobalEnv))


# ==================
# = Load Libraries =
# ==================
library(VGAM)
library(plyr)


# =============
# = Load Data =
# =============
load("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/Results/tornioBP.RData")
load("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/Results/suwaBP.RData")
load("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/Results/deltaDrivers.RData")

suwa.preds <- c("year", "co2", "enso", "air.t.as", "aod")
tornio.preds <- c("year", "co2", "nao.djfm", "air.t.mam", "aod", "sunspots")

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






