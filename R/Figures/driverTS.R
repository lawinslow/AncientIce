

# ==========
# = Set WD =
# ==========
if(Sys.info()[["user"]]%in%c("ryanb","Battrd")){
	setwd("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce") # for ryan
}


# ===============
# = Set Options =
# ===============
n.boot <- 1E3


# ==================
# = Load Functions =
# ==================
func.location <- "./R/Functions"
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
load("./Results/suwaBP.RData")
load("./Results/deltaDrivers.RData")


# ==================
# = Plot Funky NAO =
# ==================
png("~/Desktop/NAO.png", width=4.5, height=2.5, res=300, units="in")
par(mar=c(2.25,2.0,0.1,0.1), mgp=c(1.25,0.15,0), tcl=-0.15, ps=8, family="Times")
plot(tornio[,"year"], tornio[,"nao.djfm"], type="l", xlab="year", ylab="NAO (djfm)")
dev.off()


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







