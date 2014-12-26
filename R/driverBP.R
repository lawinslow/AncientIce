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



# ==================
# = Tornio Drivers =
# ==================

for(i in 2:length(tornio.preds)){
	t.pred <- tornio.preds[i]
	
	t.bp <- findBP(x=tornio[,"year"], y=tornio[,t.pred], fullOut=TRUE)
	
	if(i==2){
		torn.driver.bp <- cbind(driver=t.pred, t.bp)
	}else{
		torn.driver.bp <- rbind(torn.driver.bp, cbind(driver=t.pred, t.bp))
	}
}



