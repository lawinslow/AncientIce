
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



