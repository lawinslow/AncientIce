

# ==================
# = Load Libraries =
# ==================
library(VGAM)
library(rgenoud)
library(MCMCpack)


# ==========
# = Set WD =
# ==========
if(Sys.info()[["user"]]%in%c("ryanb","Battrd")){
	setwd("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce") # for ryan
}


# ==================
# = Load Functions =
# ==================
func.location <- "./R/Functions"
invisible(sapply(paste(func.location, list.files(func.location), sep="/"), source, .GlobalEnv))


# =============
# = Load Data =
# =============
suwa <- read.table("./Data/suwa.tsv", sep="\t", header=TRUE)
max.suwa <- max(suwa[,"doy"], na.rm=TRUE)
min.suwa <- -Inf #min(suwa[,"doy"], na.rm=TRUE)

suwa.no.ice <- suwa[,"no.ice"]==1L & !is.na(suwa[,"no.ice"]) # indexes years known to not freeze (which also means that we can't know the "doy" [feeze date] b/c it doesn't exist)
suwa[suwa.no.ice ,"doy"] <- max.suwa # for no freeze years; note that the answer is the same if we replace with (max.suwa) vs. with (max.suwa+100) [or any other positive increase]

# suwa.nodoy.freeze <- suwa[,"no.ice"]==0L & !is.na(suwa[,"no.ice"]) & is.na(suwa[,"doy"]) # indexes years
# suwa[suwa.nodoy.freeze,"doy"] <- min.suwa


suwa[,"year2"] <- 1:nrow(suwa)


# =====================
# = Define Early Late =
# =====================
suwa.early.year <- c(min(suwa[,"year"]), 1683)
suwa.late.year <- c(1923, max(suwa[,"year"]))

suwa.early.index <- suwa[,"year"]<=suwa.early.year[2]
suwa.late.index <- suwa[,"year"]>=suwa.late.year[1]

suwa[suwa.early.index,"period"] <- "early"
suwa[suwa.late.index,"period"] <- "late"
suwa[!suwa.early.index & !suwa.late.index,"period"] <- "extremeOnly"


# =======================
# = Regression on Early =
# =======================
suwa.tobit.early <- vglm(doy~year, tobit(Lower=min.suwa, Upper=max.suwa), na.action=na.exclude, data=suwa[suwa.early.index,])


# ======================
# = Regression on Late =
# ======================
suwa.tobit.late <- vglm(doy~year, tobit(Lower=min.suwa, Upper=max.suwa), na.action=na.exclude, data=suwa[suwa.late.index,])


# ==================
# = Calculate CI's =
# ==================


get.suwa.ci <- function(model, data){
	# year0 <- model@x[,"year"]
	# year <- seq(min(year0), max(year0))  #suwa[suwa[,"period"]==period,"year"]
	year <- data[,"year"]
	
	# ci0 <- data.frame("year"=year0, "doy"=model@y)
	# ci <- merge(data.frame(year=year), ci0, all=T) #data.frame("year"=year, "doy"=model@y)
	ci <- data.frame(year=year, doy=data[,"doy"])
	
	newdata <- data.frame("year"=year)
	
	se.fit <- data.frame(predict(model, newdata=newdata, se.fit=TRUE, na.action=na.exclude)$se.fit)
	
	se.fit[,"year"] <- year #[as.integer(row.names(se.fit))]
	se.fit[,"fitted"] <- predict(model, newdata=newdata, se.fit=TRUE)$fitted.values[,1]
	se.fit[,"se"] <- se.fit[,2]
	se.fit[,"upr"] <- se.fit[,"fitted"] + se.fit[,1]*1.96
	se.fit[,"lwr"] <- se.fit[,"fitted"] - se.fit[,1]*1.96
	
	ci <- merge(ci, se.fit, by="year", all=TRUE)
	
	return(ci)
}

suwa.ci.early <- get.suwa.ci(suwa.tobit.early, suwa[suwa.early.index,])
suwa.ci.late <- get.suwa.ci(suwa.tobit.late, suwa[suwa.late.index,])


# ================================
# = Suwa Slopes -- Before, After =
# ================================
(suwa.slope.early <- suwa.tobit.early@coefficients["year"])
cat("Early period:",(suwa.slope.early * 10), "days per decade")
# Early period: 0.1934544 days per decade

(suwa.slope.late <- suwa.tobit.late@coefficients["year"])
cat("Late period:",(suwa.slope.late * 10), "days per decade")
# Late period: 4.606225 days per decade

# =============================
# = Save Before After Results =
# =============================
save(suwa, suwa.tobit.early, suwa.tobit.late, suwa.ci.early, suwa.ci.late, suwa.early.index, suwa.late.index, min.suwa, max.suwa, file="./Results/Suwa_BeforeAfter.RData")

