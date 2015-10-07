
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

# ==========
# = Option =
# ==========
residType <- c("pearson", "response")[2]


# ========
# = Plot =
# ========
png("./Figures/normalityResiduals.png", width=3, height=6, res=150, units="in")

par(mfrow=c(3,1))

qqnorm(residuals(suwa.tobit.early, type=residType), main="Suwa Early\nNormal QQ")
qqline(residuals(suwa.tobit.early, type=residType), main="Suwa Early\nNormal QQ")

qqnorm(residuals(suwa.tobit.late, type=residType), main="Suwa Late\nNormal QQ")
qqline(residuals(suwa.tobit.late, type=residType), main="Suwa Late\nNormal QQ")


qqnorm(residuals(tornio.bp.fit.out, type=residType), main="Normal QQ for Tornio")
qqline(residuals(tornio.bp.fit.out, type=residType), main="Normal QQ for Tornio")

dev.off() 