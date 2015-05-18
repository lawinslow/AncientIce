
# ==================
# = Load Libraries =
# ==================
library(zoo)


# ==========
# = Set WD =
# ==========
setwd("/Users/Battrd/Documents/School&Work/WiscResearch") # for ryan


# ===========
# = Options =
# ===========
se.extend <- qnorm(0.025, lower.tail=FALSE)


# =============
# = Load Data =
# =============
load("./AncientIce/Results/deltaDrivers.RData")
iceTobit <- iceTobit[iceTobit[,"variable"]!="year",]
iceTobit.s <- iceTobit.s[iceTobit.s[,"variable"]!="year",]
iceTobit.t <- iceTobit.t[iceTobit.t[,"variable"]!="year",]

load("./AncientIce/Results/tornioBP.RData")


# ============
# = Drop AOD =
# ============
iceTobit.s <- iceTobit.s[!iceTobit.s[,"variable"]%in%c("aod"),]

iceTobit.t <- iceTobit.t[!iceTobit.t[,"variable"]%in%c("aod"),]


# ==================
# = Load Functions =
# ==================
func.location <- "./AncientIce/R/Functions"
invisible(sapply(paste(func.location, list.files(func.location), sep="/"), source, .GlobalEnv))


# ===========================
# = plot tobit coefficients =
# ===========================
# dev.new(width=3.5, height=5)
png("./AncientIce/Figures/deltaDrivers.png", width=3.5, height=5, res=150, units="in")
par(mfrow=c(2,1), mar=c(2.5, 3, 0.5, 0.5), mgp=c(1.5, 0.5, 0), tcl=-0.15, ps=9, cex=1, family="Times")


# =========================
# = Plot just Suwa coeffs =
# =========================
iceTobit.s[,"estimate"] <- as.numeric(iceTobit.s[,"estimate"])
iceTobit.s[,"stdE"] <- as.numeric(iceTobit.s[,"stdE"])
iceTobit.s[,"Z"] <- as.numeric(iceTobit.s[,"Z"])

iceTobit.s <- iceTobit.s[order(iceTobit.s[,"water"], iceTobit.s[,"variable"]),]
XL.s <- c(
	"air.t.as"=bquote(Air~~phantom()*degree*C), # august september air temperature (where?)
	# "aod"=bquote(AOD), # aerosol optical depth, where?
	"co2"=bquote(CO[2]), # where?
	"enso"=bquote(ENSO) # el niño southern oscillation
)

ity0.s <- iceTobit.s[,"estimate"]-iceTobit.s[,"stdE"]*se.extend # y0's for arrows()
ity1.s <- iceTobit.s[,"estimate"]+iceTobit.s[,"stdE"]*se.extend # y1's for arrows()
it.ylim.l.s <- min(ity0.s)
it.ylim.u.s <- max(ity1.s)
it.ylim.s <- c(it.ylim.l.s, it.ylim.u.s)

adj.x0.s <- (1:nrow(iceTobit.s))%%2
adj.x0.s[1] <- 0
adj.x.s <- cumsum(adj.x0.s*2)
xvals.s <- (1:nrow(iceTobit.s))+adj.x.s
labloc.s <- rollapply(xvals.s, width=2, by=2, mean)

plot(xvals.s, iceTobit.s[,"estimate"], ylim=it.ylim.s, col=c("blue","red"), pch=19, xaxt="n", xlab="", ylab="Suwa\nCoefficient")
# text(labloc.s, y=it.ylim.l.s*1.4, unique(iceTobit.s[,"variable"]), xpd=TRUE)
text(labloc.s, y=it.ylim.l.s*1.4, parse(text=XL.s), xpd=TRUE)
axis(side=1, at=labloc.s, labels=FALSE)
abline(h=0, lty="dotted")
text(1, 11, "A", font=2, cex=1)
legend("topright", legend=c("1581 – 1681", "1897 – 1997"), text.col=c("blue","red"), bty="n", inset=c(0,-0.065))
arrows(x0=xvals.s, y0=rep(iceTobit.s[,"estimate"],2), x1=xvals.s, y1=c(ity0.s,ity1.s), length=0.05, col=c("blue","red"), angle=90)
points(xvals.s, iceTobit.s[,"estimate"], col=c("white"), pch=c(NA, 8)[(iceTobit.s[,"diff.Pval"]<0.05)+1], cex=0.6)



# =================================
# = Plot just Tornio coefficients =
# =================================
iceTobit.t[,"estimate"] <- as.numeric(iceTobit.t[,"estimate"])
iceTobit.t[,"stdE"] <- as.numeric(iceTobit.t[,"stdE"])
iceTobit.t[,"Z"] <- as.numeric(iceTobit.t[,"Z"])

iceTobit.t <- iceTobit.t[order(iceTobit.t[,"water"], iceTobit.t[,"variable"]),]

XL.t <- c(
	"air.t.mam"=bquote(Air~~phantom()*degree*C), # august september air temperature (where?)
	# "aod"=bquote(AOD), # aerosol optical depth, where?
	"co2"=bquote(CO[2]), # where?
	"nao.djfm"=bquote(NAO), # north atlantic oscillation in december january february march
	"sunspots"=bquote(Sunspots) # number of sunspots
)

ity0.t <- iceTobit.t[,"estimate"]-iceTobit.t[,"stdE"]*se.extend # y0's for arrows()
ity1.t <- iceTobit.t[,"estimate"]+iceTobit.t[,"stdE"]*se.extend # y1's for arrows()
it.ylim.l.t <- min(ity0.t)
it.ylim.u.t <- max(ity1.t)
it.ylim.t <- c(it.ylim.l.t, it.ylim.u.t)

adj.x0.t <- (1:nrow(iceTobit.t))%%2
adj.x0.t[1] <- 0
adj.x.t <- cumsum(adj.x0.t*2)
xvals.t <- (1:nrow(iceTobit.t))+adj.x.t
labloc.t <- rollapply(xvals.t, width=2, by=2, mean)

plot(xvals.t, iceTobit.t[,"estimate"], ylim=it.ylim.t, col=c("blue","red"), pch=19, xaxt="n", xlab="", ylab="Torne\nCoefficient")
# text(labloc.t, y=it.ylim.l.t*1.2, unique(iceTobit.t[,"variable"]), xpd=TRUE)
text(labloc.t, y=it.ylim.l.t*1.125, parse(text=XL.t), xpd=TRUE)
axis(side=1, at=labloc.t, labels=FALSE)
abline(h=0, lty="dotted")
tornio.range1 <- paste0(min(tornio[tornio.bp.i,"year"]), " – ", max(tornio[tornio.bp.i,"year"]))
tornio.range2 <- paste0(min(tornio[!tornio.bp.i,"year"]), " – ", max(tornio[!tornio.bp.i,"year"]))
text(1, 0.25, "B", font=2)
legend("bottomright", legend=c(tornio.range1, tornio.range2), text.col=c("blue", "red"), bty="n", inset=c(0,-0.025))
# legend("topright", legend=c("1803 – 1866", "1937 – 2000"), text.col=c("blue", "red"), bty="n", inset=c(-0.02,-0.07))
arrows(x0=xvals.t, y0=rep(iceTobit.t[,"estimate"],2), x1=xvals.t, y1=c(ity0.t,ity1.t), length=0.05, col=c("blue","red"), angle=90)
points(xvals.t, iceTobit.t[,"estimate"], col=c("white"), pch=c(NA, 8)[(iceTobit.t[,"diff.Pval"]<0.05)+1], cex=0.6)


dev.off()
