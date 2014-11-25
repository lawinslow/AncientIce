
# ==================
# = Load Libraries =
# ==================
library(zoo)



# =============
# = Load Data =
# =============
load("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/Results/deltaDrivers.RData")


# ===========================
# = plot tobit coefficients =
# ===========================
dev.new(width=3.5, height=5)
par(mfrow=c(2,1), mar=c(2.5, 3, 0.5, 0.5), mgp=c(1.5, 0.5, 0), tcl=-0.15, ps=9, cex=1, family="Times")


# =========================
# = Plot just Suwa coeffs =
# =========================
iceTobit.s[,"estimate"] <- as.numeric(iceTobit.s[,"estimate"])
iceTobit.s[,"stdE"] <- as.numeric(iceTobit.s[,"stdE"])
iceTobit.s[,"Z"] <- as.numeric(iceTobit.s[,"Z"])

iceTobit.s <- iceTobit.s[order(iceTobit.s[,"water"], iceTobit.s[,"variable"]),]

ity0.s <- iceTobit.s[,"estimate"]-iceTobit.s[,"stdE"] # y0's for arrows()
ity1.s <- iceTobit.s[,"estimate"]+iceTobit.s[,"stdE"] # y1's for arrows()
it.ylim.l.s <- min(ity0.s)
it.ylim.u.s <- max(ity1.s)
it.ylim.s <- c(it.ylim.l.s, it.ylim.u.s)

adj.x0.s <- (1:nrow(iceTobit.s))%%2
adj.x0.s[1] <- 0
adj.x.s <- cumsum(adj.x0.s*2)
xvals.s <- (1:nrow(iceTobit.s))+adj.x.s
labloc.s <- rollapply(xvals.s, width=2, by=2, mean)

plot(xvals.s, iceTobit.s[,"estimate"], ylim=it.ylim.s, col=c("blue","red"), pch=19, xaxt="n", xlab="", ylab="Suwa\nCoefficient")
text(labloc.s, y=it.ylim.l.s*1.4, unique(iceTobit.s[,"variable"]), xpd=TRUE)
axis(side=1, at=labloc.s, labels=FALSE)
abline(h=0, lty="dotted")
legend("topleft", legend=c("1581 – 1681", "1897 – 1997"), text.col=c("blue","red"), bty="n", inset=c(-0.1,-0.065))
arrows(x0=xvals.s, y0=rep(iceTobit.s[,"estimate"],2), x1=xvals.s, y1=c(ity0.s,ity1.s), length=0.05, col=c("blue","red"), angle=90)


# =================================
# = Plot just Tornio coefficients =
# =================================
iceTobit.t[,"estimate"] <- as.numeric(iceTobit.t[,"estimate"])
iceTobit.t[,"stdE"] <- as.numeric(iceTobit.t[,"stdE"])
iceTobit.t[,"Z"] <- as.numeric(iceTobit.t[,"Z"])

iceTobit.t <- iceTobit.t[order(iceTobit.t[,"water"], iceTobit.t[,"variable"]),]

ity0.t <- iceTobit.t[,"estimate"]-iceTobit.t[,"stdE"] # y0's for arrows()
ity1.t <- iceTobit.t[,"estimate"]+iceTobit.t[,"stdE"] # y1's for arrows()
it.ylim.l.t <- min(ity0.t)
it.ylim.u.t <- max(ity1.t)
it.ylim.t <- c(it.ylim.l.t, it.ylim.u.t)

adj.x0.t <- (1:nrow(iceTobit.t))%%2
adj.x0.t[1] <- 0
adj.x.t <- cumsum(adj.x0.t*2)
xvals.t <- (1:nrow(iceTobit.t))+adj.x.t
labloc.t <- rollapply(xvals.t, width=2, by=2, mean)

plot(xvals.t, iceTobit.t[,"estimate"], ylim=it.ylim.t, col=c("blue","red"), pch=19, xaxt="n", xlab="", ylab="Tornio\nCoefficient")
text(labloc.t, y=it.ylim.l.t*1.2, unique(iceTobit.t[,"variable"]), xpd=TRUE)
axis(side=1, at=labloc.t, labels=FALSE)
abline(h=0, lty="dotted")
legend("topright", legend=c("1803 – 1866", "1937 – 2000"), text.col=c("blue", "red"), bty="n", inset=c(-0.02,-0.07))
arrows(x0=xvals.t, y0=rep(iceTobit.t[,"estimate"],2), x1=xvals.t, y1=c(ity0.t,ity1.t), length=0.05, col=c("blue","red"), angle=90)



