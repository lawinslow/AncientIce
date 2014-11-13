

library(VGAM)
library(zoo)


tornio.preds <- c("co2", "nao.djfm", "aod", "sunspots") # add air.t.mam
tornio.formula <- as.formula(paste("doy~", paste(tornio.preds, collapse="+"), sep=""))

suwa <- read.table("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/Data/suwa.tsv", sep="\t", header=TRUE)
max.suwa <- max(suwa[,"doy"], na.rm=TRUE)
min.suwa <- min(suwa[,"doy"], na.rm=TRUE)
suwa.no.ice <- suwa[,"no.ice"]==1L & !is.na(suwa[,"no.ice"])
suwa[suwa.no.ice ,"doy"] <- max.suwa
suwa[,"year2"] <- 1:nrow(suwa)



tornio <- read.table("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/Data/tornio.tsv", sep="\t", header=TRUE)
max.tornio <- max(tornio[,"doy"], na.rm=TRUE)
min.tornio <- min(tornio[,"doy"], na.rm=TRUE)
tornio[,"year2"] <- 1:nrow(tornio)


# suwa.bp <- 1842
# tornio.bp <- 1886

# suwa.bp <- 1812
# tornio.bp <- 1807


# =========================
# = Breakpoint with Tobit =
# =========================
# =========================================
# = Suwa: Calculate Breakpoint with Tobit =
# =========================================
bp.opts <- 10:(nrow(suwa)-10)
resid.vars <- rep(NA, length(bp.opts))
s.bp.pb <- txtProgressBar(min=1, max=length(bp.opts), style=3)
for(i in 1:length(bp.opts)){
	t.bp <- bp.opts[i]
	t.bp.year <- suwa[t.bp,"year"]
	t.suwa <- suwa
	t.suwa[,"bp"] <- (1:nrow(t.suwa))>=t.bp
	
	# tobit.suwa.year <- vglm(doy~year2*bp, tobit(Lower=min.suwa, Upper=max.suwa), data=t.suwa) # 1807
	# tobit.suwa.year <- vglm(doy ~ year:bp, tobit(Lower=min.suwa, Upper=max.suwa), data=t.suwa) # 1842
	tobit.suwa.year <- vglm(doy ~ year + pmax(I(year-t.bp.year), 0), tobit(Lower=min.suwa, Upper=max.suwa), data=t.suwa) # 1812
	
	
	resid.vars[i] <- exp(coef(tobit.suwa.year)[2])
	# nlls[i] <- tobit.suwa.year@criterion$loglikelihood
	
	min.sofar <- which.min(resid.vars)
	if(min.sofar!=i){
		min.tobit.suwa.year <- tobit.suwa.year
	}
	setTxtProgressBar(s.bp.pb, i)
}
suwa.bp <- suwa[bp.opts[which.min(resid.vars)],"year"]
suwa.bp
suwa.bp.i <- suwa[,"year"] < suwa.bp


# ==========
# = Tornio =
# ==========
# =========================================
# = Tornio: Calculate breakpoint with OLS =
# =========================================
bp.opts <- 1:nrow(tornio)
r2.torn <- rep(NA, length(bp.opts))
max.sofar <- NA
for(i in 1:length(bp.opts)){
	t.bp <- bp.opts[i]
	t.tornio <- tornio
	t.tornio[,"bp"] <- (1:nrow(t.tornio))>=t.bp
	# tobit.tornio.year <- lm(doy ~ year2:bp, data=t.tornio) # 1886
	# tobit.tornio.year <- lm(doy ~ year2*bp, data=t.tornio) # 1807
	tobit.tornio.year <- lm(doy ~ year + pmax(I(year-bp),0), data=t.tornio) # 1807
	r2.torn[i] <- summary(tobit.tornio.year)$r.squared
	
	max.sofar <- which.max(r2.torn)
	if(max.sofar!=i){
		max.tobit.tornio.year <- tobit.tornio.year
	}
}
tornio.bp <- tornio[bp.opts[which.max(r2.torn)],"year"]
tornio.bp
tornio.bp.i <- tornio[,"year"] < tornio.bp




# =================================
# = Breakpoint + Time Series Plot =
# =================================
# Suwa
# Calculate slopes in Suwa ice date using a "continuous" segmented regression
suwa[,"year3"] <- suwa[,"year"] - suwa.bp
ts.year <- vglm(doy ~ year + pmax(I(year-suwa.bp), 0) , tobit(Lower=min.suwa, Upper=max.suwa), data=suwa) # use for 1812
# ts.year <- vglm(doy ~ year:suwa.bp.i, tobit(Lower=min.suwa, Upper=max.suwa), data=suwa) # use for 1886
tsy.pred <- fitted(ts.year)
suwa[,"bp.pred"] <- predict(ts.year, newdata=suwa)[,1]
suwa.y <- suwa
suwa.y[suwa.no.ice ,"doy"] <- NA

# Tornio
# Calculate slopes in Tornio ice date using a "continuous" segmented regression
tornio[,"year3"] <- tornio[,"year"] - tornio.bp
tt.year <- vglm(doy ~ year + pmax(I(year-tornio.bp), 0) , tobit(Lower=min.tornio, Upper=max.tornio), data=tornio) # 1807
# tt.year <- vglm(doy ~ year:tornio.bp.i , tobit(Lower=min.tornio, Upper=max.tornio), data=tornio) # 1886
tty.pred <- fitted(tt.year)
tornio[,"bp.pred"] <- predict(tt.year, newdata=tornio)[,1]

# Plot both Suwa and Tornio
dev.new(width=3.5, height=5)
par(mfrow=c(2,1), mar=c(2, 2.25, 0.25, 0.1), oma=c(0.5, 0, 0, 0), mgp=c(1.25, 0.35, 0), tcl=-0.25, ps=9, cex=1, family="Times")

plot(suwa.y[,"year"], suwa.y[,"doy"], type="l", xlab="", ylab="Ice Formation Day of Year")
lines(suwa.y[suwa.bp.i,"year"], suwa.y[suwa.bp.i,"bp.pred"], col="yellow", lwd=3)
lines(suwa.y[!suwa.bp.i,"year"], suwa.y[!suwa.bp.i,"bp.pred"], col="red", lwd=3)
abline(v=suwa.bp, lty="dashed", lwd=1)
points(suwa.y[suwa.no.ice,"year"], rep(max.suwa, sum(suwa.no.ice)), pch="*")

plot(tornio[,"year"], tornio[,"doy"], type="l", xlab="", ylab="Ice Breakup Day of Year")
mtext("Year", side=1, line=1.25)
lines(tornio[tornio.bp.i,"year"], tornio[tornio.bp.i,"bp.pred"], col="yellow", lwd=3)
lines(tornio[!tornio.bp.i,"year"], tornio[!tornio.bp.i,"bp.pred"], col="red", lwd=3)
abline(v=tornio.bp, lty="dashed", lwd=1)



# ===============================================
# = Suwa: Run Tobit before and after breakpoint =
# ===============================================
# suwa.1 <- suwa[suwa.bp.i,]
# suwa.2 <- suwa[!suwa.bp.i,]
suwa.before.i <- suwa[,"year"] >= 1581 & suwa[,"year"] <= 1681
suwa.after.i <- suwa[,"year"] >= 1897 & suwa[,"year"] <= 1997

suwa.1 <- suwa[suwa.before.i,]
suwa.2 <- suwa[suwa.after.i,]

suwa.preds <- c("year", "co2", "enso", "air.t.as", "aod")

for(i in 1:length(suwa.preds)){
	suwa.1[,suwa.preds[i]] <- scale(suwa.1[,suwa.preds[i]])
	suwa.2[,suwa.preds[i]] <- scale(suwa.2[,suwa.preds[i]])
}


iceTobit.s1 <- data.frame("water"=NA, "period"=NA, "variable"=NA, "estimate"=NA, "stdE"=NA, "Z"=NA)
iceTobit.s2 <- data.frame("water"=NA, "period"=NA, "variable"=NA, "estimate"=NA, "stdE"=NA, "Z"=NA)
for(i in 1:length(suwa.preds)){
	t.ts.formula <- as.formula(paste("doy~", paste(suwa.preds[i], collapse="+"), sep=""))
	
	# do the before period
	t.ts1 <- vglm(t.ts.formula, tobit(Lower=min.suwa, Upper=max.suwa), data=suwa.1)
	t.ts1.s <- summary(t.ts1)@coef3
	iceTobit.s1[i,] <- c("suwa", "before", rownames(t.ts1.s)[3], t.ts1.s[3,"Estimate"], t.ts1.s[3,"Std. Error"], t.ts1.s[3,"z value"])
	
	# do the after period
	t.ts2 <- vglm(t.ts.formula, tobit(Lower=min.suwa, Upper=max.suwa), data=suwa.2)
	t.ts2.s <- summary(t.ts2)@coef3
	iceTobit.s2[i,] <- c("suwa", "after", rownames(t.ts2.s)[3], t.ts2.s[3,"Estimate"], t.ts2.s[3,"Std. Error"], t.ts2.s[3,"z value"])
	
}
iceTobit.s <- rbind(iceTobit.s1, iceTobit.s2)


# ===============================================
# = Tornio: Run Tobit before and after breakpoint =
# ===============================================
# tornio.1 <- tornio[tornio.bp.i,]
# tornio.2 <- tornio[!tornio.bp.i,]
tornio.before.i <- tornio[,"year"] >= 1803 & tornio[,"year"] <= 1882
tornio.after.i <- tornio[,"year"] >= 1921 & tornio[,"year"] <= 2000

tornio.1 <- tornio[tornio.before.i,]
tornio.2 <- tornio[tornio.after.i,]

tornio.preds <- c("year", "co2", "nao.djfm", "air.t.stock", "aod", "sunspots")

for(i in 1:length(tornio.preds)){
	tornio.1[,tornio.preds[i]] <- scale(tornio.1[,tornio.preds[i]])
	tornio.2[,tornio.preds[i]] <- scale(tornio.2[,tornio.preds[i]])
}


iceTobit.t1 <- data.frame("water"=NA, "period"=NA, "variable"=NA, "estimate"=NA, "stdE"=NA, "Z"=NA)
iceTobit.t2 <- data.frame("water"=NA, "period"=NA, "variable"=NA, "estimate"=NA, "stdE"=NA, "Z"=NA)
for(i in 1:length(tornio.preds)){
	t.tt.formula <- as.formula(paste("doy~", paste(tornio.preds[i], collapse="+"), sep=""))
	
	# do the before period
	t.tt1 <- vglm(t.tt.formula, tobit(Lower=min.tornio, Upper=max.tornio), data=tornio.1)
	t.tt1.s <- summary(t.tt1)@coef3
	iceTobit.t1[i,] <- c("tornio", "before", rownames(t.tt1.s)[3], t.tt1.s[3,"Estimate"], t.tt1.s[3,"Std. Error"], t.tt1.s[3,"z value"])
	
	# do the after period
	t.tt2 <- vglm(t.tt.formula, tobit(Lower=min.tornio, Upper=max.tornio), data=tornio.2)
	t.tt2.s <- summary(t.tt2)@coef3
	iceTobit.t2[i,] <- c("tornio", "after", rownames(t.tt2.s)[3], t.tt2.s[3,"Estimate"], t.tt2.s[3,"Std. Error"], t.tt2.s[3,"z value"])
	
}
iceTobit.t <- rbind(iceTobit.t1, iceTobit.t2)


iceTobit <- rbind(iceTobit.s, iceTobit.t)
iceTobit[,"estimate"] <- as.numeric(iceTobit[,"estimate"])
iceTobit[,"stdE"] <- as.numeric(iceTobit[,"stdE"])
iceTobit[,"Z"] <- as.numeric(iceTobit[,"Z"])

iceTobit <- iceTobit[order(iceTobit[,"water"], iceTobit[,"variable"]),]

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
legend("topright", legend=c("1803 – 1882", "1921 – 2000"), text.col=c("blue", "red"), bty="n", inset=c(-0.02,-0.07))
arrows(x0=xvals.t, y0=rep(iceTobit.t[,"estimate"],2), x1=xvals.t, y1=c(ity0.t,ity1.t), length=0.05, col=c("blue","red"), angle=90)


# ============================================
# = Write results of tobit analysis to table =
# ============================================
iceTobit[,"estimate"] <- as.numeric(iceTobit[,"estimate"])
iceTobit[,"stdE"] <- as.numeric(iceTobit[,"stdE"])
iceTobit[,"Z"] <- as.numeric(iceTobit[,"Z"])
write.table(iceTobit, file="/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/Results/tobit_coefficients.csv", sep=",", col.names=TRUE, row.names=FALSE)

# ===============================================================
# = Plot time series with breakpoint and residual variance/ R^2 =
# ===============================================================

# ========
# = Suwa: 
# ========
dev.new(width=3.5, height=3.5)
par(mar=c(2.5, 2.5, 0.5, 2.5), mgp=c(1.5, 0.5, 0), tcl=-0.35, ps=9, cex=1)
plot(suwa[bp.opts,"year"], suwa[bp.opts,"doy"], type="l", xlab="Year", ylab="Ice Breakup Day of Year")
par(new=TRUE)
plot(suwa[bp.opts, "year"], resid.vars, type="l", col="blue", xaxt="n", yaxt="n", xlab="", ylab="")
axis(side=4)
mtext(bquote(residual~~variance), side=4, line=1.5)
abline(v=suwa.bp, lwd=2, lty="dashed")


# ==============================================
# = Plot time series and breakpoint likelihood =
# ==============================================
dev.new(width=3.5, height=3.5)
par(mar=c(2.5, 2.5, 0.5, 2.5), mgp=c(1.5, 0.5, 0), tcl=-0.35, ps=9, cex=1)
plot(tornio[bp.opts,"year"], tornio[bp.opts,"doy"], type="l", xlab="Year", ylab="Ice Breakup Day of Year")
par(new=TRUE)
plot(tornio[bp.opts, "year"], r2.torn, type="l", col="blue", xaxt="n", yaxt="n", xlab="", ylab="", lwd=3)
axis(side=4)
mtext(bquote(R^2~~value), side=4, line=1.5)
abline(v=tornio.bp, lwd=2, lty="dashed")
