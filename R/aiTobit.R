

library(VGAM)

suwa.preds <- c("co2", "enso", "aod", "sunspots")
suwa.formula <- as.formula(paste("doy~", paste(suwa.preds, collapse="+"), sep=""))

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

suwa.bp <- 1812
tornio.bp <- 1807


# =========================
# = Breakpoint with Tobit =
# =========================
# =========================================
# = Suwa: Calculate Breakpoint with Tobit =
# =========================================
bp.opts <- 10:(nrow(suwa)-10)
resid.vars <- rep(NA, length(bp.opts))
for(i in 1:length(bp.opts)){
	t.bp <- bp.opts[i]
	t.bp.year <- suwa[t.bp,"year"]
	t.suwa <- suwa
	t.suwa[,"bp"] <- (1:nrow(t.suwa))>=t.bp
	
	# tobit.suwa.year <- vglm(doy~year2*bp, tobit(Lower=min.suwa, Upper=max.suwa), data=t.suwa) # 1807
	tobit.suwa.year <- vglm(doy ~ year:bp, tobit(Lower=min.suwa, Upper=max.suwa), data=t.suwa) # 1842
	# tobit.suwa.year <- vglm(doy ~ year + pmax(I(year-t.bp.year), 0), tobit(Lower=min.suwa, Upper=max.suwa), data=t.suwa) # 1807
	
	
	resid.vars[i] <- exp(coef(tobit.suwa.year)[2])
	# nlls[i] <- tobit.suwa.year@criterion$loglikelihood
	
	min.sofar <- which.min(resid.vars)
	if(min.sofar!=i){
		min.tobit.suwa.year <- tobit.suwa.year
	}
	print(round(i/length(bp.opts),2))
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
ts.year <- vglm(doy ~ year + pmax(I(year-suwa.bp), 0) , tobit(Lower=min.suwa, Upper=max.suwa), data=suwa) # use for 1807
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
suwa.before.i <- suwa[,""] # LEFT OFF HERE, APPARENTLY

# st1.com <- complete.cases(suwa.1[,c("doy",suwa.preds)])
# st2.com <- complete.cases(suwa.2[,c("doy",suwa.preds)])

# Suwa Tobit before breakpoint
ts1 <- vglm(suwa.formula, tobit(Lower=min.suwa, Upper=max.suwa), data=suwa.1)
coef(summary(ts1))
ts1.pred <- fitted(ts1)[,1]
ts1.obs <- suwa.1[st1.com,"doy"]
ts1.r2 <- cor(ts1.pred, ts1.obs)^2

# Suwa Tobit after breakpoint
ts2 <- vglm(suwa.formula, tobit(Lower=min.suwa, Upper=max.suwa), data=suwa.2)
coef(summary(ts2))
ts2.pred <- fitted(ts2)[,1]
ts2.obs <- suwa.2[st2.com,"doy"]
ts2.r2 <- cor(ts2.pred, ts2.obs)^2


# =================================================
# = Tornio: Run Tobit before and after breakpoint =
# =================================================
tornio.1 <- tornio[tornio.bp.i,]
tornio.2 <- tornio[!tornio.bp.i,]
# 
# tt1.com <- complete.cases(tornio.1[,c("doy",tornio.preds)])
# tt2.com <- complete.cases(tornio.2[,c("doy",tornio.preds)])

# Do regressions for Tornio before and after the Breakpoint
tt1 <- vglm(tornio.formula, tobit(Lower=min.tornio, Upper=max.tornio), data=tornio.1)
coef(summary(tt1))
tt1.pred <- fitted(tt1)[,1]
tt1.obs <- tornio.1[tt1.com,"doy"]
tt1.r2 <- cor(tt1.pred, tt1.obs)^2

tt2 <- vglm(tornio.formula, tobit(Lower=min.tornio, Upper=max.tornio), data=tornio.2)
coef(summary(tt2))
tt2.pred <- fitted(tt2)[,1]
tt2.obs <- tornio.2[tt2.com,"doy"]
tt2.r2 <- cor(tt2.pred, tt2.obs)^2

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
