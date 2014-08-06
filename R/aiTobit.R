

library(VGAM)



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
suwa.before.i <- suwa[,"year"] >= 1581 & suwa[,"year"] <= 1681
suwa.after.i <- suwa[,"year"] >= 1897 & suwa[,"year"] <= 1997

suwa.1 <- suwa[suwa.before.i,]
suwa.2 <- suwa[suwa.after.i,]

suwa.preds <- c("co2", "enso", "aod", "air.t.as", "reff")
suwa.pred.combn <- list()
for(i in 1:length(suwa.preds)){
	suwa.pred.combn <- c(suwa.pred.combn, combn(suwa.preds, i, simplify=FALSE))
}

# Suwa Tobit before breakpoint
ts1.minsofar <- 1E12 # starting nll for accumulating minima
ts2.minsofar <- 1E12 # same, but for later half
ts1.nlls <- c()
ts2.nlls <- c()
for(i in 1:length(suwa.pred.combn)){
	
	t.ts1.formula <- as.formula(paste("doy~", paste(suwa.pred.combn[[i]], collapse="+"), sep=""))
	t.ts1 <- vglm(t.ts1.formula, tobit(Lower=min.suwa, Upper=max.suwa), data=suwa.1)
	t.ts1.nll <- t.ts1@criterion$loglikelihood
	ts1.nlls[i] <- t.ts1.nll
	t.ts1.min <- t.ts1.nll
	if(t.ts1.min < ts1.minsofar){
		ts1.minsofar <- t.ts1.min # the nll value of the best model, so far
		ts1.min <- i # the index of the smallest nll for the model, so far
		ts1 <- t.ts1
	}
	
	
	t.ts2.formula <- as.formula(paste("doy~", paste(suwa.pred.combn[[i]], collapse="+"), sep=""))
	t.ts2 <- vglm(t.ts2.formula, tobit(Lower=min.suwa, Upper=max.suwa), data=suwa.2)
	t.ts2.nll <- t.ts2@criterion$loglikelihood
	t.ts2.min <- t.ts2.nll
	ts2.nlls[i] <- t.ts2.nll
	if(t.ts2.min < ts2.minsofar){
		ts2.minsofar <- t.ts2.min # the nll value of the best model, so far
		ts2.min <- i # the index of the smallest nll for the model, so far
		ts2 <- t.ts2
	}
	
}



# ===============================================
# = Tornio: Run Tobit before and after breakpoint =
# ===============================================
# tornio.1 <- tornio[tornio.bp.i,]
# tornio.2 <- tornio[!tornio.bp.i,]
tornio.before.i <- tornio[,"year"] >= 1803 & tornio[,"year"] <= 1882
tornio.after.i <- tornio[,"year"] >= 1921 & tornio[,"year"] <= 2000

tornio.1 <- tornio[tornio.before.i,]
tornio.2 <- tornio[tornio.after.i,]

tornio.preds <- c("air.t.stock", "sunspots", "aod", "nao.djfm", "co2")
tornio.pred.combn <- list()
for(i in 1:length(tornio.preds)){
	tornio.pred.combn <- c(tornio.pred.combn, combn(tornio.preds, i, simplify=FALSE))
}

# Tornio Tobit before breakpoint
tt1.minsofar <- 1E12 # starting nll for accumulating minima
tt2.minsofar <- 1E12 # same, but for later half
tt1.nlls <- c()
tt2.nlls <- c()
for(i in 1:length(tornio.pred.combn)){
	
	t.tt1.formula <- as.formula(paste("doy~", paste(tornio.pred.combn[[i]], collapse="+"), sep=""))
	t.tt1 <- vglm(t.tt1.formula, tobit(Lower=min.tornio, Upper=max.tornio), data=tornio.1)
	t.tt1.nll <- t.tt1@criterion$loglikelihood
	tt1.nlls[i] <- t.tt1.nll
	t.tt1.min <- t.tt1.nll
	if(t.tt1.min < tt1.minsofar){
		tt1.minsofar <- t.tt1.min # the nll value of the best model, so far
		tt1.min <- i # the index of the smallest nll for the model, so far
		tt1 <- t.tt1 # tornio tobit, part 1
	}
	
	
	t.tt2.formula <- as.formula(paste("doy~", paste(tornio.pred.combn[[i]], collapse="+"), sep=""))
	t.tt2 <- vglm(t.tt2.formula, tobit(Lower=min.tornio, Upper=max.tornio), data=tornio.2)
	t.tt2.nll <- t.tt2@criterion$loglikelihood
	t.tt2.min <- t.tt2.nll
	tt2.nlls[i] <- t.tt2.nll
	if(t.tt2.min < tt2.minsofar){
		tt2.minsofar <- t.tt2.min # the nll value of the best model, so far
		tt2.min <- i # the index of the smallest nll for the model, so far
		tt2 <- t.tt2 # tornio tobit, part 2
	}
	
}





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
