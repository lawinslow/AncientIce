library(VGAM)

suwa.preds <- c("year2", "enso", "aod", "sunspots")
tornio.preds <- c("year2", "nao.djfm", "aod", "sunspots") # add air.t.mam

suwa <- read.table("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/Data/suwa.tsv", sep="\t", header=TRUE)
max.suwa <- max(suwa[,"doy"], na.rm=TRUE)
min.suwa <- min(suwa[,"doy"], na.rm=TRUE)
suwa[suwa[,"no.ice"]==1L & !is.na(suwa[,"no.ice"]) ,"doy"] <- max.suwa
suwa[,"year2"] <- 1:nrow(suwa)



tornio <- read.table("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/Data/tornio.tsv", sep="\t", header=TRUE)
max.tornio <- max(tornio[,"doy"], na.rm=TRUE)
min.tornio <- min(tornio[,"doy"], na.rm=TRUE)
tornio[,"year2"] <- 1:nrow(tornio)


suwa.bp <- 1842
tornio.bp <- 1886


# ========================
# = Tobit on entire Suwa =
# ========================
suwa.formula <- as.formula(paste("doy~", paste(suwa.preds, collapse="+"), sep=""))
suwa.tobit.complete <- complete.cases(suwa[,c("doy",suwa.preds)])

tobit.suwa <- vglm(suwa.formula, tobit(Lower=min.suwa, Upper=max.suwa), data=suwa)
tobit.coeff.suwa <- coef(summary(tobit.suwa))
# p.tobit.suwwa <- 2 * pt(abs(tobit.coeff.suwa[, "z value"]), df.residual(tobit.suwa), lower.tail = FALSE)
# cbind(tobit.coeff.suwa, p.tobit.suwwa)

# ======================================
# = Diagnostics for a tobit regression =
# ======================================
pred.tobit.suwa <- fitted(tobit.suwa)[,1]
rr.tobit.suwa <- resid(tobit.suwa, type="response")
rp.tobit.suwa <- resid(tobit.suwa, type="pearson")[,1]

dev.new(width=7, height=5)
par(mfcol=c(2,3), mar=c(2.5, 2.5, 0.5, 0.5), ps=9, mgp=c(1.25, 0.2, 0), tcl=-0.35, cex=1)
plot(rr.tobit.suwa, pred.tobit.suwa, main="Fitted vs. Residuals")
qqnorm(rr.tobit.suwa); qqline(rr.tobit.suwa)
plot(rp.tobit.suwa, pred.tobit.suwa, main="Fitted vs. Pearson Residuals")
qqnorm(rp.tobit.suwa); qqline(rp.tobit.suwa)
plot(rp.tobit.suwa, suwa[suwa.tobit.complete,"doy"], main="Observed vs. Pearson Residuals")
plot(pred.tobit.suwa, suwa[suwa.tobit.complete,"doy"], main="Observed vs. Predicted")

# =========================
# = Breakpoint with Tobit =
# =========================
bp.opts <- seq(1, 561, by=1) #101:500
nlls <- rep(NA, length(bp.opts))
for(i in 1:length(bp.opts)){
	# t.bp <- bp.opts[i]
	t.bp <- bp.opts[i]
	t.suwa <- suwa
	t.suwa[,"bp"] <- (1:nrow(t.suwa))>=t.bp
	# tobit.suwa.year <- vglm(doy~year2*bp, tobit(Lower=min.suwa, Upper=max.suwa), data=t.suwa)
	tobit.suwa.year <- vglm(doy ~ I(year2*bp) + I(year2*!bp), tobit(Lower=min.suwa, Upper=max.suwa), data=t.suwa)
	nlls[i] <- coef(tobit.suwa.year)[2] #tobit.suwa.year@criterion$loglikelihood
	
	min.sofar <- which.min(nlls)
	if(min.sofar!=i){
		min.tobit.suwa.year <- tobit.suwa.year
	}
}
suwa.bp <- suwa[bp.opts[which.min(nlls)],"year"]
suwa.bp

# ===========================
# = Do Breakpoint w/o tobit =
# ===========================
# bp.opts <- seq(100, 500, by=1) #101:500
# nlls <- rep(NA, length(bp.opts))
# for(i in 1:length(bp.opts)){
# 	# t.bp <- bp.opts[i]
# 	t.bp <- bp.opts[i]
# 	t.suwa <- suwa
# 	t.suwa[,"bp"] <- (1:nrow(t.suwa))>=t.bp
# 	tobit.suwa.year <- lm(doy ~ I(year2*bp) + I(year2*!bp), data=t.suwa)
# 	# tobit.suwa.year <- lm(doy ~ year2*bp, data=t.suwa)
# 	nlls[i] <- summary(tobit.suwa.year)$r.squared #coef(tobit.suwa.year)[2] #tobit.suwa.year@criterion$loglikelihood
# 	
# 	min.sofar <- which.max(nlls)
# 	if(min.sofar!=i){
# 		min.tobit.suwa.year <- tobit.suwa.year
# 	}
# }
# suwa.bp <- suwa[bp.opts[which.max(nlls)],"year"]
# suwa.bp


# ==============================================
# = Plot time series and breakpoint likelihood =
# ==============================================
dev.new()
plot(suwa[bp.opts,"year"], suwa[bp.opts,"doy"], type="l")
par(new=TRUE)
plot(suwa[bp.opts, "year"], nlls, type="l", col="lightblue")
abline(v=suwa.bp, lwd=2, lty="dashed")


# ===============================================
# = Run Tobit before and after breakpoint: Suwa =
# ===============================================
suwa.bp.i <- suwa[,"year"] < suwa.bp
suwa.1 <- suwa[suwa.bp.i,]
suwa.2 <- suwa[!suwa.bp.i,]

st1.com <- complete.cases(suwa.1[,c("doy",suwa.preds)])
st2.com <- complete.cases(suwa.2[,c("doy",suwa.preds)])


ts1 <- vglm(suwa.formula, tobit(Lower=min.suwa, Upper=max.suwa), data=suwa.1)
coef(summary(ts1))
ts1.pred <- fitted(ts1)[,1]
ts1.obs <- suwa.1[st1.com,"doy"]
ts1.r2 <- cor(ts1.pred, ts1.obs)^2

ts2 <- vglm(suwa.formula, tobit(Lower=min.suwa, Upper=max.suwa), data=suwa.2)
coef(summary(ts2))
ts2.pred <- fitted(ts2)[,1]
ts2.obs <- suwa.2[st2.com,"doy"]
ts2.r2 <- cor(ts2.pred, ts2.obs)^2


ts1.year <- vglm(doy~year2, tobit(Lower=min.suwa, Upper=max.suwa), data=suwa.1)
ts1y.pred <- fitted(ts1.year)
ts1y.int <- coef(ts1.year)[1]
ts1y.slope <- coef(ts1.year)[3]

ts2.year <- vglm(doy~year2, tobit(Lower=min.suwa, Upper=max.suwa), data=suwa.2)
ts2y.pred <- fitted(ts2.year)
ts2y.int <- coef(ts2.year)[1]
ts2y.slope <- coef(ts2.year)[3]

suwa.y <- rbind(suwa.1, suwa.2)
suwa.y[,"bp.pred"] <- c(predict(ts1.year, newdata=suwa.1)[,1], predict(ts2.year, newdata=suwa.2)[,1])
suwa.y[suwa[,"no.ice"]==1L & !is.na(suwa[,"no.ice"]) ,"doy"] <- NA





# ======================================
# = Diagnostics for a tobit regression =
# ======================================
rr.ts1 <- resid(ts1, type="response")
rp.ts1 <- resid(ts1, type="pearson")[,1]

dev.new(width=7, height=5)
par(mfcol=c(2,3), mar=c(2.5, 2.5, 0.5, 0.5), ps=9, mgp=c(1.25, 0.2, 0), tcl=-0.35, cex=1)
plot(rr.ts1, ts1.pred, main="Fitted vs. Residuals")
qqnorm(rr.ts1); qqline(rr.tobit.suwa)
plot(rp.ts1, ts1.pred, main="Fitted vs. Pearson Residuals")
qqnorm(rp.ts1); qqline(rp.ts1)
plot(rp.ts1, ts1.obs, main="Observed vs. Pearson Residuals")
plot(ts1.pred, ts1.obs, main="Observed vs. Predicted")



rr.ts2 <- resid(ts2, type="response")
rp.ts2 <- resid(ts2, type="pearson")[,1]

dev.new(width=7, height=5)
par(mfcol=c(2,3), mar=c(2.5, 2.5, 0.5, 0.5), ps=9, mgp=c(1.25, 0.2, 0), tcl=-0.35, cex=1)
plot(rr.ts2, ts2.pred, main="Fitted vs. Residuals")
qqnorm(rr.ts2); qqline(rr.tobit.suwa)
plot(rp.ts2, ts2.pred, main="Fitted vs. Pearson Residuals")
qqnorm(rp.ts2); qqline(rp.ts2)
plot(rp.ts2, ts2.obs, main="Observed vs. Pearson Residuals")
plot(ts2.pred, ts2.obs, main="Observed vs. Predicted")






# ==========
# = Tornio =
# ==========

# ===========================
# = Do Breakpoint w/o tobit =
# ===========================
bp.opts <- 1:nrow(tornio)
nlls <- rep(NA, length(bp.opts))
for(i in 1:length(bp.opts)){
	t.bp <- bp.opts[i]
	t.tornio <- tornio
	t.tornio[,"bp"] <- (1:nrow(t.tornio))>=t.bp
	# tobit.tornio.year <- lm(doy ~ I(year2*bp) + I(year2*!bp), data=t.tornio)
	tobit.tornio.year <- lm(doy ~ year2*bp, data=t.tornio)
	nlls[i] <- summary(tobit.tornio.year)$r.squared
	
	min.sofar <- which.max(nlls)
	if(min.sofar!=i){
		min.tobit.tornio.year <- tobit.tornio.year
	}
}
tornio.bp <- tornio[bp.opts[which.max(nlls)],"year"]
tornio.bp

tornio.bp.i <- tornio[,"year"] < tornio.bp
tornio.1 <- tornio[tornio.bp.i,]
tornio.2 <- tornio[!tornio.bp.i,]

tt1.com <- complete.cases(tornio.1[,c("doy",tornio.preds)])
tt2.com <- complete.cases(tornio.2[,c("doy",tornio.preds)])


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


tt1.year <- vglm(doy~year2, tobit(Lower=min.tornio, Upper=max.tornio), data=tornio.1)
tt1y.pred <- fitted(tt1.year)
tt1y.int <- coef(tt1.year)[1]
tt1y.slope <- coef(tt1.year)[3]

# tt2.year <- vglm(doy~year2, tobit(Lower=min.tornio, Upper=max.tornio), data=tornio.2)
# tt2y.pred <- fitted(tt2.year)
# tt2y.int <- coef(tt2.year)[1]
# tt2y.slope <- coef(tt2.year)[3]

t1.final.doy <- tail(tornio.1[,"doy"],1)
delta.t1.t2.doy <-  # the difference between the last doy from torn.1 predictions
tt2.year <- vglm(I(doy-)~I(year2-max(tornio.1[,"year2"])), tobit(Lower=min.tornio, Upper=max.tornio), data=tornio.2)
tt2y.pred <- fitted(tt2.year)
tt2y.int <- coef(tt2.year)[1]
tt2y.slope <- coef(tt2.year)[3]

tornio[,"year3"] <- tornio[,"year"] - tornio.bp
tt.year <- vglm(doy ~ year + pmax(I(year-tornio.bp), 0) , tobit(Lower=min.tornio, Upper=max.tornio), data=tornio)
tty.pred <- fitted(tt.year)
tty.int <- coef(tt.year)[1]
tty.slope <- coef(tt.year)[3]

tornio.y <- rbind(tornio.1, tornio.2)
# tornio.y[,"bp.pred"] <- c(predict(tt1.year, newdata=tornio.1)[,1], predict(tt2.year, newdata=tornio.2)[,1])

tornio.y <- tornio
tornio.y[,"bp.pred"] <- predict(tt.year, newdata=tornio)[,1]

plot(tornio.y[,"year"], tornio.y[,"doy"], type="l")
lines(tornio.y[,"year"], tornio.y[,"bp.pred"], col="red")



# ==============================================
# = Plot time series and breakpoint likelihood =
# ==============================================
dev.new()
plot(tornio[bp.opts,"year"], tornio[bp.opts,"doy"], type="l")
par(new=TRUE)
plot(tornio[bp.opts, "year"], nlls, type="l", col="lightblue")
abline(v=tornio.bp, lwd=2, lty="dashed")



# =================================
# = Breakpoint + Time Series Plot =
# =================================
dev.new(width=3.5, height=5)
par(mfrow=c(2,1), mar=c(2, 2, 0.5, 0.5), mgp=c(1.5, 0.5, 0), tcl=-0.35, ps=9, cex=1)

plot(suwa.y[,"year"], suwa.y[,"doy"], type="l")
lines(suwa.y[suwa.bp.i,"year"], suwa.y[suwa.bp.i,"bp.pred"], lwd=3, col="yellow")
lines(suwa.y[!suwa.bp.i,"year"], suwa.y[!suwa.bp.i,"bp.pred"], lwd=3, col="red")
abline(v=suwa.bp, lty="dashed", lwd=3)

plot(tornio.y[,"year"], tornio.y[,"doy"], type="l")
lines(tornio.y[tornio.bp.i,"year"], tornio.y[tornio.bp.i,"bp.pred"], lwd=3, col="yellow")
lines(tornio.y[!tornio.bp.i,"year"], tornio.y[!tornio.bp.i,"bp.pred"], lwd=3, col="red")
abline(v=tornio.bp, lty="dashed", lwd=3)


