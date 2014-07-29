library(VGAM)


suwa <- read.table("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/Data/suwa.tsv", sep="\t", header=TRUE)
max.suwa <- max(suwa[,"doy"], na.rm=TRUE)
min.suwa <- min(suwa[,"doy"], na.rm=TRUE)
suwa[suwa[,"no.ice"]==1L & !is.na(suwa[,"no.ice"]) ,"doy"] <- max.suwa
suwa[,"year2"] <- 1:nrow(suwa)



tornio <- read.table("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/Data/tornio.tsv", sep="\t", header=TRUE)
max.tornio <- max(tornio[,"doy"], na.rm=TRUE)
min.tornio <- min(tornio[,"doy"], na.rm=TRUE)
tornio[,"year2"] <- 1:nrow(tornio)





# ========================
# = Tobit on entire Suwa =
# ========================
suwa.preds <- c("year2", "enso", "aod", "sunspots")
suwa.formula <- as.formula(paste("doy~", paste(suwa.preds, collapse="+"), sep=""))
suwa.tobit.complete <- complete.cases(suwa[,c("doy",suwa.preds)])

# tobit.suwa <- vglm(doy~year2+air.t.as+sunspots+enso+reff, tobit(Lower=min.suwa, Upper=max.suwa), data=suwa)
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

# ========================
# = Test Fake Breakpoint =
# ========================
# fake.means <- c(rep(-200, 50), rep(200, 50))
# fake.slopes <- cumsum(c(rep(0.5, 50), rep(1, 50)))
# fake.doy <- rnorm(100, sd=1)+fake.means+fake.slopes
# Fake <- data.frame("year"=1:100, "doy"=fake.doy, "bp"=(1:100)>=51)
# f.bp <- (1:100)>=51
# 
# summary(vglm(doy~year, tobit(Lower=min(fake.doy), Upper=max(fake.doy)), data=Fake))
# summary(vglm(doy~year*bp, tobit(Lower=min(fake.doy), Upper=max(fake.doy)), data=Fake))
# 
# summary(lm(doy~year*bp, data=Fake))
# summary(lm(doy~year, data=Fake))

# =========================
# = Breakpoint with Tobit =
# =========================
bp.opts <- seq(1, 562, by=1) #101:500
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

# ==============================================
# = Plot time series and breakpoint likelihood =
# ==============================================
dev.new()
plot(tornio[bp.opts,"year"], tornio[bp.opts,"doy"], type="l")
par(new=TRUE)
plot(tornio[bp.opts, "year"], nlls, type="l", col="lightblue")
abline(v=tornio.bp, lwd=2, lty="dashed")







