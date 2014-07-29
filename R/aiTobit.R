


suwa <- read.table("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/Data/suwa.tsv", sep="\t", header=TRUE)
max.suwa <- max(suwa[,"doy"], na.rm=TRUE) + 1L
min.suwa <- min(suwa[,"doy"], na.rm=TRUE)
suwa[suwa[,"no.ice"]==1L & !is.na(suwa[,"no.ice"]) ,"doy"] <- max.suwa
suwa[,"year2"] <- 1:nrow(suwa)



tornio <- read.table("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/Data/tornio.tsv", sep="\t", header=TRUE)
max.tornio <- max(tornio[,"doy"], na.rm=TRUE)
min.tornio <- min(tornio[,"doy"], na.rm=TRUE)
tornio[,"year2"] <- 1:nrow(tornio)





suwa.preds <- c("co2","enso", "aod")
suwa.formula <- as.formula(paste("doy~", paste(suwa.preds, collapse="+"), sep=""))
suwa.tobit.complete <- complete.cases(suwa[,c("doy",suwa.preds)])

# tobit.suwa <- vglm(doy~year2+air.t.as+sunspots+enso+reff, tobit(Lower=min.suwa, Upper=max.suwa), data=suwa)
tobit.suwa <- vglm(suwa.formula, tobit(Lower=min.suwa, Upper=max.suwa), data=suwa)
tobit.coeff.suwa <- coef(summary(tobit.suwa))
p.tobit.suwwa <- 2 * pt(abs(tobit.coeff.suwa[, "z value"]), df.residual(tobit.suwa), lower.tail = FALSE)
cbind(tobit.coeff.suwa, p.tobit.suwwa)


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







