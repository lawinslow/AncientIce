

# ==========
# = Set WD =
# ==========
if(Sys.info()[["user"]]%in%c("ryanb","Battrd")){
	setwd("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce") # for ryan
}

# ==================
# = Load Libraries =
# ==================
library(fields)
library(reshape2)


# ================
# = Load Results =
# ================
load("./Results/tornioBP.RData")
load("./Results/suwaBP.RData")



# ============================================
# = Plot time series with breakpoint and AIC =
# ============================================
# ========
# = Suwa: 
# ========
# dev.new(width=3.5, height=3.5)
png("./Figures/suwa.bp.residVar.png", width=3.5, height=3.5, res=150, units="in")
par(mar=c(2.5, 2.5, 0.5, 2.5), mgp=c(1.5, 0.5, 0), tcl=-0.35, ps=9, cex=1)
plot(suwa[bp.opts.s,"year"], suwa[bp.opts.s,"doy"], type="l", xlab="Year", ylab="Ice Breakup Day of Year")
par(new=TRUE)
plot(suwa[bp.opts.s, "year"], aic.suwa, type="l", col="blue", xaxt="n", yaxt="n", xlab="", ylab="", lwd=2.5)
lines(suwa[bp.opts.s, "year"], aic.suwa, col="white", lwd=1)
axis(side=4)
mtext(bquote(AIC), side=4, line=1.5)
abline(v=suwa.bp, lwd=2, lty="dashed")
dev.off()


# ==============================================
# = Plot time series and breakpoint likelihood =
# ==============================================
# dev.new(width=3.5, height=3.5)
png("./Figures/tornio.bp.R2.png", width=3.5, height=3.5, res=150, units="in")
par(mar=c(2.5, 2.5, 0.5, 2.5), mgp=c(1.5, 0.5, 0), tcl=-0.35, ps=9, cex=1)
plot(tornio[bp.opts.t,"year"], tornio[bp.opts.t,"doy"], type="l", xlab="Year", ylab="Ice Breakup Day of Year")
par(new=TRUE)
plot(tornio[bp.opts.t, "year"], aic.tornio, type="l", col="blue", xaxt="n", yaxt="n", xlab="", ylab="", lwd=2.5)
lines(tornio[bp.opts.t, "year"], aic.tornio, col="white", lwd=1)
axis(side=4)
mtext(bquote(AIC), side=4, line=1.5)
abline(v=tornio.bp, lwd=2, lty="dashed")
dev.off()


# ============================
# = Plot Tornio 2 BP Surface =
# ============================
myCol <- function(n){
	colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(n)
}
# bp2.mat <- matrix(NA, nrow=nrow(tornio), ncol=nrow(tornio), dimnames=list(unique(tornio[,"year"]), unique(tornio[,"year"])))
# bp2.mat[t(upper.tri(bp2.mat))] <- aic.tornio2
# bp2.mat <- t(bp2.mat)

df <- as.data.frame(cbind(bp.opts.t2, aic.tornio2))
names(df) <- c("year1","year2","aic")

df0 <- expand.grid(1:nrow(tornio), 1:nrow(tornio))
names(df0) <- c("year1", "year2")

df.fill <- merge(df0, df, all=TRUE)
names(df.fill) <- c("year1","year2","value")
bp2.mat <- as.matrix(dcast(df.fill, year1~year2)[,-1])

smplt <- c(0.8,0.85, 0.15,0.75)
bgplt <- c(0.1,0.95,0.1,0.95)
axargs <- list(mgp=c(1.1,0.3,0))
# dev.new(width=5, height=5)
png("./Figures/tornio.2BP.surface.png", width=5, height=5, res=150, units="in")
par(mar=c(2,2,0.1,2), ps=10, cex=1, family="Times", mgp=c(1.25,0.25,0), tcl=-0.15)
image.plot(
	x=as.integer(unique(tornio[,"year"])), #as.integer(rownames(bp2.mat)), 
	y=as.integer(unique(tornio[,"year"])), #as.integer(colnames(bp2.mat)), 
	z=bp2.mat-min(aic.tornio), 
	xlab=bquote(1^st~~Breakpoint), 
	ylab=bquote(2^nd~~Breakpoint), 
	# legend.lab=bquote(Delta~AIC~~(Relative~~to~~Best~~1~~BP~~Model)),
	smallplot=smplt, 
	bigplot=bgplt,
	legend.args=list(text=bquote(Delta~AIC~~(Relative~~to~~Best~~1~~BP~~Model)), line=1, side=4),
	# legend.line=1,
	axis.args=axargs
)
abline(a=0, b=1)
abline(a=50, b=1)
abline(a=100, b=1)
text(1850, 1850, bquote(phantom()%up%phantom()~~BP>=1~~yr~~apart~~bold(phantom()%up%phantom())), srt=45, pos=3, offset=0)
text(1850, 1900, bquote(phantom()%up%phantom()~~BP>=50~~yrs~~apart~~bold(phantom()%up%phantom())), srt=45, pos=3, offset=0)
text(1850, 1950, bquote(phantom()%up%phantom()~~BP>=100~~yrs~~apart~~bold(phantom()%up%phantom())), srt=45, pos=3, offset=0)

dev.off() 
