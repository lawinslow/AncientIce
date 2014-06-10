


library(zoo)
library(fields)

rollSpec <- function(x=Torn.1803[,2], wsize=50, specM="ar", nfreq=NULL){
	if(is.null(nfreq)){nfreq=200}
	
	spec2 <- function(x){
		if(specM=="ar"){
			c(spectrum(x, plot=FALSE, na.action=na.omit, method="ar", n.freq=nfreq)$spec)
		}else{
			spectrum(x, plot=FALSE, na.action=na.omit, method="pgram")$spec
		}
	}

	nused <- ifelse(specM=="ar", nfreq, wsize/2)
	log(rollapplyr(data=x, width=wsize, by=1, FUN=spec2))		
}

w2use <- 50
meth2use <- "ar"
freq2use <- 200

# =================
# = Example: Torn =
# =================
specres <- rollSpec(Torn.1803[,2], wsize=w2use, specM=meth2use, nfreq=freq2use)

nused <- ifelse(meth2use=="ar", freq2use, w2use/2)
gYrs <- Torn.1803[(w2use-1):length(Torn.1803[,2]),1]
gFreq <- seq(0, 0.5, length.out=nused)*w2use #seq(1, wsize/2, length.out=nused)

# dev.new(height=3.5, width=5)
png("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/Figures/Torn_roll_spec_50yr.png", res=200, width=5, height=3.5, units="in")
par(mar=c(3,3,0.5,3), ps=10, family="Times", tcl=-0.35, mgp=c(1.5, 0.35, 0))
image(x=gYrs, y=gFreq, z=specres, col=tim.colors(), xlim=range(gYrs), xlab="Year", ylab="Cycle period (yrs)")
par(new=TRUE)
plot(Torn.1803[(w2use-1):length(Torn.1803[,2]),1:2], type="l", xlim=range(gYrs), xaxs="i", xaxt="n", yaxt="n", xlab="", ylab="")
axis(side=4)
abline(v=1886, lty="dashed", lwd=2)
mtext("Tornio break-up day", side=4, line=1.5)
dev.off()

# =======================
# = Example: Suwa Early =
# =======================

w2use <- 30
meth2use <- "ar"
freq2use <- 200

nused <- ifelse(meth2use=="ar", freq2use, w2use/2)
gYrs <- Suwa.Early[(w2use-1):length(Suwa.Early[,3]),1]
gFreq <- seq(0, 0.5, length.out=nused)*w2use #seq(1, wsize/2, length.out=nused)

spec.suwaEarly <- rollSpec(Suwa.Early[,"DOY"], wsize=w2use, specM=meth2use, nfreq=freq2use)


gYrs.suwaEarly <- Suwa.Early[(w2use-1):length(Suwa.Early[,3]),1]

# dev.new(height=3.5, width=5)
png("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/Figures/Suwa_Early_roll_spec_30yr.png", res=200, width=5, height=3.5, units="in")
par(mar=c(3,3,0.5,3), ps=10, family="Times", tcl=-0.35, mgp=c(1.5, 0.35, 0))
image(x=gYrs.suwaEarly, y=gFreq, z=spec.suwaEarly, col=tim.colors(), xlim=range(gYrs.suwaEarly), xlab="Year", ylab="Cycle period (yrs)")
par(new=TRUE)
plot(Suwa.Early[(w2use-1):length(Suwa.Early[,3]),c(1,3)], type="l", xlim=range(gYrs.suwaEarly), xaxs="i", xaxt="n", yaxt="n", xlab="", ylab="")
axis(side=4)
abline(v=1886, lty="dashed", lwd=2)
mtext("Suwa break-up day", side=4, line=1.5)
dev.off()

# =======================
# = Example: Suwa Late =
# =======================

w2use <- 30
meth2use <- "ar"
freq2use <- 200

nused <- ifelse(meth2use=="ar", freq2use, w2use/2)
gYrs <- Suwa.Late[(w2use-1):length(Suwa.Late[,2]),1]
gFreq <- seq(0, 0.5, length.out=nused)*w2use #seq(1, wsize/2, length.out=nused)

spec.suwaLate <- rollSpec(Suwa.Late[,"DOY"], wsize=w2use, specM=meth2use, nfreq=freq2use)


gYrs.suwaLate <- Suwa.Late[(w2use-1):length(Suwa.Late[,2]),1]

# dev.new(height=3.5, width=5)
png("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/Figures/Suwa_Late_roll_spec_30yr.png", res=200, width=5, height=3.5, units="in")
par(mar=c(3,3,0.5,3), ps=10, family="Times", tcl=-0.35, mgp=c(1.5, 0.35, 0))
image(x=gYrs.suwaLate, y=gFreq, z=spec.suwaLate, col=tim.colors(), xlim=range(gYrs.suwaLate), xlab="Year", ylab="Cycle period (yrs)")
par(new=TRUE)
plot(Suwa.Late[(w2use-1):length(Suwa.Late[,2]),c(1,2)], type="l", xlim=range(gYrs.suwaLate), xaxs="i", xaxt="n", yaxt="n", xlab="", ylab="")
axis(side=4)
abline(v=1886, lty="dashed", lwd=2)
mtext("Suwa break-up day", side=4, line=1.5)
dev.new()



