


library(zoo)
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

specres <- rollSpec(Torn.1803[,2], wsize=w2use, specM=meth2use, nfreq=freq2use)


nused <- ifelse(meth2use=="ar", freq2use, w2use/2)
gYrs <- Torn.1803[(wsize-1):length(Torn.1803[,2]),1]
gFreq <- seq(0, 0.5, length.out=nused)*w2use #seq(1, wsize/2, length.out=nused)


dev.new(height=3.5, width=5)
par(mar=c(3,3,0.5,3), ps=10, family="Times", tcl=-0.35, mgp=c(1.5, 0.35, 0))
image(x=gYrs, y=gFreq, z=specres, col=tim.colors(), xlim=range(gYrs), xlab="Year", ylab="Cycle period (yrs)")
par(new=TRUE)
plot(Torn.1803[(w2use-1):length(Torn.1803[,2]),1:2], type="l", xlim=range(gYrs), xaxs="i", xaxt="n", yaxt="n", xlab="", ylab="")
axis(side=4)
abline(v=1886, lty="dashed", lwd=2)
mtext("Tornio break-up day", side=4, line=1.5)


