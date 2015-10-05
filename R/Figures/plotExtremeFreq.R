
# ==========
# = Set WD =
# ==========
if(Sys.info()[["user"]]%in%c("ryanb","Battrd")){
	setwd("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce") # for ryan
}


# ================
# = Load Results =
# ================
load("./Results/tornioBP.RData")
load("./Results/suwaBP.RData")


# ======================================
# = Function for Frequency of Extremes =
# ======================================
extFreq <- function(x, y){
	stopifnot(all(diff(x)==1))
	groups <- cut(x, 6)
	outFreq <- aggregate(y, by=list(groups), FUN=function(x)mean(x, na.rm=TRUE)*10)[,2]
	outRange <- aggregate(x, by=list(groups), FUN=function(x)paste0(range(x), collapse="-"))[,2]
	data.frame(period=outRange, freq=outFreq)
}


ef.dat0 <- merge(suwa[,c("year", "no.ice")], tornio[,c("year", "doy")], all=TRUE)
ef.dat <- ef.dat0
ef.dat[,"doy"] <- as.integer(ef.dat0[,"doy"]<124)


ef.suwa <- data.frame(system="suwa", extFreq(ef.dat[,"year"], ef.dat[,"no.ice"]))
ef.tornio <- data.frame(system="tornio", extFreq(ef.dat[,"year"], ef.dat[,"doy"]))

# ef <- rbind(ef.suwa, ef.tornio)

ef <- as.matrix(cbind("Suwa"=ef.suwa[,"freq"], "Tornio"=ef.tornio[,"freq"]))
rownames(ef) <- ef.suwa[,"period"]

dcast(ef, freq~period+system)


# dev.new(width=4.5, height=3.5)
png("./Figures/extremeFrequency.png", width=4.5, height=3.5, res=150, units="in")
par(mar=c(2.15, 2, 0.1, 0.1), mgp=c(1.25, 0.15, 0), tcl=-0.15, cex=1, ps=8)
barFig <- barplot(
	t(ef), 
	beside=TRUE, 
	# names.arg=regKey2,
	col=c("black", "darkgray"),
	border=c("black","darkgray"),
	space=c(0.15, 1),
	ylab="Extremes per decade",
	xlab="Time period",
	legend.text=c("Suwa", "Torne"), 
	args.legend=list(x="topleft", bty="n", inset=c(0.075,0.075), ncol=1, col=c("black","darkgray"))	
)
axis(side=1, at=colMeans(barFig), labels=FALSE)
box(bty="l")
dev.off()