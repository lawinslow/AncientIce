
rl.var.torn <- rollapplyr(Torn.1803[,2], by=1, width=30, FUN=sd, na.rm=TRUE)
rl.var.torn.day <- rollapplyr(Torn.1803[,1], by=1, width=30, FUN=max)

rl.var.suwa.early <- rollapplyr(Suwa.Early[,3], by=1, width=30, FUN=sd, na.rm=TRUE)
rl.var.suwa.early.day <- rollapplyr(Suwa.Early[,1], by=1, width=30, FUN=max)

rl.var.suwa.late <- rollapplyr(Suwa.Late[,3], by=1, width=30, FUN=sd, na.rm=TRUE)
rl.var.suwa.late.day <- rollapplyr(Suwa.Late[,1], by=1, width=30, FUN=max)


dev.new(width=3.5, height=6)
png("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/Figures/rollVariance_30yr_suwa&torn.png", res=200, width=5, height=3.5, units="in")
par(mfrow=c(3,1), mar=c(2,2,0.5, 0.5), ps=10, mgp=c(1, 0.35, 0), tcl=-0.25)

plot(rl.var.torn.day, rl.var.torn, type="l", xlab="Year", ylab="stdev of Torn")

plot(rl.var.suwa.early.day, rl.var.suwa.early, type="l", xlab="Year", ylab="stdev of Early Suwa")

plot(rl.var.suwa.late.day, rl.var.suwa.late, type="l", xlab="Year", ylab="stdev of Late Suwa")
dev.off()
