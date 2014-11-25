
# ================
# = Load Results =
# ================
load("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/Results/tornioBP.RData")
load("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/Results/suwaBP.RData")


# ===============================================================
# = Plot time series with breakpoint and residual variance/ R^2 =
# ===============================================================
# ========
# = Suwa: 
# ========
# dev.new(width=3.5, height=3.5)
png("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/Figures/suwa.bp.residVar.png", width=3.5, height=3.5, res=150, units="in")
par(mar=c(2.5, 2.5, 0.5, 2.5), mgp=c(1.5, 0.5, 0), tcl=-0.35, ps=9, cex=1)
plot(suwa[bp.opts.s,"year"], suwa[bp.opts.s,"doy"], type="l", xlab="Year", ylab="Ice Breakup Day of Year")
par(new=TRUE)
plot(suwa[bp.opts.s, "year"], resid.vars, type="l", col="blue", xaxt="n", yaxt="n", xlab="", ylab="", lwd=2.5)
lines(suwa[bp.opts.s, "year"], resid.vars, col="white", lwd=1)
axis(side=4)
mtext(bquote(residual~~variance), side=4, line=1.5)
abline(v=suwa.bp, lwd=2, lty="dashed")
dev.off()

# ==============================================
# = Plot time series and breakpoint likelihood =
# ==============================================
# dev.new(width=3.5, height=3.5)
png("/Users/Battrd/Documents/School&Work/WiscResearch/AncientIce/Figures/tornio.bp.R2.png", width=3.5, height=3.5, res=150, units="in")
par(mar=c(2.5, 2.5, 0.5, 2.5), mgp=c(1.5, 0.5, 0), tcl=-0.35, ps=9, cex=1)
plot(tornio[bp.opts.t,"year"], tornio[bp.opts.t,"doy"], type="l", xlab="Year", ylab="Ice Breakup Day of Year")
par(new=TRUE)
plot(tornio[bp.opts.t, "year"], r2.tornio, type="l", col="blue", xaxt="n", yaxt="n", xlab="", ylab="", lwd=2.5)
lines(tornio[bp.opts.t, "year"], r2.tornio, col="white", lwd=1)
axis(side=4)
mtext(bquote(R^2~~value), side=4, line=1.5)
abline(v=tornio.bp, lwd=2, lty="dashed")
dev.off()

# tornio[bp.opts.t, "year"][order(r2.tornio, decreasing=TRUE)]