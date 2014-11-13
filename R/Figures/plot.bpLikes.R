
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
dev.new(width=3.5, height=3.5)
par(mar=c(2.5, 2.5, 0.5, 2.5), mgp=c(1.5, 0.5, 0), tcl=-0.35, ps=9, cex=1)
plot(suwa[bp.opts.s,"year"], suwa[bp.opts.s,"doy"], type="l", xlab="Year", ylab="Ice Breakup Day of Year")
par(new=TRUE)
plot(suwa[bp.opts.s, "year"], resid.vars, type="l", col="blue", xaxt="n", yaxt="n", xlab="", ylab="")
axis(side=4)
mtext(bquote(residual~~variance), side=4, line=1.5)
abline(v=suwa.bp, lwd=2, lty="dashed")


# ==============================================
# = Plot time series and breakpoint likelihood =
# ==============================================
dev.new(width=3.5, height=3.5)
par(mar=c(2.5, 2.5, 0.5, 2.5), mgp=c(1.5, 0.5, 0), tcl=-0.35, ps=9, cex=1)
plot(tornio[bp.opts.t,"year"], tornio[bp.opts.t,"doy"], type="l", xlab="Year", ylab="Ice Breakup Day of Year")
par(new=TRUE)
plot(tornio[bp.opts.t, "year"], r2.tornio, type="l", col="blue", xaxt="n", yaxt="n", xlab="", ylab="", lwd=3)
axis(side=4)
mtext(bquote(R^2~~value), side=4, line=1.5)
abline(v=tornio.bp, lwd=2, lty="dashed")
