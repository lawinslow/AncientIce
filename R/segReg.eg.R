set.seed(2)

slope1 <- -0.02
slope2 <- 0.09
n.sim <- 100
y0 <- 20
noise <- rnorm(n.sim, mean=0, sd=1)
bp.loc <- n.sim/2
year <- 0:(n.sim-1)

bp.i <- year >= bp.loc

y <- y0 + slope1*year + slope2*pmax(year-bp.loc, 0) + noise


# Regression 0
bp0 <- lm(y~year)

# Regression 1
bp1 <- lm(y~bp.i)

# Regression 2
bp2 <- lm(y~bp.i*year)

# Regression 3
bp3 <- lm(y~year+pmax(year-bp.loc, 0))


dev.new()
par(mfrow=c(2,2), mar=c(2,2,0.1,0.1), mgp=c(1,0.25,0), tcl=-0.15, ps=10, cex=1, family="Times")

# Reg 0
plot(y, type="l", ylab="", xlab="")
lines(fitted(bp0), col="blue", lwd=2)

# Reg 1
plot(y, type="l", ylab="", xlab="")
lines(fitted(bp1)[!bp.i], col="blue", lwd=2)
lines(x=year[bp.i], fitted(bp1)[bp.i], col="red", lwd=2)

# Reg 2
plot(y, type="l", ylab="", xlab="")
lines(fitted(bp2)[!bp.i], col="blue", lwd=2)
lines(x=year[bp.i], fitted(bp2)[bp.i], col="red", lwd=2)

# Reg 3
plot(y, type="l", ylab="", xlab="")
lines(fitted(bp3)[!bp.i], col="blue", lwd=2)
lines(x=year[bp.i], fitted(bp3)[bp.i], col="red", lwd=2)

mtext("X", side=1, outer=TRUE, line=-0.85)
mtext("Y", side=2, outer=TRUE, line=-0.85)
