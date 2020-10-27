#AR(1)
set.seed(456)
ar1.1 <- arima.sim(n=1000, model=list(ar=c(0.9)))
ar1.2 <- arima.sim(n=1000, model=list(ar=c(-0.9)))

op <- par(mfrow=c(2, 1), mar=c(4.1, 4.1, 3.1, 1.1))
acf(ar1.1, lag.max=30, main=expression(AR(1)  ---phi[1]==+0.9))
acf(ar1.2, lag.max=30, main=expression(AR(1)  ---phi[1]==-0.9))
par(op)

#AR(2)
set.seed(456)
(phi.re <- polyroot(c(1, -0.6, -0.3)))
ar2.1 <- arima.sim(n=1000, model=list(ar=c(0.6, 0.3)))
(phi.re <- polyroot(c(1, -0.6, 0.3)))
ar2.2 <- arima.sim(n=1000, model=list(ar=c(0.6, -0.3)))

op <- par(mfrow=c(2, 1), mar=c(4.1, 4.1, 3.1, 1.1))
acf(ar2.1, lag.max=30, main=expression(AR(2)  ---phi[1]==+0.6~~phi[2]==+0.3))
acf(ar2.2, lag.max=30, main=expression(AR(2)  ---phi[1]==+0.6~~phi[2]==-0.3))
par(op)

#PACF
op <- par(mfrow=c(2, 1), mar=c(4.1, 4.1, 3.1, 1.1))
acf(ar1.1, lag.max=30, main=expression(AR(1)  ---phi[1]==+0.9))
pacf(ar1.1, lag.max=30, main=expression(AR(1)  ---phi[1]==+0.9))
par(op)

op <- par(mfrow=c(2, 1), mar=c(4.1, 4.1, 3.1, 1.1))
acf(ar1.2, lag.max=30, main=expression(AR(1)  ---phi[1]==-0.9))
pacf(ar1.2, lag.max=30, main=expression(AR(1)  ---phi[1]==-0.9))
par(op)

op <- par(mfrow=c(2, 1), mar=c(4.1, 4.1, 3.1, 1.1))
acf(ar2.1, lag.max=30, main=expression(AR(2)  ---phi[1]==+0.6~~phi[2]==+0.3))
pacf(ar2.1, lag.max=30, main=expression(AR(2)  ---phi[1]==+0.6~~phi[2]==+0.3))
par(op)

op <- par(mfrow=c(2, 1), mar=c(4.1, 4.1, 3.1, 1.1))
acf(ar2.2, lag.max=30, main=expression(AR(2)  ---phi[1]==+0.6~~phi[2]==-0.3))
pacf(ar2.2, lag.max=30, main=expression(AR(2)  ---phi[1]==+0.6~~phi[2]==-0.3))
par(op)
