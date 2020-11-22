#AR(1)
set.seed(456)
ar1.1 <- arima.sim(n=1000, model=list(ar=c(0.9)))
ar1.2 <- arima.sim(n=1000, model=list(ar=c(-0.9)))

op <- par(mfrow=c(2, 1), mar=c(4.1, 4.1, 3.1, 1.1))
acf(ar1.1, lag.max=30, main=expression(AR(1)  ---phi[1]==+0.9))
acf(ar1.2, lag.max=30, main=expression(AR(1)  ---phi[1]==-0.9))
par(op)

#AR(2)
set.seed(123)
(phi.re <- polyroot(c(1, -0.6, -0.3)))
ar2.1 <- arima.sim(n=1000, model=list(ar=c(0.6, 0.3)))
(phi.re <- polyroot(c(1, -0.6, 0.3)))
ar2.2 <- arima.sim(n=1000, model=list(ar=c(0.6, -0.3)))
#ts.plot(ar2.2)

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

#AIC
ar2.ar <- ar.mle(ar2.2)
ar2.ar
ar2.ar$aic
#ar.burg(ar2.2)
#ar.yw(ar2.2)

#MA(2)
set.seed(456)
ma2 <- arima.sim(n=1000, model = list(ma=c(0.6, -0.3)))

op <- par(mfrow=c(2, 1), mar=c(4.1, 4.1, 3.1, 1.1))
acf(ma2, lag.max=30, main=expression(MA(2)  ---phi[1]==+0.6~~phi[2]==-0.3))
pacf(ma2, lag.max=30, main=expression(MA(2)  ---phi[1]==+0.6~~phi[2]==-0.3))
par(op)

#ARIMA
op <- par(mfrow=c(2, 1), mar=c(4.1, 4.1, 3.1, 1.1))
plot(LakeHuron)
plot(diff(LakeHuron))
par(op)

library(forecast)
Huron.0 <- window(LakeHuron, end = 1962)
Huron.arima <- auto.arima(Huron.0, seasonal = F, allowdrift = F)
summary(Huron.arima)
tsdiag(Huron.arima)

Huron.force <- forecast(Huron.arima, 10)
plot(Huron.force)
lines(LakeHuron, lty=2)

#SARIMA 
op <- par(mfrow=c(2, 1), mar=c(4.1, 4.1, 3.1, 1.1))
plot(diff(AirPassengers), main = "Diff")
plot(diff(log(AirPassengers)), main  = "Diff of Log")
par(op)

Air.0 <- window(AirPassengers, end=c(1958, 12))
Air.sarima <- auto.arima(log(Air.0))
summary(Air.sarima)

Air.sarima2 <- Arima(log(Air.0), c(0, 1, 1), seasonal = list(order = c(0,1, 1), period = 12))
summary((Air.sarima2))

tsdiag(Air.sarima2)

Air.fore <- forecast(Air.sarima2, 24)

plot(Air.fore)
lines(log(AirPassengers), lty = 2)

#External Argument
library(mlbench)
data("Ozone")
summary(Ozone)

library(zoo)
start <- "1976-01-01"
x.Date <- as.Date(start) + 0:635
ozone <- na.spline(zoo(Ozone[,4], x.Date))
pressure <- na.spline(zoo(Ozone[, 5], x.Date))
windowSpeed <- na.spline(zoo(Ozone[, 6], x.Date))
humidity <- na.spline(zoo(Ozone[, 7], x.Date))
tempareture <- na.spline(zoo(Ozone[, 8], x.Date))

p <- par(mfrow=c(2, 2), mar=c(4.1, 4.1, 3.1, 1.1))
plot(pressure, ozone, pch=16, col="brown", lwd=2)
plot(windowSpeed, ozone, pch=16, col="darkgreen", lwd=2)
plot(humidity, ozone, pch=16, col="steelblue", lwd=2)
plot(tempareture, ozone, pch=16, col="tomato", lwd=2)
par(op)

X <- data.matrix(cbind(pressure, windowSpeed, windowSpeed^2, humidity, tempareture))
n <- 366
idx <- 1:(n - 31)

ozoneRes.arimax <- auto.arima(ozone[idx], xreg = X[idx,])
ozoneRes.arimax

ts.ts_value <- function(fit) {
  coef <- fit$coef
  se <- sqrt(diag(fit$var.coef))
  t_value <- coef / se
  t_value
}

ts.ts_value(ozoneRes.arimax)

ozoneRes.arimax2 <- auto.arima(ozone[idx], xreg = X[idx, -(2:3)])
ozoneRes.arimax2

tsdiag(ozoneRes.arimax)

ozoneRes.arimax3 <- auto.arima(log(ozone[idx]), xreg = X[idx, -(2:3)])
ozoneRes.arimax3

ts.ts_value(ozoneRes.arimax3)

ozone.fore <- forecast(ozoneRes.arimax3, 31, xreg = X[-idx, -(2:3)])
plot(ozone.fore)
