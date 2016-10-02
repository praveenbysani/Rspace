library(forecast)
#stationary series: mean and variance and covariance doesn't change with time, stays constant
# time series models are applicable for stationary series

#stationary tests: dickey fuller tests

data("AirPassengers")
#exploratory analysis of ts objects
frequency(AirPassengers)
time(AirPassengers)
#prints cycle across time intervals
cycle(AirPassengers)

plot(AirPassengers)
abline(reg=lm(AirPassengers~time(AirPassengers)))

#plots the year on year trend
plot(aggregate(AirPassengers,FUN=mean))
# shows the seasonal effects
boxplot(AirPassengers~cycle(AirPassengers))

#decomposing time-series into seasonal, trend and random componentss

#transform series to stationary
log(AirPassengers) # to remove unequal variance
diff(log(AirPassengers)) # to remove trend in series


#ar - autoregressive models - x(t) depends on alpha*x(t-1) + error(t), shocks have long effects
#ma - moving average models - x(t) = beta*error(t-1)+error(t), shocks in x value diminishes quickly
# auto correlation plots help to determine the order of ar and ma
# correlation in acf will be insignificant after n lags in ma(n)
# correlation in pacf will be insignificant after n lags in ar(n)
acf(log(AirPassengers))
pacf(log(AirPassengers))
# take difference since the acf has no decay
acf(diff(log(AirPassengers)))

#fit the arima model with p,d,q as 0,1,1
tsfit <- arima(log(AirPassengers), c(0,1,1), seasonal = list(order=c(0,1,1), period=12) )
#predict for 10 years
tspred <- predict(tsfit,n.ahead = 12*10)

ts.plot(AirPassengers,2.718^tspred$pred,log="y",lty=c(1,3))

#model the series using exponential smoothing space
ap.ets <- ets(AirPassengers)
ap.forecast <- forecast(ap.ets,h=12)





