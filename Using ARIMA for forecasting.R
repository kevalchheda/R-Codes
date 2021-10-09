#Time series
#install.packages("forecast")
par(ask= TRUE)

#creating a time series object in r
sales <- c(18, 33, 41, 7, 34, 35, 24, 25, 21, 25, 20,
           22, 31, 40, 29, 25, 21, 22, 54, 31, 25, 26, 35)

# ts function is used create time series
tsales <- ts(sales, start = c(2003, 1), frequency = 12)
tsales
plot(tsales)

start(tsales)
end(tsales)
frequency(tsales)

#window is a generic function which extracts the subset of the object x observed between the times start and end.
#If a frequency is specified, the series is then re-sampled at the new frequency.
tsales.subset <- window(tsales, start= c(2003, 5), end= c(2004, 6))
tsales.subset

#simple moving average
library(forecast)
opar<- par(no.readonly = TRUE)
par(mfrow=c(2,2))
ylim <- c(min(Nile), max(Nile))
plot(ma(Nile, 3), main= "Simple moving averages (k=3)", ylim=ylim)
plot(ma(Nile, 7), main= "Simple moving averages (k=7)", ylim=ylim)
plot(ma(Nile, 15), main= "Simple moving averages (k=15)", ylim=ylim)
par(opar)

#Seasonal decomposition using slt()
plot(AirPassengers)

lAirpassengers <- log(AirPassengers)
plot(lAirpassengers, ylab = "log(Airpassengers)")
fit<- stl(lAirpassengers, s.window = "period")
plot(fit)
fit$time.series
exp(fit$time.series)

par(mfrow=c(2,1))
library(forecast)
monthplot(AirPassengers, xlab= "", ylab= "")
seasonplot(AirPassengers, year.labels = "TRUE", main = "")

#exponential smoothing with level, slope and seasonal components
#Holt-Winters is a model of time series behavior. 
#Forecasting always requires a model, and Holt-Winters is a way to model three aspects of the time series: a typical value (average), a slope (trend) over time, and a cyclical repeating pattern (seasonality).
#Seasonality can be confusing.
fit<- HoltWinters(log(AirPassengers))
fit

accuracy(fit)

pred <- forecast(fit, 5)
pred
plot(pred, main = "Forecast for Air Travel",
     ylab = "Log(Airpassengers)", xlab="Time")
pred$mean <- exp(pred$mean)
pred$lower <- exp(pred$lower)
pred$upper <- exp(pred$upper)

p <- cbind(pred$mean, pred$lower, pred$upper)
dimnames(p)[[2]] <- c("mean", "L0 80", "L0 95", "Hi 80", "Hi 95")
p

#Automatic exponential forecasting with ets()
library(forecast)
fit <-ets(JohnsonJohnson)
fit
plot(forecast(fit), main="Johnson and Johnson Forecasts",
     ylab= "Quaterly Earnings(Dollars)", xlab= "Time")

# Transforming the time series and accessing stationarity
library(forecast)
library(tseries)
ployt(Nile)
ndiffs(Nile)
dNile <- diff(Nile)
plot(dNile)
adf.test(dNile)

# Fit the ARIMA model
fit <- arima(Nile, order = c(0, 1, 1))
fit
accuracy(fit)

# Automated ARIMA forecasting
library(forecast)
sunspots
fit <- auto.arima(sunspots)
fit
forecast(fit, 3)
accuracy(fit)
