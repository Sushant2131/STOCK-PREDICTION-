library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)


# get data from the yahoo.com
getSymbols("TCS.NS",from="2017-10-01", to="2022-04-13")
head(TCS.NS)
#View(TCS.NS)
TCS.NS_close_price=TCS.NS[,4]
plot(TCS.NS_close_price)
class(TCS.NS_close_price)

par(mfrow=c(1,1))
acf(TCS.NS_close_price,main="acf for differenced series")
pacf(TCS.NS_close_price,main="pacf for differenced series")

print(adf.test(TCS.NS_close_price))



W=auto.arima(TCS.NS_close_price,seasonal = FALSE)
fitA = auto.arima(TCS.NS_close_price,seasonal = FALSE)
tsdisplay(residuals(fitA),lag.max=40,main='(W) model residual')
auto.arima(TCS.NS_close_price,seasonal = FALSE)   



# Number of period we want to forecast
n <- 100

# Plot the result

fcast1 = forecast(fitA, h=n)
autoplot(fcast1)



#check residual

checkresiduals(fcast1)


#to check accuracy

accuracy(fcast1)