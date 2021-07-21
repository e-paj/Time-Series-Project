pop <- read.csv(file="C:/Users/andre/Documents/School/Stat 479/Project/Data/USAKpop.csv")
poptimeseries <- ts(pop,frequency = 12,start=c(2010,1))
poptimeseries
library(tseries)
library(forecast)
library(TSA)
library(Kendall)

## the first thing to do is plot
plot.ts(poptimeseries) # we can immediately see that there is an increasing trend

## check for stationarity
acf(poptimeseries,lag.max=length(poptimeseries)) # this is not stationary due to trend
# and the later lags exceed the confidence interval
acf(diff(poptimeseries),lag.max=30) #check to see if the first 
# difference is stationary or not
plot(diff(poptimeseries)) #plot of the first difference
# we can also use Ljung-Box and Box Pierce to test this
lag.length = length(poptimeseries)
Box.test(poptimeseries,lag=lag.length)
Box.test(poptimeseries,lag=10,type='Ljung-Box')#have to figure out how much lag we have to 
# use for this
# we can also adf test 
adf.test(poptimeseries) #this is stationary
adf.test(diff(poptimeseries)) # the null hypothesis is rejected

# ARIMA
popts.diff1 <- diff(poptimeseries)
plot.ts(popts.diff1)
acf(popts.diff1,lag.max=36)
acf(popts.diff1,lag.max=36, plot=FALSE)
pacf(popts.diff1,lag.max=36)
pacf(popts.diff1,lag.max=36,plot=FALSE)
pacf(popts.diff1,lag.max=length(popts.diff1))
auto.arima(popts.diff1)
## testing for a trend
MannKendall(poptimeseries) # since the pvalue is very small, we conclude that there is
# a trend in the data
# to visualize the trend, we can plot it
plot(poptimeseries)
lines(lowess(time(poptimeseries),poptimeseries),col='red')
MannKendall(diff(poptimeseries)) # since the pvalue is big, we conclude
# that there is no trend in the data
# we can also perform a seasonality trend test
SeasonalMannKendall(poptimeseries) # looks at monthly trend, yes there is

## ACF and PACF test
acf(poptimeseries)# this would not work as ts is not stationary
pacf(poptimeseries)
acf(diff(poptimeseries),lag.max=length(poptimeseries))# perform this one 
# as it is stationary from performing the acf on the first difference, 
# it can be seen that this could be a MA or AR process
pacf(diff(poptimeseries),lag.max = 12)
# the PACf decreases in a way (can't say geometrically)

# arima
auto.arima(poptimeseries)# this has a drift meaning non zero mean and requires 
# first difference, hence, not stationary and is a MA process not AR
auto.arima(diff(poptimeseries)) # this has lag 3 with non zero mean
ma.md1 <- Arima(diff(poptimeseries),order=c(1,0,3))
summary(ma.md1)
ma.md2 <- Arima(diff(poptimeseries),order=c(3,0,1))
summary(ma.md2) # this has the lowest AIC, though the model
ma.md3 <- Arima(diff(poptimeseries),order=c(0,1,3))
summary(ma.md3) # this has the highest AIC of the 3, but is the model
# lets try the accuracy function
accuracy(ma.md1) < accuracy(ma.md2) # based on RMSE, choose ma.md1
accuracy(ma.md1) < accuracy(ma.md3) # based on RMSE, still ma.md1
accuracy(ma.md2) < accuracy(ma.md3) # based on RMSE, choose ma.md2


# since there is a trend, try to log the ts.
log.pop.ts <-log(poptimeseries)
plot.ts(log.pop.ts) # this did not work properly. There is still an
# increasing trend. 
# smoothing the ts using SMA
library('TTR')
popts.SMA <- SMA(poptimeseries,n=10)
plot.ts(popts.SMA)
# n needs to bigger than 5. Go with 10 or so. No more than 20 though.

# Short term forecasting
# we can forecast data without seasonality using simple exponential smoothing
# use this to a stationary data
stat.popts <- diff(poptimeseries)
popts.forecasts <- HoltWinters(stat.popts,beta=FALSE,gamma=FALSE)
popts.forecasts # alpha is the estimate of the level at the current time point
# since the alpha is very close to 0, this tells us that forecasts is based on
# recent and less recent observations (though more weight is put on recent observations)
popts.forecasts$fitted # where the forecasts are stored
plot(popts.forecasts) # the first difference ts is the black line and the forecasts
# are the red line. The ts of forecasts is much more smoother than the ts of first
# difference
popts.forecasts$SSE # the sum-of-squared errors
# We can make forecasts for further time points by:
library('forecast')
# h is the further time points you want to make a forecasts for
popts.forecasts2 <- forecast:::forecast.HoltWinters(popts.forecasts,h=10) 
popts.forecasts2
plot(popts.forecasts2)
# see if we can improve simple exponential smoothing
acf(popts.forecasts2$residuals,lag.max=132,na.action = na.pass)
# test whether non-zero autocorrelations for lags 1-36 (3 years)
Box.test(popts.forecasts2$residuals,lag=36,type="Ljung-Box") # this fails 
plot.ts(popts.forecasts2$residuals)
# plot a histogram of forecasts errors
plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}
#plotForecastErrors(popts.forecasts2$residuals,na.action=na.pass) # this does not work

# Use Holt's Exponential Smoothing for data without seasonality, but with increasing trend
plot.ts(poptimeseries)
popts.forecasts3 <- HoltWinters(poptimeseries,gamma=FALSE)
popts.forecasts3; popts.forecasts3$SSE
plot(popts.forecasts3)
popts.forecasts4 <- forecast:::forecast.HoltWinters(popts.forecasts3,h=10)
plot(popts.forecasts4)
acf(popts.forecasts4$residuals,lag.max=132,na.action=na.pass)
Box.test(popts.forecasts4$residuals,lag=36,type='Ljung-Box')
plot.ts(popts.forecasts4$residuals)
plotForecastErrors(popts.forecasts4$residuals) # check this!!!

# truncating the spike
plot(poptimeseries[1:120])
popts <- poptimeseries[1:120]
auto.arima(popts)
popts.md1 <- Arima(popts,order=c(0,1,4))
popts.md2 <- Arima(popts,order=c(2,0,2))
accuracy(popts.md1)
accuracy(popts.md2)
plot(forecast(popts,h=20))


