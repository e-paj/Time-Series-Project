library(TSA)
library(tseries)
library(forecast)
### Plot test function
plotBoxTest <- function(dat,df=1){
  par(mfrow=c(2,1))
  pvs = rep(1,130)
  for( i in (df+1):130 )
    pvs[i] = Box.test(dat,lag=i,fitdf=df)$p.value
  plot(pvs,las=1,log="y",main="Box-Pierce");
  abline(h=15^(-(1:15)),col='gray')
  # test if our residuals have autocorrelations in them
  pvs = rep(1,130)
  for( i in (df+1):130 ) 
    pvs[i] = Box.test(dat,lag=i,type="Ljung",fitdf=df)$p.value
  plot(pvs,las=1,log="y",main="Ljung-Box");
  abline(h=15^(-(1:15)),col='gray')
  par(mfrow=c(1,1))
} # we need to tell R how many dfs we used, so we can subtract them 
# before running the Box.test
####

poptimeseries

# test for stationarity
plot(poptimeseries)
adf.test(poptimeseries) # the pvalue is big, hence, we could not reject the null
# that is nonstationarity after we get rid of linear trend
pp.test(poptimeseries) # the pvalue for this one is kinda small,but
# not enough to for us to reject null hypothesis as well.
# this one does the same as adf where get rid of linear model

# Remove linear trend
linearModel <- lm(poptimeseries~time(poptimeseries))
plot(poptimeseries)
abline(linearModel,col='red')
summary(linearModel)
time(poptimeseries) # the units are in years

# now look at residuals to see if there are any patterns
dat.res <- resid(linearModel)
plot(dat.res,type='l') # does not look like white noise
abline(h=0,col='gray')
acf(dat.res,lag.max=60) # there seems to be periodic fluctuations
# but also decaying at the same time
pacf(dat.res,lag.max=60)

# try some ARIMA models for residuals
md1 <- Arima(dat.res,order=c(18,0,0))
md2 <- Arima(dat.res,order=c(0,0,18))
md3 <- Arima(dat.res,order=c(18,0,18))
summary(md1)
summary(md2) # has a better AIC
summary(md3) # has better BIC
# choose the MA as less parameters
ar(dat.res) # go back this later

ar.res <- resid(linearModel)
plot(ar.res,type='l')
acf(ar.res) # there is a spike at lag 20
pacf(ar.res) # spike at lag 18
Box.test(ar.res,lag=20,fitdf=19) # this is a very small pvalue

plotBoxTest(ar.res,df=19)

# Try some seasonal models
mdls1 <- Arima(dat.res,order=c(18,0,0),seasonal=list(order=c(1,0,0),period=6))
mdls2 <- Arima(dat.res,order=c(0,0,18),seasonal=list(order=c(0,0,1),period=6))
mdls3 <- Arima(dat.res,order=c(18,0,18),seasonal=list(order=c(1,0,1),period=6))
summary(mdls1)
summary(mdls2) # this has better AIC & BIC
summary(mdls3)

sarima.res <- resid(mdls2)
plot(sarima.res,type='l')
acf(sarima.res) # great all below white noise line
pacf(sarima.res) # also great

Box.test(sarima.res,lag=21,fitdf=20)
plotBoxTest(sarima.res,df=20)

plot(forecast(mdls2,h=50)) # this is wrong!!
plot(forecast(md2,h=50))

# or try order 12
mdls11 <- Arima(dat.res,order=c(18,0,0),seasonal=list(order=c(1,0,0),period=12))
mdls21 <- Arima(dat.res,order=c(0,0,18),seasonal=list(order=c(0,0,1),period=12))
mdls31 <- Arima(dat.res,order=c(18,0,18),seasonal=list(order=c(1,0,1),period=12))
summary(mdls11)
summary(mdls21)
summary(mdls31)

# Try some ARIMA models for the original data
plot(diff(poptimeseries))
acf(diff(poptimeseries),lag.max=30)
pacf(diff(poptimeseries),lag.max=30)

plotBoxTest(diff(poptimeseries),df=0)

ar(diff(poptimeseries),aic=T) # the biggest order is 1

md11 <- Arima(poptimeseries,order=c(0,1,0))
md21 <- Arima(poptimeseries,order=c(1,1,0))
md31 <- Arima(poptimeseries,order=c(0,1,1))
md41 <- Arima(poptimeseries,order=c(1,1,1)) # the best in terms of AIC & BIC

summary(md11)
summary(md21)
summary(md31)
summary(md41)

arima.res <- resid(md41)
plot(arima.res)
acf(arima.res)
pacf(arima.res)
plotBoxTest(arima.res,df=2)

mdls11 <- Arima(dat.res,order=c(1,1,1),seasonal=list(order=c(1,0,0),period=6))
mdls12 <- Arima(dat.res,order=c(1,1,1),seasonal=list(order=c(0,0,1),period=6))
mdls13 <- Arima(dat.res,order=c(1,1,1),seasonal=list(order=c(1,0,1),period=6))
summary(mdls11) # this is best
summary(mdls12)
summary(mdls13)

plot(forecast(mdls11,h=50))# does not look good....

sarima.res <- resid(mdls11)
plot(sarima.res)
acf(sarima.res)
pacf(sarima.res)
plotBoxTest(sarima.res,df=3)

# Auto Arima
md.auto <- auto.arima(poptimeseries)
summary(md.auto) #non seasonal
plot(forecast(md.auto,h=50))

auto.res <- resid(md.auto)
plot(auto.res)
acf(auto.res)
pacf(auto.res)
plotBoxTest(auto.res,df=1)
