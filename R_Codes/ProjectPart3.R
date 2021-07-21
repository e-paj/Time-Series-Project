library(TSA)
library(forecast)
library(tseries)
poptimeseries
plot(poptimeseries)

### Plot test function
plotBoxTest <- function(dat,df=1){
  par(mfrow=c(2,1))
  pvs = rep(1,16)
  for( i in (df+1):16 )
    pvs[i] = Box.test(dat,lag=i,fitdf=df)$p.value
  plot(pvs,las=1,log="y",main="Box-Pierce");
  abline(h=10^(-(1:10)),col='gray')
  
  pvs = rep(1,16)
  for( i in (df+1):16 ) 
    pvs[i] = Box.test(dat,lag=i,type="Ljung",fitdf=df)$p.value
  plot(pvs,las=1,log="y",main="Ljung-Box");
  abline(h=10^(-(1:10)),col='gray')
  par(mfrow=c(1,1))
} # we need to tell R how many dfs we used, so we can subtract them 
# before running the Box.test
####

# we can see from the plot that there is a big spike
# hence, truncate that spike
newscore <- poptimeseries[1:125]
plot(newscore,type='l')

# see if this new data without the spike is stationary or not
adf.test(newscore) # since the pvalue is big, we could not reject null
pp.test(newscore) # the pvalue is small, hence reject null
kpss.test(newscore,null='Trend') # dont use this; though this is trend stationary

# Detrend by model fitting: linear regression
linearNewScore <- lm(newscore~time(newscore))
plot(newscore,type='l')
abline(linearNewScore,col='red')
summary(linearNewScore)

# Look at the residuals
score.res <- resid(linearNewScore)
plot(score.res,type='l') # does not look like white noise
abline(h=0, col='gray')
acf(score.res,lag.max=60) # seems periodic as it decays
pacf(score.res,lag.max=120) # seems significant when lag 11

# Try some ARIMA models for the residuals
model1 <- Arima(score.res,order=c(6,0,0))
model2 <- Arima(score.res,order=c(0,0,6))
model3 <- Arima(score.res,order=c(6,0,6))
summary(model1)
summary(model2) # best in terms of AIC & BIC
summary(model3)

ar(score.res) # interesting, this requires only 4

ma.res <- resid(model2)
plot(ma.res,type='l') # this looks better
acf(ma.res,lag.max=30) # this has a significant lag at 18
pacf(ma.res) # this has significant lags at 10 and 18
Box.test(ma.res,lag=8, fitdf=7) # this has small pvalue, so reject null
Box.test(ma.res,lag=10,fitdf=7)
Box.test(ma.res,lag=18,fitdf=7)
plotBoxTest(ma.res,df=7) # the pvalues get small as the lag increases

######
# try some seasonal models
mdls1 <- Arima(score.res,order=c(6,0,0),seasonal=list(order=c(1,0,0),period=10))
mdls2 <- Arima(score.res,order=c(0,0,6),seasonal=list(order=c(0,0,1),period=10))
mdls3 <- Arima(score.res,order=c(6,0,6),seasonal=list(order=c(1,0,1),period=10))
summary(mdls1) # best in terms of BIC
summary(mdls2)
summary(mdls3) # best in terms of AIC
# this is just to try, as the data is not seasonal
sarima.res1 <- resid(mdls3)
plot(sarima.res1,type='l')
acf(sarima.res1)
pacf(sarima.res1)
plotBoxTest(sarima.res1,df=14)
plot(forecast(mdls3,h=50))
plot(forecast(model2,h=50)) # something weird about this one!
######

# Try some ARIMA for the original data
plot(diff(poptimeseries))
plot(diff(newscore),type='l')
acf(diff(newscore)) # significant at 3 and 10, go with 3
pacf(diff(newscore)) # significant at 3 and 10, go with 3
plotBoxTest(diff(newscore),df=0)
ar(diff(newscore),aic=T) # also 3 lags

score1 <- Arima(newscore,order=c(0,1,0))
score2 <- Arima(newscore,order=c(3,1,0))
score3 <- Arima(newscore,order=c(0,1,3))
score4 <- Arima(newscore,order=c(3,1,3))

summary(score1)
summary(score2) # best AIC & BIC!!
summary(score3)
summary(score4)

arima.res1 <- resid(score2) 
plot(arima.res1)
acf(arima.res1) # all below noise line
pacf(arima.res1)

plotBoxTest(arima.res1,df=3)

md1s11 = Arima(newscore,order = c(3,1,0), seasonal = list(
  order = c(1,0,0), period = 10))
md1s22 = Arima(newscore,order = c(3,1,0), seasonal = list(
  order = c(0,0,1), period = 10))
md1s33 = Arima(newscore,order = c(3,1,0), seasonal = list(
  order = c(1,0,1), period = 10))
summary(md1s11)
summary(md1s22)
summary(md1s33) # best AIC & BIC

plot(forecast(md1s33,h=50)) # seems weird
sarima.res2 <- resid(md1s33)
plot(sarima.res2)
acf(sarima.res2)
pacf(sarima.res2)

plotBoxTest(sarima.res2,df=5)

# Auto Arima
score.auto <- auto.arima(newscore)
summary(score.auto)
plot(forecast(score.auto,h=50))
res.auto <- resid(score.auto)
plot(res.auto)
acf(res.auto)
pacf(res.auto)
plotBoxTest(res.auto,df=1)


# Now all data, no truncation

# see if stationary or not
adf.test(poptimeseries) # this says that even after taking a line regression
# not stationary
pp.test(poptimeseries) # this says it is stationary

# Remove linear trend
linearScore <- lm(poptimeseries~time(poptimeseries))
summary(linearScore)

res.score <- resid(linearScore)
plot(res.score,type='l')
abline(h=0,col='gray')
acf(res.score,lag.max=60) # significant at lag 6
pacf(res.score,lag.max=60) # significant at lags 11 & 18
# though this seems to be a MA process of order 6

# Try some ARIMA models 
ma.mod1 <- Arima(res.score,order=c(6,0,0))
ma.mod2 <- Arima(res.score,order=c(0,0,6))
ma.mod3 <- Arima(res.score,order=c(6,0,6))

summary(ma.mod1)# this is best in terms of BIC
summary(ma.mod2)
summary(ma.mod3) # this is best in terms of AIC
# since we want less parameters, go with ma.mod1

ar.mod1 <- resid(ma.mod1)
plot(ar.mod1,type='l')
acf(ar.mod1,lag.max=60)
pacf(ar.mod1,lag.max=60) # significant at lag 11, keeps coming up

Box.test(ar.mod1,lag=8,fitdf=7) # fail to reject null that residuals are indep.
Box.test(ar.mod1,lag=11,fitdf=7) # fail to reject as well, smaller p-value though

plotBoxTest(ar.mod1,df=7)

# Try some Seasonal models
md1s10 = Arima(res.score,order = c(6,0,0), seasonal = list(order = c(1,0,0), 
                                                         period = 11))
md1s01 = Arima(res.score,order = c(6,0,0), seasonal = list(order = c(0,0,1), 
                                                         period = 11))
md1s11 = Arima(res.score,order = c(6,0,0), seasonal = list(order = c(1,0,1), 
                                                         period = 11))

summary(md1s10)
summary(md1s01) # this is the best in terms of AIC & BIC
summary(md1s11)

s.mod2 <- resid(md1s01)
plot(s.mod2,type='l')
acf(s.mod2,lag.max=60) # both are below the noise line
pacf(s.mod2,lag.max=60)

plotBoxTest(s.mod2,df=8)

plot(forecast(md1s01,h=50))
plot(forecast(ma.mod1,h=50))

# Try 12 because months...
md1s01.a = Arima(res.score,order = c(6,0,0), seasonal = list(order = c(1,0,0), 
                                                             period = 12))
summary(md1s01.a) # period 11 still has better AIC & BIC

sarima.res <- resid(md1s01.a)
plot(sarima.res,type='l')
acf(sarima.res)
pacf(sarima.res)

plotBoxTest(sarima.res,df=8)

plot(forecast(md1s01.a,h = 50))

# Try some ARIMA models for the original data
plot(diff(poptimeseries))
acf(diff(poptimeseries),lag.max=120) # significant at lag 10
pacf(diff(poptimeseries),lag.max=120) # significant at lag 10
# this could be an AR of order 10

plotBoxTest(diff(poptimeseries,df=0))

ar(diff(poptimeseries),aic=T) # this says 10 orders

md00 = Arima(poptimeseries,order = c(0,1,0)) # best BIC
md10 = Arima(poptimeseries,order = c(10,1,0))
md01 = Arima(poptimeseries,order = c(0,1,10))
md11 = Arima(poptimeseries,order = c(10,1,10)) #best AIC

summary(md00)
summary(md10)
summary(md01)
summary(md11)

arima.res <- resid(md10)
plot(arima.res)
acf(arima.res) # both below noise line
pacf(arima.res)
plotBoxTest(arima.res,df=10)

ar.mod1 = Arima(poptimeseries,order = c(10,1,0), seasonal = list(
  order = c(1,0,0), period = 11))
ar.mod2 = Arima(poptimeseries,order = c(10,1,0), seasonal = list(
  order = c(0,0,1), period = 11))
ar.mod3 = Arima(poptimeseries,order = c(10,1,0), seasonal = list(
  order = c(1,0,1), period = 11))

summary(ar.mod1)
summary(ar.mod2)
summary(ar.mod3)

plot(forecast(ar.mod1,h=50)) # Is this what we want?

sarima.res = resid(ar.mod1)
plot(sarima.res)
acf(sarima.res)
pacf(sarima.res)

plotBoxTest( sarima.res, df=12  )

# Auto Arima
md.auto = auto.arima( poptimeseries )
summary(md.auto)
plot(forecast(md.auto,h=50)) # this looks better

auto.res = resid(md.auto)
plot(auto.res)
acf(auto.res)
pacf(auto.res)

plotBoxTest(auto.res,df=1)
