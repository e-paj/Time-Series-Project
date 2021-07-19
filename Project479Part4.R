pop <- read.csv(file="C:/Users/andre/Documents/School/Stat 479/Project/Data/USAKpop.csv")
poptimeseries <- ts(pop,frequency = 12,start=c(2010,1))
library(TSA)
library(forecast)
library(tseries)
poptimeseries
plot(poptimeseries)
adf.test(poptimeseries)
pp.test(poptimeseries)
ar(poptimeseries)

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

##### De-trend using linear regression
Time <- time(poptimeseries)
score.lin <- lm(poptimeseries~Time)
plot(poptimeseries,type='l')
abline(score.lin,col='red')
summary(score.lin)

score.lin2 <- lm(poptimeseries~Time+Time^2)
score.res2 <- resid(score.lin2)
plot(score.res2,type='l')
abline(h=0,col='gray')
acf(score.res2,lag.max=30)

## look at the residuals of linear fit
lin.resid.score <- resid(score.lin)
plot(lin.resid.score,type='l')
abline(h=0,col='gray')
acf(lin.resid.score,lag.max=30)
pacf(lin.resid.score,lag.max=length(lin.resid.score))

score.lin1 <- lm(poptimeseries~time(poptimeseries)+lag(poptimeseries,1))
score.res <- resid(score.lin1)
plot(score.res,type='l')
abline(h=0,col='gray')
acf(score.res,lag.max=30)
pacf(score.res,lag.max=30)

ar(lin.resid.score,aic=T)

## Try some ARIMA models for the residuals of the first linear model
lin.model1 <- Arima(lin.resid.score,order=c(6,0,0))
lin.model2 <- Arima(lin.resid.score,order=c(0,0,6))
lin.model3 <- Arima(lin.resid.score,order=c(6,0,6))

summary(lin.model1)
summary(lin.model2)
summary(lin.model3)

lin.model4 <- Arima(lin.resid.score,order=c(2,0,0))
lin.model5 <- Arima(lin.resid.score,order=c(0,0,2))
lin.model6 <- Arima(lin.resid.score,order=c(2,0,2))

summary(lin.model4)
summary(lin.model5)
summary(lin.model6)

lin.ar1 <- resid(lin.model4)
lin.ma <- resid(lin.model5)
lin.ar <- resid(lin.model1)

plot(lin.ar,type='l')
acf(lin.ar,lag.max=30); pacf(lin.ar,lag.max=30)
Box.test(lin.ar,lag=8,fitdf=7)
plotBoxTest(lin.ar,df=7)

plot(lin.ar1,type='l')
acf(lin.ar1,lag.max=30); pacf(lin.ar1,lag.max=30)
Box.test(lin.ar1,lag=3,fitdf=2)

## ARIMA models for the second linear model
lin.model7 <- Arima(score.res,order=c(1,0,0))
lin.model8 <- Arima(score.res,order=c(0,0,1))
lin.model9 <- Arima(score.res,order=c(1,0,1))

summary(lin.model7)
summary(lin.model8)
summary(lin.model9)

## ARIMA models for the thrid linear model
lin.model10 <- Arima(score.res2,order=c(1,0,0))
summary(lin.model10)
lin.model11 <- Arima(score.res2,order=c(0,0,6))
summary(lin.model11)

##### De-trend using difference
score.diff <- diff(poptimeseries)
plot(score.diff,type='l')
abline(h=0,col='gray')
acf(score.diff) # lags significant at 1 and 3
pacf(score.diff)
Box.test(score.diff,type='Box-Pierce')
Box.test(score.diff,type='Ljung-Box')
plotBoxTest(score.diff,df=0)
ar(score.diff,aic=T) # from this the most significant lag is 1.

diff.score <- Arima(poptimeseries,order=c(0,1,0))
diff.score1 <- Arima(poptimeseries,order=c(1,1,0))
diff.score2 <- Arima(poptimeseries,order=c(0,1,1))
diff.score3 <- Arima(poptimeseries,order=c(1,1,1))

summary(diff.score)
summary(diff.score1)
summary(diff.score2)
summary(diff.score3)

diff.score4 <- Arima(poptimeseries,order=c(3,1,0))
diff.score5 <- Arima(poptimeseries,order=c(0,1,3))
diff.score6 <- Arima(poptimeseries,order=c(3,1,3),method='ML')

summary(diff.score4)
summary(diff.score5)
summary(diff.score6)

res.score3 <- resid(diff.score5)
par(mfrow=c(2,3))
plot(res.score3); abline(h=0,col='gray')
acf(res.score3)
pacf(res.score3)
Box.test(res.score3,lag=4,fitdf=3,type='Box-Pierce')
Box.test(res.score3,lag=4,fitdf=3,type='Ljung-Box')
plotBoxTest(res.score3,df=3)
par(mfrow=c(2,1))
plot(forecast(diff.score5,h=50))
plot(forecast(diff.score5,h=500))

res.score4 <- resid(diff.score6)
par(mfrow=c(2,3))
plot(res.score4); abline(h=0,col='gray')
acf(res.score4)
pacf(res.score4)
Box.test(res.score4,lag=24,fitdf=6,type='Box-Pierce')
Box.test(res.score3,lag=24,fitdf=6,type='Ljung-Box')
par(mfrow=c(2,1))
plot(forecast(diff.score6,h=50))
plot(forecast(diff.score6,h=500))

# Seasonal models
sma1 <- Arima(poptimeseries,order=c(0,1,3),seasonal=list(order=c(1,0,0),period=12))
sma2 <- Arima(poptimeseries,order=c(0,1,3),seasonal=list(order=c(0,0,1),period=12))
sma3 <- Arima(poptimeseries,order=c(0,1,3),seasonal=list(order=c(1,0,1),
                                                         period=12),method='ML')

summary(sma1)
summary(sma2)
summary(sma3)

sma1.res <- resid(sma1)
par(mfrow=c(2,3))
plot(sma1.res,type='l')
abline(h=0,col='gray')
acf(sma1.res)
pacf(sma1.res)
Box.test(sma1.res,fitdf=4,lag=22,type='Box-Pierce')
Box.test(sma1.res,fitdf=4,lag=22,type="Ljung-Box")
plotBoxTest(sma1.res,df=4)
par(mfrow=c(2,1))
plot(forecast(sma1,h=50))
plot(forecast(sma1,h=500))

sma3.res <- resid(sma3)
par(mfrow=c(2,2))
plot(sma3.res,type='l')
abline(h=0,col='gray')
acf(sma3.res)
pacf(sma3.res)
Box.test(sma3.res,fitdf=5,lag=23,type='Box-Pierce')
Box.test(sma3.res,fitdf=5,lag=23,type='Ljung-Box')
par(mfrow=c(2,2))
plot(forecast(sma3,h=50))
plot(forecast(sma3,h=500))

sma4 <- Arima(poptimeseries,order=c(0,1,3),seasonal=list(order=c(1,0,0),period=10))
sma5 <- Arima(poptimeseries,order=c(0,1,3),seasonal=list(order=c(0,0,1),period=10))
sma6 <- Arima(poptimeseries,order=c(0,1,3),seasonal=list(order=c(1,0,1),
                                                         period=10))
summary(sma4)
summary(sma5)
summary(sma6)

sma5.res <- resid(sma5)
plot(sma5.res)
acf(sma5.res)
pacf(sma5.res)

par(mfrow=c(2,1))
plot(forecast(sma5,h=50))
plot(forecast(sma5,h=500))

sma7 <- Arima(poptimeseries,order=c(0,1,3),seasonal=list(order=c(1,0,0),period=18))
sma8 <- Arima(poptimeseries,order=c(0,1,3),seasonal=list(order=c(0,0,1),period=18))
sma9 <- Arima(poptimeseries,order=c(0,1,3),seasonal=list(order=c(1,0,1),period=18))

summary(sma7)
summary(sma8)
summary(sma9)

sma7.res <- resid(sma7)
plot(sma7.res)
acf(sma7.res)
pacf(sma7.res)
Box.test(sma7.res,lag=18,fitdf=4)
Box.test(sma7.res,lag=16,fitdf=4,type="Ljung-Box")

sma10 <- Arima(poptimeseries,order=c(0,1,3),seasonal=list(order=c(3,0,0),period=18))
sma11 <- Arima(poptimeseries,order=c(0,1,3),seasonal=list(order=c(0,0,3),period=18))
sma12 <- Arima(poptimeseries,order=c(0,1,3),seasonal=list(order=c(3,0,3),period=18))

#### Arima with drift
md.wd <- Arima(poptimeseries,include.drift = T, order=c(0,1,3))
summary(md.wd)

par(mfrow=c(2,1))
plot(forecast(md.wd,h=50))
plot(forecast(md.wd,h=500))

md.wd.res <- resid(md.wd)
par(mfrow=c(2,3))
plot(md.wd.res)
abline(h=0,col='gray')
acf(md.wd.res)
pacf(md.wd.res)
Box.test(md.wd.res,lag=8,fitdf=4,type="Box-Pierce")
Box.test(md.wd.res,lag=8,fitdf=4,type='Ljung-Box')
plotBoxTest(md.wd.res,df=4)

md.wd1 <- Arima(poptimeseries,include.drift = T, order=c(0,1,3),
                seasonal = list(order=c(1,0,0),period=12) )
summary(md.wd1)
md.wd1.res <- resid(md.wd1)
par(mfrow=c(2,2))
plot(md.wd1.res)
abline(h=0,col='gray')
acf(md.wd1.res)
pacf(md.wd1.res)
Box.test(md.wd1.res,lag=18,fitdf=5,type='Box-Pierce')
Box.test(md.wd1.res,lag=18,fitdf=5,type='Ljung-Box')

par(mfrow=c(2,2))
plot(forecast(md.wd1,h=50))
plot(forecast(md.wd1,h=500))

md.wd2 <- Arima(poptimeseries,include.drift = T, order=c(0,0,3))
summary(md.wd2)
md.wd2.res <- resid(md.wd2)
par(mfrow=c(2,2))
plot(md.wd2.res)
abline(h=0,col='gray')
acf(md.wd2.res)
pacf(md.wd2.res)
Box.test(md.wd2.res,lag=6,fitdf=5)
Box.test(md.wd2.res,lag=6,fitdf=5,type='Ljung-Box')

par(mfrow=c(2,1))
plot(forecast(md.wd2,h=50))
plot(forecast(md.wd2,h=500))



##### Auto arima
score <- auto.arima(poptimeseries)
summary(score)

par(mfrow=c(1,1))
plot(forecast(score,h=50))
plot(forecast(score,h=500))

res.auto.score <- resid(score)
par(mfrow=c(2,2))
plot(res.auto.score)
abline(h=0,col='gray')
acf(res.auto.score)
pacf(res.auto.score)
Box.test(res.auto.score,lag=5,fitdf=4)
Box.test(res.auto.score,lag=5,fitdf=4,type='Ljung-Box')
