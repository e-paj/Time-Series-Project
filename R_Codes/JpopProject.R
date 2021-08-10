Jpop <- read.csv(file="C:/Users/andre/Documents/School/Stat 479/Project/Data/USAJpop.csv")
J_pop <- ts(Jpop,frequency = 12,start=c(2010,1))

library(TSA)
library(forecast)
library(tseries)

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

# see what the plot looks like
plot(J_pop)
# see if the data is stationary or not
adf.test(J_pop) #Since the p-value is less than 0.05, we can reject the null that 
# is it non-stationary
pp.test(J_pop) # since the p-value is less than 0.05, we can reject the null that
# it is non-stationary

# we can see from the plot that there seems to be a decreasing trend in the data
# Hence, de-trend this first using linear regression
Time <- time(J_pop)
score.lin <- lm(J_pop~Time)
plot(J_pop, type='l')
abline(score.lin, col='red')
summary(score.lin)

par(mfrow=c(2,1))
plot(resid(score.lin), type='l', main='Detrended')
plot(diff(J_pop), type='l', main='First Difference')

# to verify that our data has become stationary, take ACF plots
par(mfrow=c(3,1))
acf(J_pop, lag.max=30, main="J_pop")
acf(resid(score.lin), lag.max=30, main='detrended')
acf(diff(J_pop), lag.max=30, main="first difference")

# look at residuals of linear fit
lin.resid.score <- resid(score.lin)
plot(lin.resid.score, type='l')
abline(h=0, col='gray')
acf(lin.resid.score,lag.max=30)
pacf(lin.resid.score,lag.max=length(lin.resid.score))

ar(lin.resid.score,aic=T)

## Try some ARIMA models for the residuals of the first linear model
lin.model1 <- Arima(lin.resid.score,order=c(1,0,0))
lin.model2 <- Arima(lin.resid.score,order=c(0,0,1))
lin.model3 <- Arima(lin.resid.score,order=c(1,0,1))
summary(lin.model1)
summary(lin.model2)
summary(lin.model3)

lin.model4 <- Arima(lin.resid.score,order=c(2,0,0))
lin.model5 <- Arima(lin.resid.score,order=c(0,0,2))
lin.model6 <- Arima(lin.resid.score,order=c(2,0,2))
summary(lin.model4)
summary(lin.model5)
summary(lin.model6)

# since model 1 has the lowest AIC and BIC, we will choose this model
lin.ar1 <- resid(lin.model1)

plot(lin.ar1,type='l')
acf(lin.ar1,lag.max=30); pacf(lin.ar1,lag.max=30)
Box.test(lin.ar1,lag=3,fitdf=2, type='Ljung-Box') # this box test show that 
# my residuals are not independent # either box pierce and ljung box
plotBoxTest(lin.ar1,df=2)

## De-trend using difference
score.diff <- diff(J_pop)
plot(score.diff,type='l')
abline(h=0,col='gray')
acf(score.diff) # sharp drop after 1 lag
pacf(score.diff) # gradual decrease 
Box.test(score.diff,type='Box-Pierce')
Box.test(score.diff,type='Ljung-Box') # this is independent
plotBoxTest(score.diff,df=0)
ar(score.diff,aic=T)

diff.score <- Arima(J_pop,order=c(0,1,0))
diff.score1 <- Arima(J_pop,order=c(1,1,0))
diff.score2 <- Arima(J_pop,order=c(0,1,1))
diff.score3 <- Arima(J_pop,order=c(1,1,1))

summary(diff.score)
summary(diff.score1)
summary(diff.score2)
summary(diff.score3)

res.score <- resid(diff.score3)
par(mfrow=c(2,3))
plot(res.score); abline(h=0,col='gray')
acf(res.score)
pacf(res.score)
Box.test(res.score,lag=4,fitdf=3,type='Box-Pierce')
Box.test(res.score,lag=4,fitdf=3,type='Ljung-Box')
plotBoxTest(res.score,df=3)
par(mfrow=c(2,1))
plot(forecast(diff.score3,h=50))
plot(forecast(diff.score3,h=500))

## Try with seasonal models
sma1 <- Arima(J_pop,order=c(1,1,1),seasonal=list(order=c(1,0,0),period=12))
sma2 <- Arima(J_pop,order=c(1,1,1),seasonal=list(order=c(0,0,1),period=12))
sma3 <- Arima(J_pop,order=c(1,1,1),seasonal=list(order=c(1,0,1),period=12))

summary(sma1)
summary(sma2)
summary(sma3)

sma1.res <- resid(sma1)
par(mfrow=c(2,3))
plot(sma1.res,type='l')
abline(h=0,col='gray')
acf(sma1.res)
pacf(sma1.res)
Box.test(sma1.res,fitdf=5,lag=6,type='Box-Pierce')
Box.test(sma1.res,fitdf=5,lag=6,type="Ljung-Box")
plotBoxTest(sma1.res,df=5)
par(mfrow=c(2,1))
plot(forecast(sma1,h=50))
plot(forecast(sma1,h=500))

##### Auto arima
score <- auto.arima(J_pop)
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
