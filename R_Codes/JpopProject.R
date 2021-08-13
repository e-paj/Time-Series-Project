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
acf(J_pop)
pacf(J_pop)

# we can see from the plot that there seems to be a decreasing trend in the data
# Hence, de-trend this first using linear regression
Time <- time(J_pop)
score.lin <- lm(J_pop~Time)
plot(J_pop, type='l')
abline(score.lin, col='red')
summary(score.lin)

lin.resid.score <- resid(score.lin)
plot(lin.resid.score, type='l')
abline(h=0, col='gray')
ar(lin.resid.score,aic=T)

plot(lin.resid.score, type='l', main='Detrended')
abline(h=0, col='gray')

adf.test(resid(score.lin))

### Take ACF plots
acf(lin.resid.score, lag.max=30, main='detrended')
pacf(lin.resid.score, lag.max=30, main='detrended')


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

plot(lin.ar1,type='l',main='Model 1'); abline(h=0,col='gray')
acf(lin.ar1,lag.max=30, main='Model 1'); pacf(lin.ar1,lag.max=30,main='Model 1')
Box.test(lin.ar1,lag=3,fitdf=2) # this box test show that 
# my residuals are not independent # either box pierce and ljung box
plotBoxTest(lin.ar1,df=2)

plot(forecast(lin.model1,h=50))
plot(forecast(lin.model1,h=500))

## De-trend using difference
score.diff <- diff(J_pop)
plot(score.diff,type='l',main='First Difference')
abline(h=0,col='gray')
acf(score.diff,main="First Difference") # sharp drop after 1 lag
pacf(score.diff,main='First Difference') # gradual decrease 
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
plot(res.score,main='Model 2'); abline(h=0,col='gray')
acf(res.score,main='Model 2')
pacf(res.score,main='Model 2')
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

##### Drift
model3 <- Arima(J_pop, include.drift = T, order=c(1,0,0))
summary(model3)

model4 <- Arima(J_pop, include.drift = T, order=c(1,1,1))
summary(model4)

model4.resid <- resid(model4)
plot(model4.resid, main='Model 3'); abline(h=0, col='gray')
acf(model4.resid,main='Model 3')
pacf(model4.resid, main='Model 3')
Box.test(model4.resid, lag=5, fitdf=4)
Box.test(model4.resid, lag=5, fitdf=4, type='Ljung-Box')

plot(forecast(model4,h=50))

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

### How well my models performed!!
## To do this let's compare actual values to the predicted values
# All Models
actual.val1 <- forecast(lin.model1, h=7)
actual.val2 <- forecast(diff.score3, h=7)
actual.val3 <- forecast(model4, h=7)

J2021 <- read.csv(file='C:/Users/andre/Documents/School/Stat 479/Project/Data/Jpop2021.csv')
J2021 <- ts(J2021,frequency = 12,start=c(2021,1))
# we have to normalize the data
summary(J2021)
log_scale1 <- log(as.data.frame(J2021))

K2021 <- read.csv(file='C:/Users/andre/Documents/School/Stat 479/Project/Data/Kpop2021.csv')
K2021 <- ts(K2021,frequency = 12,start=c(2021,1))

# Now make a frame consisting of actual and predicted values
# First Model
J.com <- data.frame(actual.val1$mean, log_scale1)
colnames(J.com) <- c('Forecast','Actual')
ts.plot(J.com, gpars=list(xlab='Jan-Jul 2021',ylab='Value',lty=c(2:4),
                          main='Jpop Model Comparison'))
legend(5, 2, legend=c('LM:AR','Actual:Log'),lty=c(2:4),cex=0.8)
# Second Model
J_2 <- data.frame(actual.val2$mean, J2021)
ts.plot(J_2, gpars=list(xlab='Jan-Jul 2021',ylab='Value',
                        lty=c(1:2),main='Jpop Model Comparison'))
legend(5, 40, legend = c('Diffence:MA','Actual'),lty=c(1:2),cex=0.8)
# Third Model
J_3 <- data.frame(actual.val3$mean, J2021)
ts.plot(J_3, gpars=list(xlab='Jan-Jul 2021',ylab='Value',
                        lty=c(2:3),main='Jpop Model Comparison'))
legend(5, 40, legend = c('Drift','Actual'),lty=c(2:3),cex=0.8)

#### Comparing J-pop from a year ago.
J.jun <- (49-30)/30
J.jun
J.jul <- (51-35)/35
J.jul
J.may <- (62-34)/34
J.may

#### Comparing K-pop from a year ago.
K.jun <- (80-100)/100
K.jun
K.jul <- (78-75)/75
K.jul
K.may <- (70-72)/72
K.may

##
comp <- read.csv(file='C:/Users/andre/Documents/School/Stat 479/Project/Data/Comparison.csv')
comp$Week <- as.Date(as.character(comp$Week))

require(ggplot2)
comp.plot <- ggplot(comp, aes(Week)) + 
  geom_line(aes(y = J.pop, colour = "J.pop")) + 
  geom_line(aes(y = K.pop, colour = "K.pop"))

comp.plot + ggtitle("Kpop vs Jpop from July 2020 to July 2021") + 
  xlab('Date') + ylab("Value")
