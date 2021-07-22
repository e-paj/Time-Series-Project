Jpop <- read.csv(file="C:/Users/andre/Documents/School/Stat 479/Project/Data/USAJpop.csv")
J_pop <- ts(Jpop,frequency = 12,start=c(2010,1))

library(TSA)
library(forecast)
library(tseries)

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
plot(resid(score.lin),type='l', main="Detrended")
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
