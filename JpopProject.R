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

# we can see from the plot that there seems to be a trend in the data
# it is going down.
# Hence, de-trend this first using linear regression
time <- time(J_pop)
score.lin <- lm(J_pop~time)
plot(score.lin,type='l')
