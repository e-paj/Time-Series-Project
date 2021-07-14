# Forecasting Kpop Popularity
This is a final project for a Time Series course. My professor told me I could further work on it. Hence, this!!

## Facts:
* I gathered my data from Google Trends. It tracks the popularity of the search term over time. The maximum is 100.
* For my project, I used the term "K-pop" and the range of my data was from January 2010 to December 2020. 
* Overall, I have 132 observations to use. 
* I used R to analyze my data. 
* (Unfurtunately, I have not saved a copy of my professor's comments about my project, but I do remember some of it.)

## What I have done so far:
* The purpose of course is to know what model could best represent my data and use the model to forecast the popularity of the term in the next 25 years.
* In order to know which model is best, I used six forecasting performance measures to compare each model. These include MSE, RMSE, BIC, etc..
* I initially tried three ARIMA models of which ARIMA(0,1,3) model has the best performance.
* I then tried six seasonal ARIMA models of which SARIMA(0,1,3)(0,1,1)12 model has the best performance. 
* Then I included a drift to each model that has the best performance of which ARIMA(0,1,3) with drift model has the best performance.
* Lastly, from the three models I have I used the measures to pick one. 

## What I can do:
* My professor told me I could compare the popularity with other music genres like Jpop.
* Also, I was told to be more precise about how I got my model in the end.
