
rm(list=ls())

# Required Libraries 
library(Quandl) # to obtain data set
library(tidyverse) # FYI: the core of "tidyverse" also includes "tibble"
library(ggplot2); theme_set(theme_bw()) # set theme for ggplot  
library(PerformanceAnalytics) # for normality test
library(tseries) # for time series functions
library(urca) # for existence of trend 
library(tsibble) # modern TS objects
library(tibble)
library(feasts) # feature extraction and statistics
library(fable) # time series modeling and forecasting
library(forecast)
library(broom)
library(gridExtra)

# Question 1 ________________________________________________________

#Q1.1
# For the Data set we send Quandl API with the key U3iq1D4xGe4oSSqhWVfZ

data <- Quandl("LBMA/GOLD", 
               api_key="U3iq1D4xGe4oSSqhWVfZ", 
               collapse="weekly", 
               type = "xts")


#Descriptive Statistics to view NA's in data set column.
summary(data) 

#______________________________________________________________________________#

#Q1.2
# In Conventional Data Frame we are using columns and "USD AM" as base price for analysis

data1 <- data.frame(
  day = as.Date(rownames(data.frame(data))),
  price = as.numeric(data$"USD (AM)"),
  log_return = as.numeric(diff(log(data$"USD (AM)") *100, lag = 5))) 

# Since we multiplied log_return by 100, we can get the weekly percentage change in Time series

#______________________________________________________________________________#

#Q1.3
# Two Variables Time Series Plots 

plot1 <- ggplot(data1, aes(x=day, y=price)) +
  geom_line(color="steelblue") + 
  xlab("") +
  ylab("") +
  theme_bw() +
  scale_x_date(date_breaks = "10 year", date_labels = "%Y")
plot1 



plot2 <- ggplot(data1, aes(x=day, y=log_return)) +
  geom_line(color="steelblue") + 
  xlab("") +
  ylab("") +
  theme_bw() +
  scale_x_date(date_breaks = "10 year", date_labels = "%Y")
plot2 


# For the Normality Test for log_return Time Series
# to remove N/A's
log_return <- na.remove(data1[,"log_return"])
#summary(log_return)

skewness(log_return) # 0.87 (Two tail skewed)
kurtosis(log_return) # 6.74 > 3 The Distribution has the fat tails
jarque.bera.test(na.remove(data1[,3])) # p-value < 2.2e-16 is almost 0 

# Histogram of Log_return
h <- hist(log_return, breaks = 120, col = "blue", 
          xlab = "Log_Return", main ="Histogram of LBMA / Gold Log_Return", 
          xlim = c(-20,20))

# Additional corresponding normal density
xfit <- seq(min(log_return), max(log_return), length = 1000) 
yfit <- dnorm(xfit, mean = mean(log_return), sd = sd(log_return)) 
yfit <- yfit * diff(h$mids[1:2]) * length(log_return) 
lines(xfit, yfit, col="red", lwd=2) 
abline(v = mean(log_return), col="red") 

# Log_return are normally distributed, it is Skewness and exhibits heavy tails
# Hence with JB- Test, clearly reject the null hypothesis of normality.
# Weekly data of log_return is Skewness and follow the Normal distribution. 

# For the Normality Test of Price Time Series
price <- na.remove(data1[,"price"])
#summary(price)

skewness(price) # 1.13 > 0 positively skewed and right tail.
kurtosis(price) # 0.11 < 3 The Distribution do not have fat tails
jarque.bera.test(na.remove(price)) # p-value < 2.2e-16 is almost 0 

# Histogram for Price

h <- hist(price, breaks = 10, col = "blue", 
          xlab = "price", main ="Histogram of LBMA / Gold Price",
          xlim = c(10,2000))

# Additional corresponding normal density (Not in bell)
xfit1 <- seq(min(price), max(price), length = 1000) 
yfit1 <- dnorm(xfit1, mean = mean(price), sd = sd(price)) 
yfit1 <- yfit1 * diff(h$mids[1:2]) * length(price) 
lines(xfit1, yfit1, col="red", lwd=2) 
abline(v = mean(price), col="red") 

# Price are not normally distributed. In particular, the distribution is positively skewed.
# Hence with JB- Test, clearly reject the null hypothesis of normality. 

#______________________________________________________________________________#

#Q1.4

# Highest and the lowest value of the log-return and price

high_lr <- setDT(data1)[, .SD[which.max(log_return)]]  
low_lr <- setDT(data1)[, .SD[which.min(log_return)]] 

high_pr <- setDT(data1)[, .SD[which.max(price)]]
low_pr <- setDT(data1)[, .SD[which.min(price)]] 

# Table of the Highest and Lowest values of the log_return and price

values_table <- data.frame(Variables = c("Log_return High", "Log_return Low", "Price High", "Price Low"),
                           Date = c(high_lr$day, low_lr$day, high_pr$day, low_pr$day),
                           Values = c(high_lr$log_return, low_lr$log_return, high_pr$price, low_pr$price)
                           )
values_table

#______________________________________________________________________________#

#Q1.5
# Converting weekly time series into monthly

 # create variable year month

# Assign the average values to the weeks of the month.
data1 <- mutate(data1, day = yearmonth(day)) # year month date conversion
data1 <- data1 %>% 
          group_by(day) %>% 
          summarise_all(mean) # aggregating  by mean all values.
data1 <- as_tsibble(data1, index = day) # modern timeseries object for data set.
data1 <- na.omit(data1) # Remove the NA's

#______________________________________________________________________________#

#Q1.6
# Autocorrelation of log_return

p <- data1$log_return %>%
  acf(lag = 360) # autocorrelation function
plot(p[1:360], main= "LBMA/GOLD Monthly ACF of Log_retun") 

# There is the cyclic trend and  no autocorrelation in log_return time series.

#______________________________________________________________________________#

#Q1.7
# Unit root test in for log_return with a trend

summary(ur.df(data1$log_return, selectlags = "AIC", type ="trend"))
summary(ur.df(data1$log_return, selectlags = "BIC", type ="trend"))

# Trend is not significant and t-test (-17.3618) < -3.12 lie in the reject area 
# We do reject the null Ho  non stationary => not Non-stationary / 
# not having a unit root / not having stochastic trend

#______________________________________________________________________________#

#Q1.8

# Estimate ARIMA(0,0,0)-ARIMA(4,0,4)

arma <- data1 %>% # write down some possible combinations of AR(p) and MA(q) orders
  model(
    arma00 = ARIMA(log_return ~ 1 + pdq(0, 0, 0) + PDQ(0, 0, 0)),
    arma10 = ARIMA(log_return ~ 1 + pdq(1, 0, 0) + PDQ(0, 0, 0)),
    arma20 = ARIMA(log_return ~ 1 + pdq(2, 0, 0) + PDQ(0, 0, 0)),
    arma30 = ARIMA(log_return ~ 1 + pdq(3, 0, 0) + PDQ(0, 0, 0)),
    arma40 = ARIMA(log_return ~ 1 + pdq(4, 0, 0) + PDQ(0, 0, 0)),
    arma01 = ARIMA(log_return ~ 1 + pdq(0, 0, 1) + PDQ(0, 0, 0)),
    arma11 = ARIMA(log_return ~ 1 + pdq(1, 0, 1) + PDQ(0, 0, 0)),
    arma21 = ARIMA(log_return ~ 1 + pdq(2, 0, 1) + PDQ(0, 0, 0)),
    arma31 = ARIMA(log_return ~ 1 + pdq(3, 0, 1) + PDQ(0, 0, 0)),
    arma41 = ARIMA(log_return ~ 1 + pdq(4, 0, 1) + PDQ(0, 0, 0)),
    arma02 = ARIMA(log_return ~ 1 + pdq(0, 0, 2) + PDQ(0, 0, 0)),
    arma12 = ARIMA(log_return ~ 1 + pdq(1, 0, 2) + PDQ(0, 0, 0)),
    arma22 = ARIMA(log_return ~ 1 + pdq(2, 0, 2) + PDQ(0, 0, 0)),
    arma32 = ARIMA(log_return ~ 1 + pdq(3, 0, 2) + PDQ(0, 0, 0)),
    arma42 = ARIMA(log_return ~ 1 + pdq(4, 0, 2) + PDQ(0, 0, 0)),
    arma03 = ARIMA(log_return ~ 1 + pdq(0, 0, 3) + PDQ(0, 0, 0)),
    arma13 = ARIMA(log_return ~ 1 + pdq(1, 0, 3) + PDQ(0, 0, 0)),
    arma23 = ARIMA(log_return ~ 1 + pdq(2, 0, 3) + PDQ(0, 0, 0)),
    arma33 = ARIMA(log_return ~ 1 + pdq(3, 0, 3) + PDQ(0, 0, 0)),
    arma43 = ARIMA(log_return ~ 1 + pdq(4, 0, 3) + PDQ(0, 0, 0)),
    arma04 = ARIMA(log_return ~ 1 + pdq(0, 0, 4) + PDQ(0, 0, 0)),
    arma14 = ARIMA(log_return ~ 1 + pdq(1, 0, 4) + PDQ(0, 0, 0)),
    arma24 = ARIMA(log_return ~ 1 + pdq(2, 0, 4) + PDQ(0, 0, 0)),
    arma34 = ARIMA(log_return ~ 1 + pdq(3, 0, 4) + PDQ(0, 0, 0)),
    arma44 = ARIMA(log_return ~ 1 + pdq(4, 0, 4) + PDQ(0, 0, 0))
  )

glance(arma)
glance(arma)[which.min(glance(arma)[["AIC"]]), ] # arma43
glance(arma)[which.min(glance(arma)[["BIC"]]), ] # arma01

# We take d = 0 as we not using the integrated part of the arima on using AR 
#and MA in (AR,I,MA) = (p, d, q)
# If d=0, this means that our data does not have trend
# stationary without having  fluctuation

#______________________________________________________________________________#

#Q1.9 
# Take AIC model and run test for a residual autocorrelation
arma %>%
  select(arma43) %>%
  report() #  summary() 

a_model<- arma %>%
  residuals() %>% # get the residuals from arma04
  group_by(.model) %>% # groups the residuals by model
  features(features = ljung_box, lag = 20) # Ljung-Box test for autocorrelation

a_model[24,] 
# Using the AIC model arma43
# P-value 0.0961 is greater 0.05, fail to reject the null hypothesis Ho (no autocorrelation)
#Stick with this model

#______________________________________________________________________________#

#Q1.10 
# Forecast the log-return form May 2020 to May 2021. Measure the accuracy
#formally and graphically. Comment on the results.

data_May2020 <- data1 %>% #  2020 End at MAy for out-of-sample forecasts
  filter(day >= yearmonth("1968-04")) %>% # beginning of TS
  filter(day <= yearmonth("2020-05")) # end of TS

arma_May2020 <- data_May2020 %>% # possible combinations of AR(p) and MA(q) orders
  model(
    arma00 = ARIMA(log_return ~ 1 + pdq(0, 0, 0) + PDQ(0, 0, 0)),
    arma10 = ARIMA(log_return ~ 1 + pdq(1, 0, 0) + PDQ(0, 0, 0)),
    arma20 = ARIMA(log_return ~ 1 + pdq(2, 0, 0) + PDQ(0, 0, 0)),
    arma30 = ARIMA(log_return ~ 1 + pdq(3, 0, 0) + PDQ(0, 0, 0)),
    arma40 = ARIMA(log_return ~ 1 + pdq(4, 0, 0) + PDQ(0, 0, 0)),
    arma01 = ARIMA(log_return ~ 1 + pdq(0, 0, 1) + PDQ(0, 0, 0)),
    arma11 = ARIMA(log_return ~ 1 + pdq(1, 0, 1) + PDQ(0, 0, 0)),
    arma21 = ARIMA(log_return ~ 1 + pdq(2, 0, 1) + PDQ(0, 0, 0)),
    arma31 = ARIMA(log_return ~ 1 + pdq(3, 0, 1) + PDQ(0, 0, 0)),
    arma41 = ARIMA(log_return ~ 1 + pdq(4, 0, 1) + PDQ(0, 0, 0)),
    arma02 = ARIMA(log_return ~ 1 + pdq(0, 0, 2) + PDQ(0, 0, 0)),
    arma12 = ARIMA(log_return ~ 1 + pdq(1, 0, 2) + PDQ(0, 0, 0)),
    arma22 = ARIMA(log_return ~ 1 + pdq(2, 0, 2) + PDQ(0, 0, 0)),
    arma32 = ARIMA(log_return ~ 1 + pdq(3, 0, 2) + PDQ(0, 0, 0)),
    arma42 = ARIMA(log_return ~ 1 + pdq(4, 0, 2) + PDQ(0, 0, 0)),
    arma03 = ARIMA(log_return ~ 1 + pdq(0, 0, 3) + PDQ(0, 0, 0)),
    arma13 = ARIMA(log_return ~ 1 + pdq(1, 0, 3) + PDQ(0, 0, 0)),
    arma23 = ARIMA(log_return ~ 1 + pdq(2, 0, 3) + PDQ(0, 0, 0)),
    arma33 = ARIMA(log_return ~ 1 + pdq(3, 0, 3) + PDQ(0, 0, 0)),
    arma43 = ARIMA(log_return ~ 1 + pdq(4, 0, 3) + PDQ(0, 0, 0)),
    arma04 = ARIMA(log_return ~ 1 + pdq(0, 0, 4) + PDQ(0, 0, 0)),
    arma14 = ARIMA(log_return ~ 1 + pdq(1, 0, 4) + PDQ(0, 0, 0)),
    arma24 = ARIMA(log_return ~ 1 + pdq(2, 0, 4) + PDQ(0, 0, 0)),
    arma34 = ARIMA(log_return ~ 1 + pdq(3, 0, 4) + PDQ(0, 0, 0)),
    arma44 = ARIMA(log_return ~ 1 + pdq(4, 0, 4) + PDQ(0, 0, 0))
  )

glance(arma_May2020)
glance(arma_May2020)[which.min(glance(arma_May2020)[["AIC"]]), ] # arma43


# Forecast with ARMA(4,3) model for year May 2020 to May 2021
Forecast_arma43 <- data_May2020 %>%
  model(ARIMA(log_return ~ 1 + pdq(4, 0, 3) + PDQ(0, 0, 0))) %>%
  forecast(h = 12, level = 95) # future prediction of a TS from the fitted model, 12 periods ahead

accuracy(Forecast_arma43, data1) # evaluation of the FC model with of descriptive measures

# Naive forecast
Forecast_naive43 <- data_May2020 %>%
  model(NAIVE(log_return)) %>%
  forecast(h = 12)

accuracy(Forecast_naive43, data1) # accuracy Naive FC.

#Plot the forecast
plot1 <- autoplot(Forecast_arma43, slice(data1, (n() - 20):n())) +
  xlab("") + ylab("ARIMA(4,0,3)")


plot2 <- autoplot(Forecast_naive43, slice(data1, (n() - 20):n())) + # "slice() the tail()"
  xlab("") + ylab("Naive")

plot3 <- grid.arrange(plot1, plot2, ncol = 1, nrow = 2) 
# combine multiple plots in one
plot(plot3)

#ARIMA(4,0,3) forecast show the realizations has dynamics over the months
#Naive forecast has straight line or same level for every month.

#______________________________________________________________________________#
