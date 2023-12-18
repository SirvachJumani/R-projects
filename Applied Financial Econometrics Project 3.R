# Preamble (sets working directory, clears memory, resets) ---------------------

if (!("rstudioapi" %in% installed.packages())) {install.packages("rstudioapi")}
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))# set the working directory to the current R Script location 
y = function(){dev.new();x=par(no.readonly=T); dev.off(); x} #'
par(y()); options(scipen=0); dev.off(); rm(list=ls())


# Packages  --------------------------------------------------------------------

library("readxl")
library("tidyverse")
library("dplyr") 
library("tsibble") 
library("lubridate") 
library("ggplot2") 
library("reshape2") # melt
library("systemfit")
library("fable") #' modern time series modeling but we also work with [{vars}] as
#' fable does not yet have a straightforward IRFs support  
library("vars")
library("Quandl")
library("feasts")
#install.packages("rugarch")  # To estimate DCC-GARCH model
library("rugarch")
#install.packages("rmgarch")  
library("rmgarch")

#_____________________________________________________________________________#

# Q1.1  
# Calculate yearly inflation rate and remove CPI thereafter.

data <-read_excel("Project3.xlsx")  # load data-set and view the summary

summary(data)
head(data)

data <- data %>%
  group_by(date = year(date))%>% 
  summarise_all(mean)


data <- as_tsibble(data, index= date)


INFLRATE <- ts(100*difference(data$CPI)/
              data$CPI, 
              start = 1960, frequency = 12)  # Create Inflation Rate

data$INFLRATE <- INFLRATE  # Add Inflation Rate

data<- data[,-c(2)] # Remove The CPI 
                                 
data <- na.omit(data) # Remove the NA's

#_____________________________________________________________________________#

# Q1.2 Declare the three macro-variables as plain time series objects. Visualize
# the resulting data set. What can be inferred in terms of stationary?

# Create a plain time series object
data <- data %>%
  mutate(
    FEDFUNDS = ts(FEDFUNDS, start = 1960, frequency = 12),
    UNRATE = ts(UNRATE, start = 1960, frequency = 12),
    INFLRATE = ts(INFLRATE, start = 1960, frequency = 12)
  )

# Plots

p <- data %>%
  melt(id = "date") %>% # convert wide to long format
  ggplot() +
  aes(x = date, y = value, colour = variable) +
  geom_line() +
  labs(x = "", y = "in %", color = "Macro-Variables") +
  scale_color_manual(labels = c("Federal Funds Rate", "Unemployment Rate", "Inflation Rate"),
                     values = c("blue", "red", "darkgreen"))
p

#' Comment
#' Time Series are non- stationary, because the stationary mean of the time series
#' change over the time period.

#_____________________________________________________________________________#


# Q1.3 Select a three-variable VAR model with the best fit. Estimate it.


VAR <- as.data.frame(data[, c("FEDFUNDS", "UNRATE", "INFLRATE")])


VARselect(VAR, lag.max = 6, type = c("const"))

fit <- vars::VAR(VAR, p = 1, type = "const") 
fit


#_____________________________________________________________________________#

# Q1.4  Is there autocorrelation left? With 12 lags? With 3 lags?
# How is the lag length to be selected for the test?


Box.test(fit$varresult$FEDFUNDS$residuals, lag = 12, type = "Ljung-Box") # There is autocorrelation
Box.test(fit$varresult$UNRATE$residuals, lag = 12, type = "Ljung-Box")   # There is No autocorrelation
Box.test(fit$varresult$INFLRATE$residuals, lag = 12, type = "Ljung-Box") # There is autocorrelation

Box.test(fit$varresult$FEDFUNDS$residuals, lag = 3, type = "Ljung-Box") # There is autocorrelation 
Box.test(fit$varresult$UNRATE$residuals, lag = 3, type = "Ljung-Box")   # There is autocorrelation
Box.test(fit$varresult$INFLRATE$residuals, lag = 3, type = "Ljung-Box") # There is autocorrelation

#' Comment
#' We could not use these test to reject the Null hypothesis of No autocorrelation
#' Because in both Q test stats with chi-square of freedom = 12 we have two 
#' equations with autocorrelation, In chi-square of freedom = 3  we have three
#' equations with autocorrelation. It may be useful to include more lags.

#_____________________________________________________________________________#

# Q1.5 Increase the VAR(p) order and re-estimate the model. 
# Then, repeat the procedure from the previous question. Are there any changes?

VAR <- as.data.frame(data[, c("FEDFUNDS", "UNRATE", "INFLRATE")])


VARselect(VAR, lag.max = 6, type = c("const"))

fit2 <- vars::VAR(VAR, p = 3, type = "const") 
fit2

Box.test(fit2$varresult$FEDFUNDS$residuals, lag = 12, type = "Ljung-Box")  # There is autocorrelation
Box.test(fit2$varresult$UNRATE$residuals, lag = 12, type = "Ljung-Box")    # There is No autocorrelation
Box.test(fit2$varresult$INFLRATE$residuals, lag = 12, type = "Ljung-Box")  # There is No autocorrelation

Box.test(fit2$varresult$FEDFUNDS$residuals, lag = 3, type = "Ljung-Box") # There is No autocorrelation 
Box.test(fit2$varresult$UNRATE$residuals, lag = 3, type = "Ljung-Box")   # There is No autocorrelation
Box.test(fit2$varresult$INFLRATE$residuals, lag = 3, type = "Ljung-Box") # There is No autocorrelation


#' Comment 
#' We could use these test to reject the Null hypothesis of No autocorrelation
#' Because in both Q test stats with chi-square of freedom = 12 we have two 
#' equations with No autocorrelation, In chi-square of freedom = 3  we have three
#' equations with No autocorrelation. So we can have the high significance and 
#' we fail to reject the Null Hypothesis of No autocorrelation

#_____________________________________________________________________________#

# Q1.6 Pick an ordering and derive the impulse response functions. 
# Explain your choice of the Cholesky ordering and other specifications.

VAR2 <- vars::VAR(data[, c(2, 4, 3)], p = 3)

#' Comment Federal Fund Rate -> Inflation Rate -> Unemployment Rate
#' Federal fund rate does not depend on the Inflation rate and Unemployment
#' rate


IRF_FEDFUND <- irf(VAR2, impulse = "FEDFUNDS", n.ahead = 48, runs = 500) 
IRF_INFLRATE <- irf(VAR2, impulse = "INFLRATE", n.ahead = 48, runs = 500)
IRF_UNRATE <- irf(VAR2, impulse = "UNRATE", n.ahead = 48, runs = 500)


plot(IRF_FEDFUND, xlab = "", main = "Impulse: FEDFUNDS",           
     col = "blue", lwd = 2.5)                                   # plot1
plot(IRF_INFLRATE, xlab = "", main = "Impulse: INFLRATE",      
     col = "blue", lwd = 2.5)                                   # plot2

plot(IRF_UNRATE, xlab = "", main = "Impulse: UNRATE",      
     col = "blue", lwd = 2.5)                                   # plot3 

#' Comment
#' federal fund means to influence the economic growth. As proc increase due to 
#' inflation demand of goods decreases, slowing the overall economic growth.
#' When economy recedes and Unemployment grow.

#_____________________________________________________________________________#

# Q1.7 Give a short but sufficient interpretation of your results with regard 
# to significance, persistence, and stationary.


#' Comment  
#' Impulse of Federal Fund Rate
#' FEDFUND: It's significant for the 3 periods,Persistent reaches mean value slowly, And stationary b/c it converges to 0,
#' INFLRATE:It's significant for the 2 periods, Persistent reaches mean value slowly, And stationary b/c it converges to 0,
#' UNRATE: It's significant for the 1 period, Persistent reaches mean value slowly, And stationary b/c it converges to 0,
#' 
#' Impulse of Inflation Rate
#' FEDFUND: It's not significant, Persistent reaches mean value slowly, And stationary b/c it converges to 0,
#' INFLRATE:It's significant for the 3 periods, Persistent reaches mean value slowly, And stationary b/c it converges to 0,
#' UNRATE: It's not significant, Persistent reaches mean value slowly, And stationary b/c it converges to 0,
#' 
#' Impulse of Unemployment Rate 
#' FEDFUND: It's not significant, Persistent reaches mean value slowly, And stationary b/c it converges to 0,
#' INFLRATE:It's not significant, Persistent reaches mean value slowly, And stationary b/c it converges to 0,
#' UNRATE: It's significant for 3 periods, Persistent reaches mean value slowly, And stationary b/c it converges to 0,


#_____________________________________________________________________________#

# Q2.1 Find a source for exchange rate data and load monthly series of the 
# Euro/US-Dollar and British Pound/US-Dollar exchange rates into your working environment.


GBP_USD <- Quandl("CUR/GBP", 
                  api_key="U3iq1D4xGe4oSSqhWVfZ", 
                  collapse="monthly", type = "xts")
EUR_USD <- Quandl("CUR/EUR", 
                  api_key="U3iq1D4xGe4oSSqhWVfZ", 
                  collapse="monthly", type = "xts")


data1 <- merge(EUR_USD,GBP_USD)
date <- row.names(data.frame(data1))

EUR_USD_Return <- c(100 * diff (log(data1[,1]),lag = 12))
GBP_USD_Return <- c(100 * diff (log(data1[,2]),lag = 12))  

data1 <- data.frame(date, EUR_USD, EUR_USD_Return, GBP_USD, GBP_USD_Return)
data1 <- na.omit(data1) # Remove NA,s

#_____________________________________________________________________________#

# Q2.2 Find the DCC-GARCH-model with the best fit for the log-returns of the two exchange
# rate series.1 Interpret your estimation results.

info_c <- NULL  # finding min AIC for series EUR_USD_Return and GBP_USD_Return
for (i in 0:5){
  for (j in 0:5){
    info_c <- rbind(info_c, c(i,j,
                              AIC(arima(data1[,3], order = c (i,0,j))),
                              AIC(arima(data1[,5], order = c (i,0,j)))
    ))
  }
}

colnames(info_c) <- c("p","q", "AIC_EUR_USD", "AIC_GBP_USD")

which.min(info_c[, "AIC_EUR_USD"]) # arma (3,6)  getting min aic for both series.
which.min(info_c[, "AIC_GBP_USD"]) # arma (3,5)

garch11_mod1 <- ugarchspec(                            
  mean.model         = list(armaOrder = c(3, 6)),
  variance.model     = list(model = "sGARCH", garchOrder = c(1, 1)),
  distribution.model = "norm")

garch11_mod2 <- ugarchspec(                            
  mean.model         = list(armaOrder = c(3, 5)),
  variance.model     = list(model = "sGARCH", garchOrder = c(1, 1)),
  distribution.model = "norm")

dcc_garch_mod0 <- dccspec(uspec = multispec(replicate(2, garch11_mod1)),
                          dccOrder = c(1,1), distribution = "mvnorm")

dcc_garch_mod1 <- dccspec(uspec = multispec(replicate(2, garch11_mod2)),
                          dccOrder = c(1,1), distribution = "mvnorm")


data2 <- data1[, c(3,5)]
head(data2)

## estimating dcc garch

dcc_fit1 <- dccfit(dcc_garch_mod0, data2) # "AIC_EUR_USD" 
dcc_fit2 <- dccfit(dcc_garch_mod1, data2) # "AIC_GBP_USD"

dcc_fit1 # Estimates of DCC-GARCH models.
dcc_fit2

#' Comment dcc_fit1
#' EUR_USD Time Series dcca1 (alpha = 0.08) Not big and significantly different 
#' from  0, and shows the correlation b/w the two series varies over the time.
#' dccb1 (beta = 0.519) moments are less persistent and (alpha + beta < 1) means
#' model is persistent weakly stationay
#'  
#' Comment dcc_fit2
#' GBP_USD Time Series dcca1 (alpha = 0.00) very smnall and significantly different 
#' from  0, and shows the correlation b/w the two series varies over the time.
#' dccb1 (beta = 0.898) moments are quite persistent and (alpha + beta < 1) means
#' model is persistent and weakly stationary.
 

#_____________________________________________________________________________#

# Q2.3 Create a plot of the time-varying correlation implied by DCC-GARCH. Give a
# detailed interpretation of your result.

plot(dcc_fit1)  # press enter and select 4 to see time varying conditional correlation



plot(dcc_fit2)  # press enter and select 4 to see time varying conditional correlation


#' Comment 
#' Conditional correlation b/w the GBP_USD return and EUR_USD return two 
#' currency exchange rates both models have being active almost the same markets.
#' If market shock to the currency rate then both should be effect.




