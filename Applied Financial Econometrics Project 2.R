
# Preamble (sets working directory, clears memory, resets) ---------------------

if (!("rstudioapi" %in% installed.packages())) {install.packages("rstudioapi")}
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))# set the working directory to the current R Script location 
y = function(){dev.new();x=par(no.readonly=T); dev.off(); x} #'
par(y()); options(scipen=0); dev.off(); rm(list=ls())

# or rewrite our preamble, e.g., as a function (you can experiment...):

currentwd <- function() { #'
  path <- rstudioapi::getSourceEditorContext()$path
  path <- unlist(strsplit(path, "/"))
  path <- paste0(path[-length(path)], collapse = "/")
  return(path)
} # ... etc.

# Packages  --------------------------------------------------------------------
setwd(currentwd())
library("readxl") # to load xlsx file
library("ggplot2")
library("magrittr") # pipe %>%
library("tidyverse")
library("tsibble")
library("lubridate")
library("quantmod")
library("moments")  ## skewness / kurtosis
library("fable")
library("feasts")
library("rugarch")
library("cowplot") 
library("tseries")
library("PerformanceAnalytics")
library("dplyr")
library("GAS")


UTblue <- rgb(5/255, 110/255, 167/255, 1)

theme_set(theme_bw()) 


#-------------------- Question No1----------------------------#

# Q1.1 

data <- read_excel("EUROSTOXX50.xlsx")

  data <- data %>%
  mutate(log_return = difference(log(Close), 1) * 100) %>% # create a log-return variable
  tail(-1) %>%
  arrange(Date) %>%
  mutate(tradingday = row_number(Date)) %>%
  as_tsibble(index = tradingday) 

#' Comment
#' Our financial data is very irregular every month have different amount of days 
#' so we define the trading days in our data set.

p <- data %>%
  ggplot() +
  geom_line(aes(Date, Close), col = UTblue, size = 1) +
  theme_bw() +
  xlab("Years") +
  ylab("Close Price") +
  theme(legend.position = "none") 
p

p <- data %>%
  ggplot(aes(Date, log_return)) +
  geom_line(aes(col = Symbol), col = UTblue, size = 1) +
  theme_bw() +
  xlab("Years") +
  ylab("Log_return") +
  theme(legend.position = "none")
p

n <- length(data$log_return)
p <- data.frame(x = rnorm(n), return = data$log_return) %>%
  ggplot(aes(return, fill = "yellow")) +
  geom_histogram(aes(y = ..density..), bins = 60,
                 alpha = 0.7, fill = UTblue, col = "black") +
  stat_function(fun = dnorm, aes(x = x, col = "red"), size = 1.5) +
  xlim(c(-20, 20)) + # limit the x range for a better aesthetics
  ylab("Density") + xlab("log_return") +
  theme(legend.position = "none")
p 

#' Comment
#' Our distribution is leptocurtic (more mass at the center and at the tails)

summary(data$log_return) 

#' Comment
#' In summary command to find NA's in log_return column. 
#' And we do not find find the NA's in return column.

kurtosis(data$log_return) #5.56

#' Comment
#' kurtosis shows that the return have the leptokurtic distribution.
#' And distribution is independently and identically distributed
#' and the uncertainty in financial Market evolving with the volatility clustering. 

#_______________________________________________________________________________

#Q1.2 

arma <- data %>% # write down some possible combinations of AR(p) and MA(q) orders
  model(
    arma50 = ARIMA(log_return ~ 1 + pdq(5, 0, 0) + PDQ(0, 0, 0)),
    arma51 = ARIMA(log_return ~ 1 + pdq(5, 0, 1) + PDQ(0, 0, 0)),
    arma52 = ARIMA(log_return ~ 1 + pdq(5, 0, 2) + PDQ(0, 0, 0)),
    arma53 = ARIMA(log_return ~ 1 + pdq(5, 0, 3) + PDQ(0, 0, 0)),
    arma54 = ARIMA(log_return ~ 1 + pdq(5, 0, 4) + PDQ(0, 0, 0)),
    arma55 = ARIMA(log_return ~ 1 + pdq(5, 0, 5) + PDQ(0, 0, 0)),
  )


glance(arma)
glance(arma)[["AIC"]]
glance(arma)[which.min(glance(arma)[["AIC"]]), ]

#' Comment
#' We find the smallest value of the AIC at ARMA 53 model. 
#' We Select ARMA 53 among them. And the lag of 5 for weekly influence of data.

fit <- data %>% 
  model(ar53 = ARIMA(log_return ~ 1 + pdq(5, 0, 3) + PDQ(0, 0, 0)))

fit %>%
  select(ar53) %>%
  fabletools::report()


data %>% 
  select(log_return) %>% 
  feasts::PACF(lag_max = 20) %>% 
  autoplot() +
  xlab("Lags") +
  ylab("ACF") 

#' Comment
#' we some out-liers in the financial return data set. 
#' Some auto-correlation exceed by chance at the residual probability at 5% 


p <- fit %>% # this ACF (estimated autocorrelations of the residuals)
  select(ar53) %>% 
  residuals() %>%
  select(.resid) %>%
  feasts::ACF(lag_max = 20) %>% # using 20 lags maximum here
  autoplot() +
  xlab("Lags") +
  ylab("ACF") 
p


p <- fit %>%
  select(ar53) %>%
  residuals() %>%
  mutate(.resid2 = (.resid)^2) %>% # add a vector
  select(.resid2) %>%
  feasts::ACF(lag_max = 20) %>% # using 20 lags maximum here
  autoplot() +
  xlab("Lags") +
  ylab("ACF") 
p
#' Comment
#' We use ARMA (5,3) model to filter out autocorrelation in data but ARMA model
 

fit %>%
  augment() %>%
  features(.resid, ljung_box, lag = 20) 

#' Comment
#' P-value is 0.849  at 20 degrees of freedom 
#' we failed to reject null hypothesis "No Autocorrelation".

fit %>%
  augment() %>% 
  features(.resid^2, ljung_box, lag = 20) 

#' Comment
#' P-value is 0 at 20 degrees of freedom and we reject "No Autocorrelation".
#' In every period there is auto-correlation.So the ARMA model is not good to
#' filter out the autocorrelation we use GARCH model to filter out the autocorrelation. 

#_______________________________________________________________________________

# Q1.3

#' Comment
#' Yes absolute residual result support the volatility clustering, because
#' they are not only uncorrelated but also homoskedastic that is the
#' commpeletly independent in each time period and variance for 
#' every time period is the same  and unexplained fluctuations have 
#' no dependencies in the second moments.

#_______________________________________________________________________________

# Q1.4

#### ARMA(5,3)- GJR- GARCH(1,1) Model ####

specGJRGARCH11 <- ugarchspec(
  mean.model = list(armaOrder = c(5, 3)),
  variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
  distribution.model = "norm"
)

GJRGARCH11 <- ugarchfit(specGJRGARCH11, na.omit(data$log_return))
GJRGARCH11 

#' Comment
#' gamma1 = -0.123738 indicates that the data is not asymmetry
#' In Eurostoxx50 data, we do not have the leverage effect.

#_______________________________________________________________________________

# Q1.5

specEGARCH11 <- ugarchspec(
  mean.model = list(armaOrder = c(5, 3)),
  variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
  distribution.model = "norm"
)

EGARCH11 <- ugarchfit(specEGARCH11, na.omit(data$log_return))
EGARCH11 

#' Comment
#' alpha1 = 0.106392 indicates that the data is not asymmetry
#' In the Eurostoxx50 data, we do not have the leverage effect.

#-------------------- Question No2----------------------------#

# Q2.1


plot(ecdf(data$log_return), col='black', 
     xlab = 'log_Return', 
     main ='ECDF of log_Return')  

#' converges to the Normal ECDF

data<- data[,-c(4)] # to remove trading data

#_______________________________________________________________________________

# Q2.2


#' Comment Value at Risk (VaR) at 99%
#' In whole sample VaR with the Confidence interval 99% show the shortfall 
#' probability p quantile of the return distribution, If return lie above the 
#' VaR 99% confidence interval density and we found not to extreme losses 
#' in return. If Area below th VaR 99% Confidence interval we found extreme loss.


#' Comment Expected Shortfall (ES) at 99%
#' ES at the 99% Confidence interval, if you have extreme shortfall return is
#' shorter than the VaR 99% and Expected shortfall gives us the mean shortfall
#' of lower than VaR 99%

#_______________________________________________________________________________

#Q2.3: Historical Simulation


VaR01 <- rep(NA, dim(data)[1]) #' Creating blank values

data <- as.data.frame(data) # to run below code converted to data frame.


for(i in 1001:(dim(data)[1])){ # construct quantiles
  
  VaR01[i] <- quantile(data[((i-250):(i-1)), "log_return"], c(0.01)) #Var 99%
  
}


#_______________________________________________________________________________

#Q2.4 Historical Simulations plot with log_Returns 

## GARCH (1,1) with Normal Distribution

spec1 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                    mean.model=list(armaOrder=c(5,3), include.mean = TRUE), 
                    distribution="norm") 

roll1 <- ugarchroll(spec1, data[,"log_return"], n.start=1000, refit.window = "moving", 
                    VaR.alpha =  0.01) #' VaR tail level to calculate


## GARCH(1,1) with Student-t Distribution

spec2 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                  mean.model=list(armaOrder=c(5,3), include.mean = TRUE), 
                  distribution="sstd") 

roll2 <- ugarchroll(spec2, data[,"log_return"], n.start=1000, refit.window = "moving", 
                           VaR.alpha = 0.01) #' VaR tail level to calculate


## GJR-GARCH(1,1) with Normal Distribution

spec3 <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)), 
                  mean.model=list(armaOrder=c(5,3), include.mean = TRUE), 
                  distribution="norm") 

roll3 <- ugarchroll(spec3, data[,"log_return"], n.start=1000, refit.window = "moving", 
                           VaR.alpha = 0.01)

## GJR-GARCH(1,1) with Student-t Distribution

spec4 <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)), 
                  mean.model=list(armaOrder=c(5,3), include.mean = TRUE), 
                  distribution="sstd") 

roll4 <- ugarchroll(spec4, data[,"log_return"], n.start=1000, refit.window = "moving", 
                           VaR.alpha = 0.01)


# Create combination
VaR_11 <- c(rep(NA, 1000),roll1@forecast$VaR[,1])    # GARCH (1,1) with Normal distribution
VaR_12 <- c(rep(NA, 1000),roll2@forecast$VaR[,1])    # GARCH (1,1) with Student t distribution   
VaR_13 <- c(rep(NA, 1000),roll3@forecast$VaR[,1])    # GARCH (1,1) with Normal gjr-GARCH distribution
VaR_14 <- c(rep(NA, 1000),roll4@forecast$VaR[,1])    # GARCH (1,1) with Student gjrGARCH distribution


# Compilation of the Estimated and Finalized

VaR99 <- cbind(data, VaR01, VaR_11, VaR_12, VaR_13, VaR_14)

head(VaR99)  
tail(VaR99)

VaR99ts <- ts(VaR99[-(1:1000),], 
               start = c(2010, 02, 16), 
               frequency = 252)
head(VaR99ts) # 
plot(VaR99ts[,c(3, 4, 7,8)], 
     plot.type="single", 
     ylab=NA, col=c('black','red','green','blue'), 
     xlab =NA, main="VaR(99%)")
legend(x=2010, y=-14.5, legend=c("Log_Return","Historical Simulation", 
                                 "Normal ARMA(5,1)-GJR-GARCH(1,1)", 
                                 "Student t ARMA(5,1)-GJR-GARCH(1,1)"), 
       lty= c(1,1), col=1:2, bty="n")



#_______________________________________________________________________________

#Q2.5 Backtest VaR estimate 


BT99Tab <- matrix(NA,5,3)
for(i in 4:8){ 
  
  BT <- BacktestVaR(VaR99ts[,3],VaR99ts[,i],0.01)
  BT99Tab[(i-3),1] <- BT$AE
  BT99Tab[(i-3),2] <- BT$LRuc[2]
  BT99Tab[(i-3),3] <- BT$LRcc[2]
  
}

rownames(BT99Tab) <- c("Historical Simulation", 
                      "Normal ARMA(5,3)-GARCH(1,1)", 
                      "Student-t ARMA(5,3)-GARCH(1,1)", 
                      "Normal ARMA(5,3)-GJR-GARCH(1,1)", 
                      "Student-t ARMA(5,3)-GJR-GARCH(1,1)")

colnames(BT99Tab) <- c("theta", "UC", "CC")
BT99Tab

#' Comment
#' Historical simulation is the good model for the Back test 
#' theta is 1.213 and P-value of UC can reject Ho at 45% and 
#' P-value CC can reject Ho at 56%  

