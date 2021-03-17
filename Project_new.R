library(zoo)
library(tseries)

climate <- read.csv("~/Desktop/STAT 443 Project/climate.csv")

climate$Date.Time <- as.Date(climate$Date.Time)
climate_time <- climate[climate$Date.Time >= as.Date('2012-01-01') & climate$Date.Time <= as.Date('2019-12-31'),] 
climate_sub <- climate_time[climate_time$Date.Time != as.Date('2012-02-29') & climate_time$Date.Time != as.Date('2016-02-29'),] # for period of 365

temp_na <- climate_sub$Mean.Temp...C.
temp <- na.fill(temp_na,"extend") # get rid of na values

temp.ts <- ts(temp, frequency = 365,start = c(2012,1,1))

train <- 1:(365*7)
temp.train <- ts(temp[train], frequency = 365,start = c(2012,1,1))
temp.test <- ts(temp[-train], frequency = 365,start = c(2019,1,1))

temp.de <- decompose(temp.train) # decomposition
plot(temp.de)
temp.sea <- temp.de$seasonal
seasonal <- temp.sea[1:365] # get the seasonal effect

temp.desea <- temp.train - temp.sea
plot(temp.desea) # no obvious trend or period now
adf.test(temp.desea) # it is stationary

acf(temp.desea, lag = 50) # sine wave, tails off
pacf(temp.desea) # lag = 1 is significant
spec.pgram(temp.desea, spans = 2*sqrt(length(temp.desea))) # low frequencies dominance, no "hidden" period

# conclusion: a positive correlation between historical data and future besides the cyclical effect


# Find the best ARMA model to use Box-Jenkins forecasting
# The following code would run for minites because of ML's Hessian Matrix convergence problem.

# temp.desea.aic <- matrix(0,7,7)
# 
# for (p in 0:6) for (q in 0:6) {
#     temp.desea.aic[p+1,q+1] <- arima(temp.desea, order = c(p, 0, q))$aic
# }

# write.csv(temp.desea.aic,file = 'temp.desea.aic.csv')

# Can just use this code for reading my result
temp.desea.aic <- as.matrix(read.csv("~/Desktop/STAT 443 Project/temp.desea.aic.csv", header=T,row.names = 1))

min(temp.desea.aic) # p = 5, q = 5

arma55 <- arima(temp.desea, order = c(5, 0, 5)) # best ARMA model

tsdiag(arma55)  # diagnostic, good
Box.test(arma55$residuals)

# To see just seasonal vs. seasonal + ARMA

temp.pred.full <- seasonal + predict(arma55, 365)$pred # seasonal + ARMA

temp.mean <- arima(temp.desea, order = c(0, 0, 0))  # just get the mean
temp.pred.sea <- seasonal + predict(temp.mean,365)$pred # pure seasonal 

# They looks extremely close, meaning: at least for long term, historical data doesn't matter
plot(temp.test)
lines(temp.pred.full,col = 'blue')
lines(temp.pred.sea,col = 'red')


# Compare forecasting one lead in the future

seasonal <- decompose(temp.ts)$seasonal
temp.desea <- temp.ts - seasonal

# seasonal + ARMA, 1 time in the future, 365 times
# The following code would run around 20 minites

# error1 <- vector()
# for (i in (365*7):(365*8-1)) {
#   train <- 1:i
#   temp.train <- ts(temp.desea[train], frequency = 365,start = c(2012,1,1))
#   test <- temp[i+1]
#   
#   arma55 <- arima(temp.train, order = c(5, 0, 5))
#   pred <- predict(arma55, 1)$pred + seasonal[i+1]
#   
#   error1[i-(365*7)+1] <- (pred - test)/test
# }

# write.csv(error1, file = 'error1.csv')

# get my result
error1 <- read.csv("~/Desktop/STAT 443 Project/error1.csv", header=T)$x

# just the seasonal way of forecasting
mean <- mean(temp.ts)
error2 <- vector()
for (i in (365*7):(365*8-1)) {
  test <- temp[i+1]
  pred <- seasonal[i+1] + mean
  error2[i-(365*7)+1] <- (pred - test)/test
}

plot(abs(error1),type = 'l', col = 'red') # looks that the red line is a little better
lines(abs(error2),type = 'l')

summary(abs(error1)) # Mean error 46% -> 20%, Median 12% -> 8%
summary(abs(error2))

# Conclusion: knowing historical data would help, but mostly for the abnormal situations.
