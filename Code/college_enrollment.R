# College Enrollment by Race based on Macroeconomic Changes
# Afif Mazhar, Master of Arts in Economics
' Vector Auto-Regressive Model to forecast the relationship between macroeconomic conditions/variables such as 
economic growth, inflation, and the unemployment rate'


# install libraries
library(forecast)
library(urca)
library(tidyverse)
library(mFilter)
library(readr)
library(tseries)
library(strucchange) # need to download and figure out strucchange
library(vars)

# load data
ce_data <- read_csv("Data/college_enrollment_clean.csv")

# graph and view data (simple graph)
ggplot(data = ce_data) + geom_point(mapping = aes(x = Year, y = WHITE))
ggplot(data = ce_data) + geom_point(mapping = aes(x = Year, y = MINORITY))
ggplot(data = ce_data) + geom_point(mapping = aes(x = Year, y = CPI))
ggplot(data = ce_data) + geom_point(mapping = aes(x = Year, y = IP))
ggplot(data = ce_data) + geom_point(mapping = aes(x = Year, y = UNRATE))


# declare Time Series variables
white <- ts(ce_data$WHITE, start = 1976, end = 2019, frequency = 1)
minority <- ts(ce_data$MINORITY, start = 1976, end = 2019, frequency = 1)
cpi <- ts(ce_data$CPI, start = 1976, end = 2019, frequency = 1)
ip <- ts(ce_data$IP, start = 1976, end = 2019, frequency = 1)
unrate <- ts(ce_data$UNRATE, start = 1976, end = 2019, frequency = 1)

# view Time Series data
autoplot(cbind(white,minority))
autoplot(cbind(cpi,ip,unrate))

# transformation of variables to detrend the data (log and diff)
t_white <- diff(diff(log(white)))
t_minority <- diff(diff(log(minority)))
t_cpi <- diff(cpi)
t_ip <- diff(log(ip))
t_unrate <- diff(log(unrate))

# re-view Time Series data
autoplot(cbind(t_white,t_minority))
autoplot(cbind(t_ip,t_unrate))
autoplot(cbind(t_cpi))

# statistical/time series testing (stationarity dfuller and pperron)
adf.test(t_white)
adf.test(t_minority)
adf.test(t_cpi)
adf.test(t_ip)
adf.test(t_unrate)

pp.test(t_white)
pp.test(t_minority)
pp.test(t_cpi)
pp.test(t_ip)
pp.test(t_unrate)

# persistence of time series
acf(t_white, main = "ACF for White College Enrollment")
pacf(t_white, main = "PACF for White College Enrollment")

acf(t_minority, main = "ACF for Minority College Enrollment")
pacf(t_minority, main = "PACF for Minority College Enrollment")

acf(t_cpi, main = "ACF for Economic Output")
pacf(t_cpi, main = "PACF for Economic Output")

acf(t_ip, main = "ACF for Inflation Rate")
pacf(t_ip, main = "PACF for Inflation Rate")

acf(t_unrate, main = "ACF for Unemployment Rate")
pacf(t_unrate, main = "PACF for Unemployment Rate")

# finding optimal lags
ce_data.bv <- cbind(t_white, t_minority, t_cpi, t_ip, t_unrate)
colnames(ce_data.bv) <- cbind("White", "Minority", "Economic Output", "Inflation Rate", "Unemployment rate")

lagselect <- VARselect(ce_data.bv, lag.max = 10, type = "const")
lagselect$selection


# create short-run restrictions (matrices A,B,C)



# run short-run restrictions VAR model (white)

# test short-run restrictions VAR model if it works (varsoc and varstable)

# create SVAR model (white)

# create IRFs (white)



# run short-run restrictions VAR model (minority)

# test short-run restrictions VAR model if it works (varsoc and varstable)

# create SVAR model (minority)

# create IRFs (minority)



# create long-run restrictions (matrices A,B,C)



# run long-run restrictions VAR model (white)

# test long-run restrictions VAR model if it works (varsoc and varstable)

# create SVAR model (white)

# create IRFS (white)




# run long-run restrictions VAR model (minority)

# test long-run restrictions VAR model if it works (varsoc and varstable)

# create SVAR model (minority)

# create IRFS (minority)


