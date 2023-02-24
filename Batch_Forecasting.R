# Batch Forecasting
# Part 2: Batch forecasting

# Using the time series with IDs 1001 to 1100 (quarterly data) from the M3 
# competition (package Mcomp), after a short description the data set, your aim 
# is to produce and evaluate forecasts using three models that have been 
# introduced in this unit. Potential option could be (not limited to):  
# automatic exponential smoothing model (ets function),
# automatic ARIMA model (auto.arima function), or combined model, etc.  
# Once the forecasts for the out-of-sample (test data or $xx) have been 
# produced, you are asked to evaluate the forecasts using at least two 
# appropriate error measures, give justification on the selection of the error
# measures. Evaluation should be carried out across time series. 
# Compare the accuracy of the proposed selection strategy with that of two 
# suitable benchmark methods (for example, Naive, Damped Exponential Smoothing, etc.)

# Data exploration 5%
# Producing forecasts using three models that have been introduced in the unit 15%
# Evaluation and analysis (including justifications for selected error measures) 10%
# Benchmarking 5%

library(Mcomp)
library(forecast)
library(tidyverse)
library(smooth)

# The main advantage of scale dependent metrics is that they are usually easy to calculate and interpret. However, they can not be used to compare different series, because of their scale dependency (Hyndman, 2006)
# Percentage Error Metrics solve this problem. They are scale independent and used to compare forecast performance between different time series. 
# MAPE, Measures the average percentage error between the actual and forecast values. It provides a good measure of the accuracy of the forecasts when the actual values are of different magnitudes.
# The mean absolute percentage error (MAPE) is one of the most popular used error metrics in time series forecasting. It is calculated by taking the average (mean) of the absolute difference between actuals and predicted values divided by the actuals.
# MAPE, sMAPE, MASE


# Defining which M3 competition time series to use

time_series_set <- 1001:1100

# Data exploration, descriptions
glimpse(M3[[1001]])
glimpse(M3[[1011]])
glimpse(M3[[1022]])

glimpse(M3[[1028]])
glimpse(M3[[1035]])
glimpse(M3[[1038]])
glimpse(M3[[1042]])  # AUSTRALIA-GDP by kind of activity
glimpse(M3[[1045]])  # AUSTRALIA-GDP by kind of activity (Cnst Prices)
glimpse(M3[[1048]])  # AUSTRALIA-Financing of Gross Capital Formation
glimpse(M3[[1050]])  # AUSTRALIA-Gross fixed Capital Formation by institutional sector
glimpse(M3[[1053]])  # AUSTRALIA-Gross fixed Capital Formation by type of capital goods
glimpse(M3[[1058]])  # AUSTRALIA-Fixed Capital Formation institutional sector(Cnst Prices)
glimpse(M3[[1066]])  # AUSTRALIA-Private Consumption expenditure (Current Prices)
glimpse(M3[[1071]])  # (n=26) AUSTRALIA-Private Consumption expenditure (Cnst Prices)
glimpse(M3[[1076]])  # AUSTRALIA-Private Consumption expenditure (Cnst Prices)
glimpse(M3[[1077]])  # AUSTRIA-GDP by expenditure
glimpse(M3[[1083]])  # AUSTRIA-GDP by expenditure (Cnst Prices)
glimpse(M3[[1089]])  # AUSTRIA-GDP by expenditure (Implicit Price Ind.)
glimpse(M3[[1095]])  # AUSTRIA-GDP by cost structure
glimpse(M3[[1100]])  # AUSTRIA-GDP by kind of activity (Cnst Prices)

# Scale and value exploration (mean, sd, range)
means <- rep()
ranges <- rep()
sds <- rep()

for (tsi in 1100:1100) {
 means <- append(means, (M3[[tsi]]$x))
 ranges <- append(ranges, range(M3[[tsi]]$x))
 sds <- append(sds, sd(M3[[tsi]]$x))
}
range(ranges)
mean(means)
mean(sds)

# trend/seasonality count
plot(decompose(M3[[1061]]$x))
ndiffs(M3[[1001]]$x)

# How many time series require differencing?
counter = 0
for (tsi in 1001:1100) {
  ndiffs <- ndiffs(M3[[tsi]]$x, type = "trend")
  print(ndiffs)
  if (ndiffs >= 1) {
    counter = counter + 1
  }
}
print(counter)
8

plot(decompose(M3[[1100]]$x))

# finding time series with adf < .05
time_series_set <- 1001:1100
adf_test <- c()
counter_adf = 0
for (tsi in time_series_set) {
  adf_test <- adf.test(M3[[tsi]]$x)
  if (adf_test$p.value < .05) {
    counter_adf <- counter_adf + 1
    print(tsi)
  }
}
counter_adf

plot(decompose(M3[[1091]]$x))
ndiffs(M3[[1091]]$x)

# Range of ranges of all time series
range_list <- rep()
for (tsi in 1001:1100) {
  range_list <- append(range_list, range((M3[[tsi]]$x)))
}
range(range_list)
print(9630.0-1103.6)  # 8526.4

# How many time-series don't have seasonality with > 8.53 fluctuation?
plot(decompose(M3[[1100]]$x))
no_seasonality <- c(20, 27, 34, 35, 38, 39, 41, 44, 46, 48, 57, 66, 76)
length(no_seasonality)
# Naive method benchmark

MAPEs_naive <- rep()
sMAPE_naive <- rep()
MPE_naive <- rep()

for (tsi in time_series_set){
  historical_data <- M3[[tsi]]$x
  future_data <- M3[[tsi]]$xx
  horizon <- length(future_data)
  naive_model <- naive(historical_data, h = horizon)
  naive_forecast <- forecast(naive_model, horizon)$mean
  MAPEs_naive <- append(MAPEs_naive, 100 * mean(abs(future_data - naive_forecast)/abs(future_data)))
  sMAPE_naive <- append(sMAPE_naive, 200 * mean(abs(future_data - naive_forecast)/(future_data + naive_forecast)))
  MPE_naive <- append(MPE_naive, 100 * mean((future_data - naive_forecast)/future_data))
}

mean(MAPEs_naive)  # 7.134864
sd(MAPEs_naive)  # 7.58888

mean(sMAPE_naive)  # 6.784217
sd(sMAPE_naive)  # 6.086763

mean(MPE_naive)  # -0.6769068
sd(MPE_naive)  # 9.800905


# Auto-arima benchmark model
MAPEs_arima <- rep()
sMAPE_arima <- rep()
MPE_arima <- rep()

for (tsi in time_series_set){
  historical_data <- M3[[tsi]]$x
  future_data <- M3[[tsi]]$xx
  horizon <- length(future_data)
  arima_model <- auto.arima(historical_data)
  arima_forecast <- forecast(arima_model, horizon)$mean
  MAPEs_arima <- append(MAPEs_arima, 100 * mean(abs(future_data - arima_forecast)/abs(future_data)))
  sMAPE_arima <- append(sMAPE_arima, 200 * mean(abs(future_data - arima_forecast)/(future_data + arima_forecast)))
  MPE_arima <- append(MPE_arima, 100 * mean((future_data - arima_forecast)/future_data))
  }

mean(MAPEs_arima)  # 5.683726
sd(MAPEs_arima)  # 6.3324

mean(sMAPE_arima)  # 5.370717
sd(sMAPE_arima)  # 5.429724

mean(MPE_arima)  # -3.440989
sd(MPE_arima)  # 7.60717

# Auto-theta model 

MAPEs_theta <- rep()
sMAPE_theta <- rep()
MPE_theta <- rep()

for (tsi in time_series_set){
  historical_data <- M3[[tsi]]$x
  future_data <- M3[[tsi]]$xx
  horizon <- length(future_data)
  theta_model <- thetaf(historical_data, horizon)
  theta_forecast <- forecast(theta_model, horizon)$mean
  MAPEs_theta <- append(MAPEs, 100 * mean(abs(future_data - theta_forecast)/abs(future_data)))
  sMAPE_theta <- append(sMAPE, 200 * mean(abs(future_data - theta_forecast)/(future_data + theta_forecast)))
  MPE_theta <- append(MPE_theta, 100 * mean((future_data - theta_forecast)/future_data))
  }

mean(MAPEs_theta)  # 5.553969
sd(MAPEs_theta)  # 6.272906

mean(sMAPE_theta)  # 5.303437
sd(sMAPE_theta)  # 5.359509

mean(MPE_theta)  # -1.58446
sd(MPE_theta)  # 8.076944

# Damped exponential smoothing

MAPEs_damped <- rep()
sMAPE_damped <- rep()
MPE_damped <- rep()

for (tsi in time_series_set){
  historical_data <- M3[[tsi]]$x
  future_data <- M3[[tsi]]$xx
  horizon <- length(future_data)
  damped_model <- ets(historical_data, damped = TRUE)
  damped_forecast <- forecast(damped_model, horizon)$mean
  MAPEs_damped <- append(MAPEs_damped, 100 * mean(abs(future_data - damped_forecast)/abs(future_data)))
  sMAPE_damped <- append(sMAPE_damped, 200 * mean(abs(future_data - damped_forecast)/(future_data + damped_forecast)))
  MPE_damped <- append(MPE_damped, 100 * mean((future_data - damped_forecast)/future_data))
}

mean(MAPEs_damped)  # 4.672529
sd(MAPEs_damped)  # 5.088035

mean(sMAPE_damped)  # 4.539832
sd(sMAPE_damped)  # 4.547428

mean(MPE_damped)  # -1.214629
sd(MPE_damped)  # 6.612626

# Auto neural network model
MAPEs_nnar <- rep()
sMAPE_nnar <- rep()
MPE_nnar <- rep()

for (tsi in time_series_set){
  historical_data <- M3[[tsi]]$x
  future_data <- M3[[tsi]]$xx
  horizon <- length(future_data)
  nnar_model <- nnetar(historical_data)
  nnar_forecast <- forecast(nnar_model, horizon)$mean
  MAPEs_nnar <- append(MAPEs_nnar, 100 * mean(abs(future_data - nnar_forecast)/abs(future_data)))
  sMAPE_nnar <- append(sMAPE_nnar, 200 * mean(abs(future_data - nnar_forecast)/(future_data + nnar_forecast)))
  MPE_nnar <- append(MPE_nnar, 100 * mean((future_data - nnar_forecast)/future_data))
}

mean(MAPEs_nnar)  # 6.799899
sd(MAPEs_nnar)  # 8.137133

mean(sMAPE_nnar)  # 6.367925
sd(sMAPE_nnar)  # 6.856817

mean(MPE_nnar)  # -2.528781
sd(MPE_nnar)  # 9.872259


# Median Relative Absolute Error (MdRAE)
MdRAE <- function(y, y_hat, benchmark) {
  return(median(abs(y - y_hat)/abs(y - benchmark)))
}
MdRAE(future_data, theta_forecast, naive_forecast)
MdRAE(future_data, theta_forecast, arima_forecast)
MdRAE(future_data, damped_forecast, naive_forecast)
MdRAE(future_data, damped_forecast, arima_forecast)
MdRAE(future_data, nnar_forecast, naive_forecast)
MdRAE(future_data, nnar_forecast, arima_forecast)
