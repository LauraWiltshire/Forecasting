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

# The main advantage of scale dependent metrics is that they are usually easy to calculate and interpret. However, they can not be used to compare different series, because of their scale dependency (Hyndman, 2006)
# Percentage Error Metrics solve this problem. They are scale independent and used to compare forecast performance between different time series. 
# MAPE, Measures the average percentage error between the actual and forecast values. It provides a good measure of the accuracy of the forecasts when the actual values are of different magnitudes.
# The mean absolute percentage error (MAPE) is one of the most popular used error metrics in time series forecasting. It is calculated by taking the average (mean) of the absolute difference between actuals and predicted values divided by the actuals.
# MAPE, sMAPE, MASE


# Auto-theta model
time_series_set <- 1001:1100
MAPEs_theta <- rep()
sMAPE_theta <- rep()

for (tsi in time_series_set){
  historical_data <- M3[[tsi]]$x
  future_data <- M3[[tsi]]$xx
  horizon <- length(future_data)
  theta_model <- thetaf(historical_data, horizon)
  theta_forecast <- forecast(theta_model, horizon)$mean
  MAPEs_theta <- append(MAPEs, 100 * mean(abs(future_data - theta_forecast)/abs(future_data)))
  sMAPE_theta <- append(sMAPE, 200 * mean(abs(future_data - theta_forecast)/(future_data + theta_forecast)))
}

mean(MAPEs_theta)
mean(sMAPE_theta)



# Auto-arima model
MAPEs_arima <- rep()
sMAPE_arima <- rep()

for (tsi in time_series_set){
  historical_data <- M3[[tsi]]$x
  future_data <- M3[[tsi]]$xx
  horizon <- length(future_data)
  arima_model <- auto.arima(historical_data)
  arima_forecast <- forecast(arima_model, horizon)$mean
  MAPEs_arima <- append(MAPEs_arima, 100 * mean(abs(future_data - arima_forecast)/abs(future_data)))
  sMAPE_arima <- append(sMAPE_arima, 200 * mean(abs(future_data - arima_forecast)/(future_data + arima_forecast)))
}

mean(MAPEs_arima)
mean(sMAPE_arima)



# Auto neural network model
MAPEs_nnar <- rep()
sMAPE_nnar <- rep()

for (tsi in time_series_set){
  historical_data <- M3[[tsi]]$x
  future_data <- M3[[tsi]]$xx
  horizon <- length(future_data)
  nnar_model <- nnetar(historical_data)
  nnar_forecast <- forecast(nnar_model, horizon)$mean
  MAPEs_nnar <- append(MAPEs_nnar, 100 * mean(abs(future_data - nnar_forecast)/abs(future_data)))
  sMAPE_nnar <- append(sMAPE_nnar, 200 * mean(abs(future_data - nnar_forecast)/(future_data + nnar_forecast)))
}

mean(MAPEs_nnar)
mean(sMAPE_nnar)


