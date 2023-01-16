# WORK IN PROGRESS ... 

# Part 1: Manual Modelling

# This uses quaterly time series data from the M3 competition (package Mcomp)
# My student ID ends in a 3, so the Series ID I will use is 1357.

# I shall manually select three models, for fitting data and producing forecasts.
# One regression model (with trend and seasonality components)
# One exponential smoothing model
# One ARIMA model

# I will perform data exploration using graphs, statistical descriptive 
# summaries, and statistical tests.
# This includes residual diagnostics, and prediction intervals (80% and 95% CI)
# for the next 8 quaters

# Loading libraries
library(fpp2)
library(Mcomp)
library(forecast)
library(tidyverse)

# Loading the data from series ID 1357 of the M3 competition 
data <- M3[[1357]]
glimpse(data)
in_sample_data <- M3[[1357]]$x
out_sample_data <- M3[[1357]]$xx
plot(data)

# Decomposition of the in-sample data
components_data_a <- decompose(in_sample_data)  # additive decomposition
plot(components_data_a)
components_data_m <- decompose(in_sample_data, 
                               type = "multiplicative")  # multiplicative decomposition
plot(components_data_m)
# From the components plot, it appears that trend is a curved, non-linear line,
# so trend is likely multiplicative.
# There are similar widths and heights of seasonal periods over time,
# so seasonality is likely additive.
# The decomposition of the multiplicative time series shows the residuals (random)
# are closely centered around 1.00, however the additive model yields a much higher
# range and standard deviation, visually. 
# A multiplicative model fits the decomposition better.

# Length of steps ahead to forecast is equal to the length of the test data
h = length(out_sample_data)  # h = 8 

# Fitting a linear model to in-sample time series (incl trend & seasonality)
linear_model <- tslm(in_sample_data ~ trend + season)
summary(linear_model)
plot(forecast(linear_model, h = 8, level = c(0.8, 0.95)))
lines(out_sample_data, lty = 2, lwd = 2)
legend("topright", c("Historical data", "Actual future data", "Forecast data"),
       col = c("black", "black", "#31A9F6"),
       lwd = c(1, 2, 2), lty = c(1, 2, 1))

# Validating performance
accuracy(forecast(linear_model, h = 8), out_sample_data)

# Fitting an exponential smoothing model

# Trying several models, to find the lowest AIC
# First, changing the error type ("A" or "M")
ANN <- ets(in_sample_data, model = "ANN") # Simple exponential smoothing
summary(ANN)  # AIC = 900.9128
MNN <- ets(in_sample_data, model = "MNN")
summary(MNN)  # AIC = 901.4817
# additive error type yields smaller AIC, therefore is a better fit

# Secondly, changing the trend type ("A","M" or "N")
AAN <- ets(in_sample_data, model = "AAN") # Holt's exponential smoothing
summary(AAN)  #AIC = 902.0294
# AMN is a forbidden model type with the ets() function, so we will compare
# different trend types with a baseline multiplicative error instead
MAN <- ets(in_sample_data, model = "MAN")
summary(MAN)  # AIC = 904.0645
MMN <- ets(in_sample_data, model = "MMN")
summary(MMN)  # AIC = 903.6495
MNM <- ets(in_sample_data, model = "MNM")
summary(MNM)  # AIC = 835.4538
# Trend = "none" gives the smallest AIC, therefore is a better fit

# Thirdly, changing the season type ("A", "M", or "N")
ANA <- ets(in_sample_data, model = "ANA")
summary(ANA)  # AIC = 831.2607, the lowest AIC
# ANM is a forbidden model combination 
ANN <- ets(in_sample_data, model = "ANN")
summary(ANN)  # AIC = 900.9128

# Automatically selecting error, trend, and seasonality types 
# also selects ANA as the best combination of component types for the model
ZZZ <- ets(in_sample_data, model = "ZZZ")
summary(ZZZ)

# Graphically comparing forecasts produced by MNM and ANA exponential smoothing models
plot(data, ylim = c(3600, 6000))
lines(forecast(ANA, h = 8)$mean, col = "blue", lty = 1, lwd = 2)
lines(forecast(MNM, h = 8)$mean, col = "green", lty = 1, lwd = 2)
legend("topright", c("Historical data", "Actual future data", "MNM forecast data", "ANA forecast data"),
       col = c("black", "red", "green", "blue"),
       lwd = c(1, 1, 2, 2), lty = c(1, 1, 1, 1))

# Validating ANA and MNM exponential smoothing models, against the training and test sets
accuracy(forecast(ANA, h = 8), out_sample_data)
accuracy(forecast(MNM, h = 8), out_sample_data)

# Residual diagnostics for ANA and MNM exponential smoothing models

checkresiduals(MNM)
checkresiduals(ANA)
# As found during decomposition, a multiplicative model has much smaller residuals
# Even though the ANA model had a slightly "better" AIC score, it has huge residuals
# Histogram: The left tails of the MNM and ANA residual distributions are too long for a normal distribution
# Portmanteau tests: MNM and ANA residuals are distinguishable from a white noise series

# One ARIMA model

