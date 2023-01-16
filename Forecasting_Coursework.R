# Welcome to my Forecasting Coursework


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
# Simple exponential smoothing
ANN <- ets(in_sample_data, model = "ANN")
# Holt's exponential smoothing with linear trend
AAN <- ets(in_sample_data, model = "AAN", damped = FALSE)
# Automatic forecasting chosen best
ANA <- ets(in_sample_data, model = "ANA", damped = FALSE)
# Damped exponential smoothing
AANd <- ets(in_sample_data, model = "AAN", damped = TRUE)
# Holt-Winter's exponential smoothing (additive seasonality)
AAAd <- ets(in_sample_data, model = "AAA", damped = FALSE)
# Hot-Winter's exponential smoothing (multiplicative seasonality)
MAMd <- ets(in_sample_data, model = "MAM", damped = TRUE)

# Comparing exponential smoothing models
plot(data, ylim = c(3200, 6000))
lines(forecast(ANN, h = 8)$mean, col = "orange", lty = 4)
lines(forecast(AAN, h = 8)$mean, col = "grey", lty = 4)
lines(forecast(ANA, h = 8)$mean, col = "pink", lty = 4)
lines(forecast(AANd, h = 8)$mean, col = "green", lty = 4)
lines(forecast(AAAd, h = 8)$mean, col = "blue", lty = 4)
lines(forecast(MAMd, h = 8)$mean, col = "purple", lty = 4)


# cheeky autofit
fit <- ets(in_sample_data)
summary(fit)
