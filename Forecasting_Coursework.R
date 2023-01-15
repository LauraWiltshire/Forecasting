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
library(ggplot2)
library(magrittr)

# Loading the data from series ID 1357 of the M3 competition 
data <- M3[[1357]]
in_sample_data <- M3[[1357]]$x
out_sample_data <- M3[[1357]]$xx
plot(data)

# Length of steps ahead to forecast (h) = length of test data (h = 8)
h = length(out_sample_data)

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

# Fitting a linear model to in-sample time series (incl trend & seasonality)
linear_model <- tslm(in_sample_data ~ trend + season)
summary(linear_model)
plot(forecast(linear_model, h = 8, level = c(0.8, 0.95)))
lines(out_sample_data, lty = 2, lwd = 2)
legend("topright", c("Historical data", "Actual future data", "Forecast data"),
       col = c("black", "black", "#31A9F6"),
       lwd = c(1, 2, 2), lty = c(1, 2, 1))

