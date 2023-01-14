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

# Exploring in-sample data from series ID 1357 of the M3 competition.
in_sample_data <- M3[[1357]]$x
plot(in_sample_data)
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
