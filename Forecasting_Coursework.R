# WORK IN PROGRESS ... 

# Part 1: Manual Modelling

# One regression model (with trend and seasonality components)
# One exponential smoothing model
# One ARIMA model

# Including data exploration using graphs, statistics, residual diagnostics, 
# with prediction intervals (80% and 95% CI) for the next forecasted 8 quaters

# Loading libraries
library(fpp2)
library(Mcomp)
library(forecast)
library(tidyverse)
library(tseries)

# Loading the data from series ID 1357 of the M3 competition 
data <- M3[[1357]]
glimpse(data)
in_sample_data <- M3[[1357]]$x
out_sample_data <- M3[[1357]]$xx
plot(data)

# Decomposition of the in-sample data
components_data_a <- decompose(in_sample_data)  # additive
plot(components_data_a)
components_data_m <- decompose(in_sample_data, 
                               type = "multiplicative")  # multiplicative
plot(components_data_m)
# Trend is non-linear, possibly multiplicative.
# Seasonality is possibly additive.
# Multiplicative residuals < additive model residuals
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

# Validating performance of linear model
accuracy(forecast(linear_model, h = 8), out_sample_data)

# Fitting an exponential smoothing model
# "A" = additive, "M" = multiplicative, "N" = none, "Z" = automatic
# Better fitting models have lower AIC

# First, changing the error type ("A" or "M")
ANN <- ets(in_sample_data, model = "ANN") 
  summary(ANN)  # AIC = 900.9128
MNN <- ets(in_sample_data, model = "MNN")
  summary(MNN)  # AIC = 901.4817
# Error better as A than M

# Secondly, changing the trend type ("A","M" or "N")
AAN <- ets(in_sample_data, model = "AAN")
  summary(AAN)  #AIC = 902.0294
# AMN is a forbidden model type with the ets() function, so we will compare
# different trend types with a baseline multiplicative error instead
MAN <- ets(in_sample_data, model = "MAN")
  summary(MAN)  # AIC = 904.0645
MMN <- ets(in_sample_data, model = "MMN")
  summary(MMN)  # AIC = 903.6495
MNM <- ets(in_sample_data, model = "MNM")
  summary(MNM)  # AIC = 835.4538
# Trend better as N than M/A

# Thirdly, changing the season type ("A", "M", or "N")
ANA <- ets(in_sample_data, model = "ANA")
  summary(ANA)  # AIC = 831.2607, lowest AIC
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
# A multiplicative model has much smaller residuals
# ANA model had a slightly "better" AIC score, but MNM has better residuals 
# Histogram: negative skew of the MNM and ANA residual distributions 
# Portmanteau tests: MNM & ANA residuals distinguishable from white noise series

# Plotting MNM ES model forecast, 80% & 90% confidence intervals
plot(forecast(MNM, h = 8, level = c(0.8, 0.95)))
lines(out_sample_data, lty = 2, lwd = 2)
legend("topright", c("Historical data", "Actual future data", "Forecast data"),
       col = c("black", "black", "#31A9F6"),
       lwd = c(1, 2, 2), lty = c(1, 2, 1))


# ARIMA model
# Plot the data. Identify any unusual observations.
tsdisplay(in_sample_data)

# If necessary, transform the data (using a log or a Box-Cox transformation) to stabilise the variance.

# If the data are non-stationary: take first differences of the data until the data are stationary.
nsdiffs(in_sample_data)  # required degree of seasonal differencing is 1
season_diff <- diff(in_sample_data, 1)
ndiffs(season_diff, test = "adf")  # no further differencing required

# Plot of time-series data before and after differencing
diff_vs_no_diff <- cbind("Original data" = in_sample_data,
              "Seasonal differences" = diff(in_sample_data, 1))
autoplot(diff_vs_no_diff, facets=TRUE) +
  xlab("Time") +
  ggtitle("Employment")

# ACF and PACF plots for differenced data
tsdisplay(diff(in_sample_data, 1))
# estimated AR(1) or AR(5) from PACF
# estimated seasonal AR, 1
# estimated I terms, 1
# estimated MA(1) from ACF
# estimated seasonal MA, 4?

# Examine the ACF/PACF: Is an AR(p) or MA(q) model appropriate?
# Try your chosen model(s), and use the AICc to search for a better model.
# Check the residuals from your chosen model by plotting the ACF of the residuals, and doing a portmanteau test of the residuals. If they do not look like white noise, try a modified model.
# Once the residuals look like white noise, calculate forecasts.

