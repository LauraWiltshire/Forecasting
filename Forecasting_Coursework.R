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
library(MTS)

# Loading the data from series ID 1357 of the M3 competition 
data <- M3[[1357]]
glimpse(data)
in_sample_data <- M3[[1357]]$x
out_sample_data <- M3[[1357]]$xx

# Time plot
plot(data, main = "Quaterly Industry Employment in Finland")
legend("topright", c("Historical Data", "Future Data"),
       col = c("black", "red"),
       lwd = c(1, 1))

# Seasonal plot
ggseasonplot(in_sample_data, year.labels = TRUE) +
  ggtitle("Seasonal Plot of Employment in Finland")

# Lag plot
gglagplot(in_sample_data) +
  scale_x_continuous(n.breaks = 3) +
  title(main = "Lag Plot of Employment in Finland")

# ACF plot
ggAcf(in_sample_data) +
  labs(title = "ACF Employment in Finland")

# QQnorm plots
qqnorm(in_sample_data, 
       main = "Normal Q-Q Plot for Employment in Finland")  # no transformation
qqline(in_sample_data)
hist(in_sample_data,
     main = "Histogram for Employment in Finland", 
     xlab = "")

qqnorm(in_sample_data^3,
       main = "Normal Q-Q Plot for Cube-Transformed Employment in Finland")  # cube root transform
qqline(in_sample_data^3)
hist(in_sample_data^3,
     main = "Histogram for Cube-Transformed Employment in Finland")

qqnorm((in_sample_data^6))  # closest to normal dist, but ^6 not traditional
qqline((in_sample_data^6))
hist((in_sample_data^6))
plot((in_sample_data^6))


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

# Seasonal ARIMA model
# Plot the data. Identify any unusual observations.
tsdisplay(in_sample_data)
# ACF slow fall, PACF significant at lag 1, so AR(1)

# AR & MA models require stationary time series, otherwise they overestimate R-squared 
# The time-series is stationary if these conditions are met:
# It has a constant mean.
# It has a constant standard deviation.
# There is no seasonality 
# We can check if it's stationary with an ADF or KPSS test
adf.test(in_sample_data)  # p = 0.9, non-stationary
kpss.test(in_sample_data)  # p-value smaller than printed value, non-stationary

# The data are non-stationary, with seasonality, so we will take the seasonal 
# difference 
in_sample_data %>%
  diff(lag = 4) %>%  # diff at lag 4
  tsdisplay()
adf.test(diff(in_sample_data, 4))  # p = 0.01877, stationary
kpss.test(diff(in_sample_data, 4))  # p-value smaller than printed value, non-stationary

# Still appears non-stationary, so we take an additional first difference
in_sample_data %>%
  diff(lag = 4) %>% #  seasonal diff
  diff() %>% # first diff 
  tsdisplay()
differenced <- diff(diff(in_sample_data, 4), 1)
adf.test(differenced)  # p-value smaller than printed value, so stationary
kpss.test(differenced)  # p-value greater than printed value, so stationary

# Plot of time-series data before and after differencing
diff_vs_no_diff <- cbind("Original Data" = in_sample_data,
              "Seasonal Differencing" = diff(in_sample_data, 4),
              "First Difference" = diff(in_sample_data, 1),
              "Seasonal and First" = differenced)
autoplot(diff_vs_no_diff, facets=TRUE) +
  xlab("Time") +
  ggtitle("Employment")

# Checking for constant variance with the ARCH test
archTest(differenced) # non-significant, therefore constant variance

# Autocorrelation is the correlation between a time series and a delayed 
# version of itself, while the ACF plots the correlation coefficient against the 
# lag, and it’s a visual representation of autocorrelation.
# Partial autocorrelation captures the correlation between two variables after 
# controlling for the effects of other variables. 

# ACF and PACF plots for first and quarterly differenced data
tsdisplay(differenced)
# ACF: significant spikes at lags 3, 4, 5, 9, 13 (complicated)
# PACF: significant spikes at lags 3, 4, 8, 9, 12 (seasonal 4, 8, 12)
# PACF has a faster drop off than ACF which appears more geometric = AR(p) 
# so AR(p) model may fit better than MA(q)
# First guesses ARIMA(pdq)(PDQ)m:
# d = 1, D = 1 due to differencing
# ARIMA(0,1,0)(0,1,0)4 only focusing on the differencing
# ARIMA(1,1,0)(1,1,0)4 due to seasonal component at lag 4 in the PACF
# ARIMA(0,1,1)(0,1,1)4 due to seasonal component at lag 4 in the ACF

# Finding an appropriate ARIMA model
# Train and test samples from in_sample_data
train_ARIMA <- head(in_sample_data, length(in_sample_data) - length(out_sample_data))
test_ARIMA <- tail(in_sample_data, length(out_sample_data))

# auto.arima() came up with the best model (for AICc), ARIMA(0,1,0)(0,1,1)[4] 
arima010010 <- Arima(train_ARIMA, order = c(0,1,0), seasonal = c(0,1,0))
  summary(arima010010)  # AICc = 606.79
arima110110 <- Arima(train_ARIMA, order = c(1,1,0), seasonal = c(1,1,0))
  summary(arima110110)  # AICc = 599.75 
arima011011 <- Arima(train_ARIMA, order = c(0,1,1), seasonal = c(0,1,1))
  summary(arima011011)  # AICc = 592.01 
auto <- auto.arima(train_ARIMA, stepwise = FALSE)
  auto  # AICc = 589.75

# Check the residuals from your chosen model by plotting the ACF of the 
# residuals, and doing a portmanteau test of the residuals. If they do not look
# like white noise, try a modified model.
# Once the residuals look like white noise, calculate forecasts.
checkresiduals(arima010010)  # small p-value, not white-noise, so remove
checkresiduals(arima110110)  # p = 0.1588, white noise
checkresiduals(arima011011)  # p = 0.3354, white noise
checkresiduals(auto)  # p = 0.4288, white noise [best]

# Testing the models against in-sample validation data (test_ARIMA)
# ARIMA(1,1,0)(1,1,0)[4] had the lowest mean absolute scaled error (MASE)
accuracy(forecast(arima110110, h = 8), test_ARIMA)  # MASE = 0.4772776 [best]
accuracy(forecast(arima011011, h = 8), test_ARIMA)  # MASE = 2.0404656
accuracy(forecast(auto, h = 8), test_ARIMA)  # MASE = 2.0477586

# Testing the models against future data (out_sample_data)
best_mase <- Arima(in_sample_data, order = c(1,1,0), seasonal = c(1,1,0))
third_arima <- Arima(in_sample_data, order = c(0,1,1), seasonal = c(0,1,1))
best_aicc <- Arima(in_sample_data, order = c(0,1,0), seasonal = c(0,1,1))
accuracy(forecast(best_mase, h = 8), out_sample_data)  # MASE = 2.6744393
accuracy(forecast(third_arima, h = 8), out_sample_data)  # MASE = 2.1224180 ['best']
accuracy(forecast(best_aicc, h = 8), out_sample_data)  # MASE = 2.1482249

# Plotting the models
plot(forecast(best_mase, h = 8, level = c(0.8, 0.95)))
lines(out_sample_data, lty = 2, lwd = 2)
legend("topright", c("Historical data", "Actual future data", "Forecast data"),
       col = c("black", "black", "#31A9F6"),
       lwd = c(1, 2, 2), lty = c(1, 2, 1))

plot(forecast(third_arima, h = 8, level = c(0.8, 0.95)))
lines(out_sample_data, lty = 2, lwd = 2)
legend("topright", c("Historical data", "Actual future data", "Forecast data"),
       col = c("black", "black", "#31A9F6"),
       lwd = c(1, 2, 2), lty = c(1, 2, 1))

plot(forecast(best_aicc, h = 8, level = c(0.8, 0.95)))
lines(out_sample_data, lty = 2, lwd = 2)
legend("topright", c("Historical data", "Actual future data", "Forecast data"),
       col = c("black", "black", "#31A9F6"),
       lwd = c(1, 2, 2), lty = c(1, 2, 1))

# All perform quite badly at forecasting, due to the upswing in 'future' data
# However, ARIMA(0,1,0)(0,1,1)[4] (auto.arima) had the best AICc, therefore 
# I would select this as the 'best' ARIMA model for the data.
