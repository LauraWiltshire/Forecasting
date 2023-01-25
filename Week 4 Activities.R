library(Mcomp)
library(fpp2)
library(tidyverse)

# Exercise 1: Using the time series with ID 1896 from the M3 competition 
#(package Mcomp): Perform additive and multiplicative classical decomposition 
# and discuss on the results.
# Draw a polar seasonal plot. 
ex1_data <- M3[[1896]]$x
additive <- decompose(ex1_data)
plot(additive)
multiplicative <- decompose(ex1_data, type = c("multiplicative"))
plot(multiplicative)
# multiplicative better due to smaller range of random component
ggseasonplot(ex1_data, polar = T)

# Ex 2: Using the in-sample observations of the series with ID 1402 from the M3 
# competition (package Mcomp): Produce forecasts with the naive method and 
# measure the out-of-sample accuracy based on MASE.
# Repeat for Simple Exponential Smoothing (SES).
# Compare the accuracy of the two models. 
ex2_data <- M3[[1402]]$x
length(M3[[1402]]$xx) # length = 18
naive <- ex2_data %>%
  naive(h = 18) %>%
  forecast()
accuracy(naive, M3[[1402]]$xx)[2,6] # o-o-s MASE = 0.461
SES <- ses(ex2_data, h = 18)
accuracy(SES, M3[[1402]]$xx)[2,6] # o-o-s MASE = 0.793
# MASE for SES > naive, so naive is more accurate

# Ex 3 Repeat the above exercise for 20 series, with IDs 1402 to 1421, saving the 
# MASE per series and per method in an appropriate object. Summarise the 
# across-series performance.

MASEs <- array(NA, c(2, 20))
for (i in 1:20){
  ts <- M3[[1401+i]]$x
  out_sample_ts <- M3[[1401+i]]$xx
  NAIVE <- naive(ts, h=18)
  SES <- ses(ts, h=18)
  MASEs[1,i] <- accuracy(NAIVE, out_sample_ts)[2,6]
  MASEs[2,i] <- accuracy(SES, out_sample_ts)[2,6]
}
rowMeans(MASEs)

custom_forecasts <- function() {
  m <- 8
  t <- m %/% 2
  MA2 <- array(NA, length(y))
  #for each number in an array of length y, except half the moving average length either side
  #moving average 2 for i is the mean of the two vectors of 
  # for example array y's i1,i2,i3,... and array y's i2, i3, i4,...
  
  

