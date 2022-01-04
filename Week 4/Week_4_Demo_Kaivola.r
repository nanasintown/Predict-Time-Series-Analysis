setwd('~/Prediction/Week_4')

library(forecast)

INTEL <- read.table("INTEL.txt",header=T)
SUNSPOT <- read.table("SUNSPOT.txt",header=T,row.names=1)
SALES <- read.table("SALES.txt",header=T)

Intel_Close <- ts(INTEL$Intel_Close)
Spots <- ts(SUNSPOT,start=1749)
Sales <- ts(SALES$Sales,frequency=12,start = 1970)

ts_plotter <- function(ts, lag_max) {
  par(mfrow = c(2,2))
  plot(ts, main = 'Run Plot')
  acf(ts, main = 'Autocorrelation', lag.max = lag_max)
  pacf(ts, main = 'Partial Autocorrelation', lag.max = lag_max)
  spec <- spectrum(ts, plot = F)
  plot(spec$freq, spec$spec, main = 'Spectrum' ,
       xlab = 'Frequency', ylab = 'Energy density')
}

# Box-Jenkins method: A three-stage approach to SARIMA(p,h,q)(P,H,Q)_s model selection

# (1) Model Identification: Determine the orders of the model
# (1a) Determine the order of differencing h and the order of seasonal differencing H
# (and season length s) needed to stationarize the time series (SARIMA -> SARMA)
# You might also need to consider other transformations as well such as log, sqrt etc.
# depending on the behavior of the trend and the variance.
# (1b) Identify the orders p,q,P,Q

# (2) Model Estimation: Estimate the parameters of the SARMA model
# We do this by using the maximum likelihood estimating equations of the normal model
# even if the residuals are not assumed to be normally distributed.

# (3) Model Diagnostics: Determine whether the estimated model is sufficient
# For SARMA processes, the residual process should be a white noise (WN) process
# WN Process e_t
# (a) E[e_t] = 0 for all t
# (b) Var[e_t] = sigma for all t
# (c) Cov[e_t,e_j] = 0 for all t != j
# These can be analyzed by examining the estimated residuals the same way we examined the original time series.
# We can also perform a significance test on the autocorrelations of the residual process, more about this later.

# If there is substantial autocorrelation present in the residual process, some part of the model is
# misspecified and we need to return to step (1) and identify a better candidate model.
# The model is deemed sufficient if autocorrelation is not present in the estimated residual process.

# 4.1
ts_plotter(Intel_Close, lag_max = 19)

# The autocorrelation function seems to decay exponentially
# The PACF vanishes after two lags.
# No seasonality observed in the run plot or the spectrum.
# These observations indicate an autoregressive process of order 2 (AR(2) process)

?arima

intel_ar_2 <- arima(Intel_Close, order = c(2,0,0))
summary(intel_ar_2)

# For diagnostics, we analyze the residual process.
ts_plotter(intel_ar_2$residuals, lag_max = 19)
# The autocorrelations are zero according to the individual tests (H_0: rho_k = 0, H_1: rho_k != 0) depicted by the blue lines
# No periodicity present

# We can also use an aggregated test for testing the autocorrelations, the Ljung-Box test
# The hypothesis is H_0: e_t ~ WN, H_1: e_t !~ WN where the test statistic aggregates multiple sample autocorrelations.
# The idea is that pooling the statistics can (1) give us a test with a higher power (Probability of observing the
# effect if one exists) and (2) control the Type 1 error (Probability of rejecting a true null hypothesis) when compared
# to testing all the autocorrelations individually which inflates the Type 1 error rate. Also, we are not really interested in 
# the individual autocorrelations, only whether there exists one which is non-zero.

?Box.test

ljung_box_tester <- function(res, n_param, max_lag) {
  lags <- seq(n_param + 1, max_lag)
  sapply(lags, function(lag) Box.test(res, lag = lag, type = 'Ljung', fitdf = n_param)$p.value)
}

ljung_box_tester(intel_ar_2$residuals, 2, 19)

# The null hypothesis is not rejected for any lag, thus it is reasonable to believe that the residual
# process is white noise.

fit <- fitted(intel_ar_2)
plot(fit,type="b", col="blue", ylim=c(60,68),
     ylab="Price", xlab="Time")
lines(Intel_Close, col="red", type="b")
legend(16, 68, legend=c("Observed", "Fitted"),
       col=c("red","blue"),lty=c(1,1),cex=0.8)

# The fit does seem quite reasonable. To analyze the forecasting capacity of the model, we use the first
# 16 observations to fit the model and use the forecast package

intel_ar_2_pred <- arima(Intel_Close[1:16], order = c(2,0,0))
?forecast # We consider how predictions can be constructed later in this course
intel_price_pred <- forecast(intel_ar_2_pred, h = 15, level = F)$mean

plot(Intel_Close, col="red", type="b", ylim=c(60,68), xlim=c(0,30),
     ylab="Price", xlab="Time")
lines(intel_price_pred, col="blue", type="b")
legend(16, 68, legend=c("Observed", "Predicted"),
       col=c("red","blue"),lty=c(1,1),cex=0.8)

# While the previous diagnostics implied a sufficient fit, the predictions start to deviate rapidly
# from the observed values. Since the time series is relatively short, even a simple model can
# start to deviate substantially if we predict too far into the future if we overfit.
# Later in this course, we consider certain
# fundamental issues with ARMA models and long term predictions.

ts_plotter(Spots, lag_max = 50)
# The autocorrelation seems to decay relatively slowly and clearly oscillates due to the seasonal component
# in the time series. The spectrum also identifies this seasonal component.
# The PACF seems to mostly vanish after lag 2 so it is at least in some sense similar to the Intel_Close data.

# For the sake of illustration, let's examine the AR(2) model here.

spots_ar_2 <- arima(Spots, order = c(2,0,0))
summary(spots_ar_2)

ts_plotter(spots_ar_2$residuals, lag_max = 50)
# The correlations corresponding to lags that are close to the period length seem to be statistically significant.

ljung_box_tester(spots_ar_2$residuals, 2, max_lag = 50) < 0.05
# The lags between 10 and 18 are statistically significant at the 5 % level.
# Thus AR(2) is not satisfactory.

# To find a suitable model, we experiment with an automatic model search method
?auto.arima

spots_auto <- auto.arima(Spots) # stepwise = F, approximation = F
summary(spots_auto) # A rather complicated model emerges, whereas the AR(2) actually has a slightly better AIC value
# Always consider whether the criterion being optimized serves your goals, optimizing AIC might introduce
# undesirable uncertainty in the model estimates.

ts_plotter(spots_auto$residuals, lag_max = 50)
# Relatively small changes compared to AR(2)
ljung_box_tester(spots_auto$residuals, 4, max_lag = 50) < 0.05
# Similar conclusions as previously for AR(2)

# See the official model solution for comments 

ts_plotter(diff(Spots ,lag = 12), lag_max = 50) # The period length seems to be unstable

# Let's observe the fit

fit.sun <- fitted(spots_ar_2)
fit.auto <- fitted(spots_auto)
plot(fit.sun, type="b", col="blue",
     ylab="Spots", xlab="Time")
lines(Spots, col="red", type="b")
lines(fit.auto, col="green", type="b")
legend("topleft", legend=c("Spots time series", "AR(2)-fit",
                           "ARMA(3,1)-fit"),
       col=c("red","blue","green"),lty=c(1,1),cex=0.8)

# The models are relatively reasonable though their predictions are biased upwards for certain valleys.
# These valleys seem to sustain a lower level and not immediately recover as compared to the fits.

# Let's try forecasting with the AR(2)
spots_ar_2_pred <- arima(Spots[1:172], order = c(2,0,0))

spots_pred <- ts(forecast(spots_ar_2_pred, h=43)$mean ,start=1921)
plot(Spots, col="red",type="l", ylim=c(0,200))
lines(spots_pred, col="blue", type="l")

# The predictions a few steps ahead are relatively reasonable but the long terms predictions converge
# to a fixed value corresponding to the intercept. This is a general limitation of the ARMA family of models.
# This problem is likely exacerbated by the lack of fit observed in the diagnostics.


ts_plotter(Sales, lag_max = 50)
# ACF decays rather slowly (the upward trend) and peaks at the period length of 1 year.
# According to the previous homework, we could try differencing to eliminate the trend and seasonality.
# (And log-transformation can also be used)

# Let's see what auto.arima has to say

sales_auto <- auto.arima(Sales) # stepwise = F, approximation = F
summary(sales_auto) # The order of the differences definitely makes sense
ts_plotter(diff(diff(Sales), lag = 12), lag_max = 50)
# The AR component sort of makes sense due to PACF vanishing after lag 2 but the MA part is rather strange.
# Perhaps the periodic component that remains after differencing is not that stable.

ts_plotter(sales_auto$residuals, lag_max = 50)
# There are some peaks in the correlation plots, especially around the period length and its multiples.
# Maybe some periodicity remains in the residual process?

ljung_box_tester(sales_auto$residuals, n_param = 3, max_lag = 50) < 0.05
# Clearly some of the autocorrelations are statistically significant
# Thus the model is not satisfactory from this perspective.

fit_sales <- fitted(sales_auto)
plot(fit_sales, type="b", col="blue",
     ylab="Spots", xlab="Time")
lines(Sales, col="red", type="b")
legend("topleft", legend=c("Sales time series", "Fitted"),
       col=c("red","blue"),lty=c(1,1),cex=0.8)

# The fit is relatively reasonable though the exact fit at the beginning is quite interesting.
# Clearly at least some part of the model is on the right track.

prediction_sales <- forecast(sales_auto,h=48)$mean
plot(Sales,col="red",type="b",ylim=c(100,340),
     xlim=c(1970,1987),ylab="Sales",xlab="Time")
lines(prediction_sales,col="blue",type="b")

# Predictions are quite reasonable as well.
# Thus even if the Ljung-Box test rejected the null, this model could be useful.

# HW
# a) Consider the differencing (both standard and seasonal) needed to obtain a stationary time series. 
# Any slow decay in the autocorrelation functions indicate that the series is not likely to be stationary.
# After obtaining a relatively stationary time series, consider the properties of SARMA models and select
# some reasonable model.
# Use Ljung-Box test to assess the sufficiency of the model.
# While you can use auto.arima, the results might not be as good as one can obtain by analyzing
# the correlation plots of the stationarized time series.

# b) Use forecast as in the demo exercises.

