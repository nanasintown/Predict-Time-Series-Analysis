setwd('~/Prediction/Week_3')

INTEL <- read.table("INTEL.txt",header=T)
SUNSPOT <- read.table("SUNSPOT.txt",header=T,row.names=1)
MLCO2 <- read.table("MLCO2.txt",header=T,row.names=1)
SALES <- read.table("SALES.txt",header=T)
PASSENGERS <- read.table("PASSENGERS.txt",header=T,row.names=4)

?ts
Intel_Close <- ts(INTEL$Intel_Close)
Intel_Volume <- ts(INTEL$Intel_Volume)
Spots <- ts(SUNSPOT,start=1749)
Mlco2 <- ts(MLCO2$MLCO2,frequency=12)
Sales <- ts(SALES$Sales,frequency=12)
Passengers <- ts(PASSENGERS$Passengers)

spectrum_f <- function(ts) {
  spec_data <- spectrum(ts, plot=F)
  plot(spec_data$freq, spec_data$spec)
}

# A quick reminder on stationary processes x_t:
# (1) E[x_t] = mu for all t
# (2) Var[x_t] = sigma^2 < inf for all t
# (3) Cov[x_t, x_(t-k)] = gamma_k for all t,k

# 3.1

# Intel_Close: The price of the Intel stock at NYSE at market close
# over a period of 20 trading days.
plot(Intel_Close)

# There does not seem to be any systematic trends (growth or decay over the time period)
# Hard to say whether the level changes systematically compared to natural variation
# Of course, supply and demand influences the level so in a sense the process is not stationary.
# A short snapshot might be approximately stationary but the estimates extracted 
# are unlikely to be meaningful.
# No seasonality observed.
spectrum_f(Intel_Close)

# Intel_Volume: The amount of shares traded at NYSE during a trading day
# over a period of 20 trading days.
plot(Intel_Volume)

# No systematic trends
# The level seems to vary quite drastically with sudden shocks in demand/supply.
# No seasonality
spectrum_f(Intel_Volume)
# Since the level varies (again supply and demand influence the volume),
# the process is likely to be non-stationary.

# Spots: The number of sun spots annually.
plot(Spots)

# There is no clear trend but the level seems to vary periodically with a period of
# 11 years. Additionally, the amplitude of the seasonal component seems to vary
# x(t) = a(t)sin(wt) + e
# A clear seasonal component implies non-stationarity since the level is not 
# invariant with respect to time.
spectrum_f(Spots) # A peak is observed a bit below 0.1

# Mlco2: Carbon dioxide measurements of the Mauna Loa volcano
# Note that the natural time unit is chosen to be years
plot(Mlco2)

# We observe a clear trend of approx. linearly increasing measurements.
# Clear seasonality with a period length of approximately 12 months.
# The amplitude of th seasonal component is quite stable.
# With these observations in mind, the process is clearly non-stationary.
spectrum_f(Mlco2) # A peak at 1

# Sales: Monthly sales volume (The homework data set)
plot(Sales)

# A clear upward trend. Likely to be linear.
# A clear seasonal component with a period length of 12 months.
spectrum_f(Sales) # Observe the harmonic components
# Amplitude of the seasonal component seems to be increasing.
# Assuming that the customer base in increasing linearly, the increasing
# amplitude makes sense since there will be more variability in the demand.
# Clearly a non-stationary process.

# Passengers: Monthly airline passengers on international routes in the USA
plot(Passengers)

# A clear upward trend. Non-linear but likely sub-exponential.
# Clear seasonality of a period length of 12 months.
# The amplitude is increasing, similar logic to sales.
spectrum_f(Passengers) # A peak bit below 0.1
# The process is not stationary.

# 3.2

PASS2 <- ts(PASSENGERS$Passengers,start=1949,frequency=12)

par(mfrow=c(1,2))
plot(PASS2, main = 'Original')
plot(log(PASS2), main = 'Log transformed')
# We can observe that the log transformation stabilizes the increasing
# amplitude of the original time series. This works well when Var[x_t] = s^2E[x_t]^2
# i.e. the variance of x_t scales quadratically with respect to its expected value.
# However, the curvature of the trend has flipped, likely due to the relatively
# fact that the trend is sub-exponential.

# 3.3
par(mfrow=c(1,2))
?acf
?pacf

acf(Intel_Close)
pacf(Intel_Close)

# The blue dotted lines give the critical value for a significance test
# which tests: H_0: gamma_k = 0, H_1: gamma_k != 0. Values below significance
# threshold can be treated as zeros.

# Some observations: The autocorrelation decays quite rapidly (exponentially),
# which is characteristic for processes having a stationary AR part.
# The partial autocorrelation also vanishes after 2 lags which further strengthens
# the case for an AR model (more about this in week 4).
# The well-behaved correlation plots indicate that the process could be stationary.

acf(Spots, lag.max = 50)
pacf(Spots, lag.max = 50)
# The autocorrelation decays quite slowly. Additionally, it peaks around the
# period length of 12. Do note that seasonal AR can be stationary. The issue
# here is the slow rate of decay in the autocorrelation.
# Partial autocorrelation seems fine in comparison.

#HW
?diff
# Argument lag defines the order of the lag i.e. 1 - L^(lag)
# You can use this to perform linear differencing and seasonal differencing.

# differences refers to the order of the difference i.e. (1 - L^(lag))^(differences)
# This is not necessary for this exercise.

# Make sure that the order of your transformations is reasonable so that everything
# is well-defined.