# 3.4

SALES <- read.table("SALES.TXT",header=T)
Sales <- ts(SALES$Sales,frequency=12, start=1970)
# This plot shows the monthly sale volume
plot(Sales)

# Apply log transformation to remove the increasing variance
log_transform <- log(Sales)
plot(log_transform, ylab='Log Transformation')

# First differencing removed the linear trend of the time-serie model
sale_diff <- diff(log_transform, lag=12)
ts.plot(sale_diff, ylab="First Differencing")

# Second differencing removed the seasonality, so the model looks better and
# really close to the stationary model
second_diff <- diff(sale_diff, lag=2)
plot(second_diff, ylab="Second differencing")

par(mfrow=c(1,2))
acf(Sales, lag.max = 50)
acf(second_diff, main='Final')

pacf(Sales, lag.max = 50)
pacf(second_diff, main='Final')



