# 4.3 a
library(forecast)

MLCO2 <- read.table("MLCO2.txt", header=T)
Mlco2 <- ts(MLCO2$MLCO2)
plot(Mlco2)

par(mfrow=c(1,2),mar=c(2.5,2.5,3.5,1.5))
acf(Mlco2,main="ACF", lag.max=50)   # No exponentially decay
pacf(Mlco2,main="PACF", lag.max=50) #cuts off after lag 2.

# remove linear upward trend and seasonality
seasonal = list(order = c(0,1,2), period=12)
test.model <- arima(Mlco2, order=c(2,1,1),seasonal)
test.model

#par(mfrow=c(1,2),mar=c(2.5,2.5,3.5,1.5))
acf(test.model$res,main="ACF", lag.max=50)   
pacf(test.model$res,main="PACF", lag.max=50)

mlco2_bl <- rep(NA,45)
k <- 5
for(i in 1:45){
  mlco2_bl[i] <- Box.test(test.model$res,lag=(i+k),fitdf=k,
                          type="Ljung-Box")$p.value
}
mlco2_bl
which(mlco2_bl <= 0.05) + k  

dev.off()
fit.model <- fitted(test.model)
plot(fit.model, type="b",col="green",xlab="Time", ylab='MLCO2')
lines(Mlco2,type='b',col='red')
legend(2,32, legend=c("time series", "fit"), col=c("red","green"),lty=c(1,1),cex=0.8)

model_plot1 <- arima(Mlco2[1:214], order=c(2,1,1),seasonal)
prediction <- forecast(model_plot1,h=2, level=FALSE)$mean
plot(Mlco2, col='red', type='b', ylim=c(10,35), main='Prediction with 2 time-step')
lines(prediction,col='blue',type='b')
legend(2,32, legend=c("time series", "prediction"), col=c('red','blue'),lty=c(1,1), cex=0.8)

model_plot2 <- arima(Mlco2[1:192], order=c(2,1,1),seasonal)
prediction <- forecast(model_plot2,h=24, level=FALSE)$mean
plot(Mlco2, col='red', type='b', ylim=c(10,35), main='Prediction with 24 time-step')
lines(prediction,col='green',type='b')
legend(2,32, legend=c("time series", "prediction"), col=c('red','green'),lty=c(1,1), cex=0.8)

# -------------------

new_mlco2 <- diff(diff(Mlco2),lag=12)
auto.model <- auto.arima(new_mlco2)
auto.model
par(mfrow=c(1,2),mar=c(2.5,2.5,3.5,1.5))
acf(auto.model$res,main="ACF", lag.max=50)   
pacf(auto.model$res,main="PACF", lag.max=50)


mlco2_test <- rep(NA,48)
k <- 2
for(i in 1:48){
  mlco2_test[i] <- Box.test(auto.model$res,lag=(i+k),fitdf=k,
                          type="Ljung-Box")$p.value
}
round(mlco2_test,3)
which(mlco2_test > 0.05) + k  

# -------------------


