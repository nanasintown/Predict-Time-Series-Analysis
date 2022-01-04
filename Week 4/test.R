library(forecast)

MLCO2 <- read.table("MLCO2.txt", header=T)
Mlco2 <- ts(MLCO2$MLCO2)
plot(Mlco2)

seasonal = list(order = c(0,1,2), period=12)
test.model <- arima(Mlco2, order=c(2,1,1),seasonal)

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