library(car)
library(forecast)
library(lmtest)

# 5.4 a, Model 1
data <- read.table("t38.txt",header=T)
CONS <- ts(data$CONS)
INC <- ts(data$INC)
INFLAT <- ts(data$INFLAT)

model1 <-lm(CONS~INC+INFLAT)
summary(model1)

# the residuals are almost normally distributed
par(mfrow=c(3,2))
hist(model1$residuals,xlab="Residuals",ylab="Frequency",main=" ")

acf(model1$residuals,main="")
plot(model1$residuals,type="p",ylab="Residuals",xlab="Time",pch=16,xaxt="n")
abline(0,0)

qqnorm(model1$residuals, pch=16)
qqline(model1$residuals,col="red",lwd=2)

plot(model1$fitted.values, ylab="Model 1 Fitted")

plot(cooks.distance(model1),ylab="Cook's distances",xlab="Time",pch=16,xaxt="n")
axis(1)

dev.off()
plot(CONS,col="red",xlab="Time",ylab="")
fit <- ts(predict(model1))
lines(fit,col="blue")
legend("topright", legend=c("CONS", "fit"),
       col=c("red","blue"),lty=c(1,1),cex=1)

vif(model1)

# b Model 2
DCONS <- diff(CONS)
DINC <- diff(INC)
DINFLAT <- diff(INFLAT)

model2 <- lm(DCONS~DINC+DINFLAT)
summary(model2)

vif(model2)
par(mfrow=c(3,2))
hist(model2$residuals,xlab="Residuals",ylab="Frequency",main=" ")
# the residuals are normally distributed
acf(model2$residuals,main="")
plot(model2$residuals,type="p",ylab="Residuals",xlab="Time",pch=16,xaxt="n")
abline(0,0)

qqnorm(model2$residuals, pch=16)
qqline(model2$residuals,col="red",lwd=2)

plot(model2$fitted.values, ylab="Model 2 Fitted",xlab="Index",pch=16,xaxt="n")
abline(0,0)

plot(cooks.distance(model2),ylab="Cook's distances",xlab="Time",pch=16,xaxt="n")
axis(1)

dev.off()
plot(DCONS,col="red",xlab="Time",ylab="")
fit2 <- ts(predict(model2))
lines(fit2, col="blue")
legend("topright", legend=c("DCONS", "fit2"),
       col=c("red","blue"),lty=c(1,1),cex=0.5)

model2_bg <- rep(NA,155)
# Breusch-Godfrey can be performed up to order:
# (sample size) - (number of estimated parameters) = 158-3 = 155
for (i in 1:155)
{
  model2_bg[i]= bgtest(model2, order=i)$p.value
}
model2_bg
which(model2_bg > 0.05)


# c Model 3

n <- nrow(data)
model3 <- lm(CONS[-1]~CONS[-n]+INC[-1]+INC[-n]+INFLAT[-1]+INFLAT[-n])
summary(model3)
# ,breaks=seq(from=-0.1,to=0.1,by=0.02
par(mfrow=c(3,2))
hist(model3$residuals,xlab="Residuals",ylab="Frequency",main=" ")

acf(model3$residuals,main="")

qqnorm(model3$residuals,pch=16)
qqline(model3$residuals)

plot(model3$residuals,type="p",ylab="Residuals",xlab="Time",pch=16,xaxt="n")
abline(0,0)

plot(model3$fitted.values, model3$residuals,type="p",ylab="Residuals", xlab = "Fitted values", pch=16)
abline(0,0)
plot(cooks.distance(model3),ylab="Cook's distances",xlab="Index",pch=16,xaxt="n")
axis(1)

dev.off()
plot(ts(CONS),col="red",xlab="Time",ylab="",type="l")
fit3 <- ts(predict(model3))
lines(fit3,col="blue")
legend("topright", legend=c("CONS", "fit"),
       col=c("red","blue"),lty=c(1,1),cex=0.8)#dev.off()


# ,labels=seq(from=1950,to=1981,by=3)

model3_bg <- rep(NA,152)

# Breusch-Godfrey can be performed up to order: 
# (sample size) - (number of estimated parameters) = 158 - 6 = 152

for (i in 1:152)
{
  model3_bg[i]= bgtest(model3, order=i)$p.value
}
model3_bg
which(model3_bg < 0.05)
# Null hypothesis of no autocorrelation accepted with all lags

vif(model3)













