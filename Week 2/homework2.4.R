# 2.4

crop_data <- read.table("crop.txt", header=T, sep='\t')

set.seed(123)

# a
crop_lm <- lm(Yield~Fertilizer, data=crop_data)
summary(crop_lm)

FIT <- fitted(crop_lm)
RES <- resid(crop_lm)

plot(crop_data$Fertilizer,crop_data$Yield, ylab="Yield",xlab="Fertilizer",pch=16)
abline(crop_lm)

plot(crop_data$Yield,FIT, ylab="Fits",xlab="Yield",pch=16)

plot(FIT,RES, xlab="Fits",ylab="Residuals",pch=16)
abline(a=0,b=0,col="grey",lwd=1)

# b
set.seed(123)
crop_lm2 <- lm(Yield~Fertilizer+LSqrd, data=crop_data)
summary(crop_lm2)

x <- crop_data$Fertilizer # x^2 = LSqrd
beta_1 <- 31.0812
beta_2 <- -2.4611
const_term <- 193.3100

plot(crop_data$Fertilizer, crop_data$Yield, xlab="Fertilizer + LSqrd",ylab="Yield")
curve((const_term + beta_1*x + beta_2*x^2), from=0, to=10, ylab='Yield', xlab="Fertilizer + LSqrd", add=TRUE)




