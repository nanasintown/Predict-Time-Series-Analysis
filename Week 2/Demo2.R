

# 2.1
# (a)
# The data is separated with tabulator (you can verify this e.g. with Notepad)

smoking <- read.table("tobacco.txt",header=T,sep="\t")
model <- lm(ILL~CONSUMPTION,data=smoking)
countries <- c("Iceland","Norway","Sweden","Canada","Denmark",
               "Austria","USA","Netherlands","Switzerland","Finland","England")



# pch sets the marker
plot(smoking$CONSUMPTION,smoking$ILL, ylab="cases in 1950", 
     xlab="CONSUMPTION in 1930", pch=16,
     main="CONSUMPTION/ILL per 100 000 individuals")

abline(model,col="red")

# The arguments below: x-cord, y-cordd, labels = the shown labels,
# cex = size of the labels, pos=position (1,2,3 or 4)
# if the labels are unclear, sunbstitute with labels=1:11

text(smoking$CONSUMPTION, smoking$ILL, labels=countries, cex= 0.8,pos=3)



# pch sets the marker
plot(smoking$CONSUMPTION,smoking$ILL, ylab="cases in 1950",
     xlab="CONSUMPTION in 1930", pch=16,
     main="CONSUMPTION/ILL per 100 000 individuals")

abline(model,col="red", lwd=0.8)
# identify provides a convenient way to identify data points by clicking.
# Press esc when you are done
identify(smoking$CONSUMPTION,smoking$ILL,labels=countries)

# (b)

FIT <- model$fit
RES <- model$res

# (c)

plot(smoking$ILL,FIT, ylab="Fits",xlab="Sick",pch=16)
# A way to show only the label of USA (observation number 7)
text(smoking$ILL,FIT,  labels = ifelse(rownames(smoking)=="7", "USA", NA),pos=2)


plot(FIT,RES, xlab="Fits",ylab="Residuals",pch=16)
abline(a=0,b=0,col="grey",lwd=1)

text(FIT,RES,  labels = ifelse(rownames(smoking)=="7", countries, NA),pos=3)


# (d) The outlierness of USA is visible especially in (FIT,RES) plot 

# (e)

cooksd <- cooks.distance(model)
# parameter xaxt ="n" leaves x-axis empty

x <-plot(cooksd,xaxt="n",xlab=" ",ylab="Cook's distances")

# parameter side = 1 refers to the x-axis,
# parameter at gives the points on the x-axis
# las=2 rotates the text 90 degrees
axis(side=1,at=1:11, labels=countries,las=2)



# (f)

# Remove the line 7, i.e. USA
smoking2 <- smoking[-7,]
model2 <- lm(ILL~CONSUMPTION,data=smoking2)
summary(model2)
# Compare to Homework 1.


# 2.2
# a
# run if you have not installed car before
# install.packages("car")

library(car)
hald <- read.table("hald.txt",header=T,sep="\t")

fullmodel=lm(HEAT~CHEM1+CHEM2+CHEM3+CHEM4,data=hald)
summary(fullmodel)

vif(fullmodel)


model2 <- lm(CHEM2 ~ CHEM1+CHEM3+CHEM4,data=hald)
summary(model2)

1/(1-summary(model2)$r.squared)

model3 <- lm(CHEM4 ~ CHEM1 + CHEM2 + CHEM3, data=hald)
summary(model3)

1/(1-summary(model3)$r.squared)

# b
# Note that, we want to minimize the value in the AIC column
step(fullmodel)

model4 <- lm(HEAT ~ CHEM1  + CHEM2 +CHEM4 , data=hald)
summary(model4)

b = seq(from=-4, to=4, length.out=9)

hist(fullmodel$residuals,col="grey",xlab=" ",main="Residuals of the full model",breaks=b)
#

















