#library("knitr")
#knit2html("file")
# 1.2 Homework

tobacco = read.table("tobacco.txt", header=T, sep = "\t")
tobacco.matrix <- as.matrix(tobacco)
# colnames(tobacco)
set.seed(123)

# (a)
# Linear regression model
# constant term  = Intercept
fit1 <- lm(ILL~CONSUMPTION, data=tobacco)
summary(fit1)


# (b)
consumption <- as.matrix(tobacco$CONSUMPTION)

n <- nrow(tobacco) # get number of rows
Intercept <- rep(1,n) 
X = cbind(Intercept, consumption) # add constant term to the matrix

# estimate the regression coefficient with least squares method
#and give interpretations for it
b_coeffiecient <- solve(t(X) %*% X) %*% t(X)%*%(tobacco$ILL)
b_coeffiecient

# (c)

# The coefficient of determination of the model is 54.9%, which corresponds to
# "multiple R-squared" in the output
summary(fit1)$r.squared


# (d)
#F-stat: 10.96 on 1 and 9 DF, p-value: 0.009081
#p_value <- summary(fit1)$coefficients[2,4]
#return True, so we can reject the null hypothesis
#this model is statistically significant according to the F-test

alpha = 0.01 # level of significance
p_value <- pf(summary(fit1)$fstatistic[1],
              summary(fit1)$fstatistic[2],
              summary(fit1)$fstatistic[3],
              lower.tail=FALSE)
p_value < alpha

# (e)
# the variable CONSUMPTION is statistically significance as its p-value is lesser
# than the significance level 
p_value2 <- summary(fit1)$coefficients[,4][2]
p_value2
p_value2 < alpha


# (f)
plot(tobacco[,5], tobacco[,14], xlab = "CONSUMPTION", ylab = "ILL")
abline(fit1)

# (h) 95%
confint(fit1,level=0.95)

# (h) 99%
confint(fit1,level=0.99)

# (i)

k <- 2000
bootmat <- matrix(NA, nrow=k,ncol=2)
y <- tobacco$ILL

set.seed(123)
for(i in 1:(k-1)){
  ind <- sample(1:n,replace = TRUE)
  Xtmp <- X[ind,]
  ytmp <- y[ind]
  btmp <- solve(t(Xtmp)%*%Xtmp)%*%t(Xtmp)%*%ytmp
  bootmat[i,] <- t(btmp)
}

bootmat[k,] <- t(b_coeffiecient)

q_consumption <- quantile(bootmat[,2], probs = c(0.025,0.975))
q_consumption





