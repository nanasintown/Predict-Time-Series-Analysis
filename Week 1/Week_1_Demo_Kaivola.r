setwd('~/Prediction/Week_1')

# Basic Syntax and Commands

# For packages
# install.packages("ggplot2") Anti-virus software might cause some issues... Disable/create an exception if necessary.
# Installation might also complain about Rtools4 on Windows, only needed if you want to build from source.
library(ggplot2)

# You might want to install car for the next computer exercise (though don't do it now)

# Assignment
y <- 1
# Prefer = to <- when using functions due to scoping rules
mean(x = 2) # Only internally to the function call
mean(x <- 2) # Assigned globally

# Vectors and matrices
?c
z <- c(1,2,3,4)
x <- c(5,6,7,8)
?cbind
m <- cbind(z,x)

# Indexing (Starts at 1)
?'['
z[1]
m[1,1]
m[1,] # Select first row
m[,'x'] # Select column with name 'x'
z[z>2] # Select elements that are greater than 2

# For strings, you can use either " " or ' '. Use ' ' if you want quotes in the text.

# String manipulation
a <- c('Hip','Joint')
b <- c(1,2)

?paste
names <- paste(a, b, sep="_")
paste(names, collapse = " + ")

?read.table
emission_data_og <- read.table('emissions.txt', header = T, sep = "\t")
emission_data_og$ObsNo

# Let's drop the first column
emission_data <- emission_data_og[,-1]

# Set seed to get deterministic results
set.seed(123)

# a)
?hist
?par

par(mfrow=c(2,2))

hist(emission_data$NOx)
hist(emission_data$Humidity)
hist(emission_data$Temp)
hist(emission_data$Pressure)

?apply # This is extremely useful
apply(emission_data,2,hist) # But not that pretty this time

?lapply # Same but with lists
var_names <- colnames(emission_data)
# Anonymous function needed since we are passing the same value to multiple arguments of hist.
sapply(var_names, function(x) hist(emission_data[, x], main = x, xlab = 'Unit'))

# Clearly all variables are not normally distributed (Skewness, general asymmetry)

# Use this to reset graphical parameters
dev.off()

# b)
?lm
# Some useful string manipulation features
linear_part <- paste0(var_names[-1], collapse = ' + ')
model_formula <- paste0(c('NOx', '~', linear_part),collapse = '')
model_formula

model_b <- lm(as.formula(model_formula), data = emission_data)
lm(NOx ~ ., data = emission_data) # A shorthand for above

# Useful function all around
?summary
summary(model_b)

# All elements of the summary

# Call: Model specification and data source
# Residuals: Certain descriptive statistics of the estimated residuals

# Coefficients:
# Estimate: The least squares (LS) estimates for the explanatory variables and the intercept
# Std. Error: Estimates for the standard deviations of the LS estimator

# t value: The value of the test statistic for the Wald test (a parametric test for maximum likelihood estimates)
# The test is: H_0: b_i = 0, H_1: b_i != 0 and is reasonable when residuals are normally distributed
# Pr(>|t|): The associated two-tailed p-value 
# Signif. codes: Traditional significance level thresholds

# Residual standard error: Estimate for the residual standard deviation
# Multiple R-squared: Coefficient of Determination
# Adjusted R-squared: Coeff. of Deter. adjusted for model degrees of freedom (i.e. the number of explanatory variables)
# F-statistic: A parametric test for the entire model against a base model that only has the intercept
# The test is: H_0: b_i = 0 for all i (excluding the intercept), H_1: b_i != 0 for some i
# Is reasonable if residuals are normally distributed

# c)
summary(model_b)$r.squared

# Or we can also do this by hand
SST <- sum((emission_data$NOx - mean(emission_data$NOx))^2)
SSE <- sum((resid(model_b)^2))
1-SSE/SST
SSM <- sum((fitted(model_b) - mean(emission_data$NOx))^2)

# R^2 measures the proportion of variation in the response that can be "explained" by the variation of the model

library(dplyr)
?near
near(SSM + SSE, SST) # This decomposition is the important observation

# Or alternatively
r_squared_og <- cor(fitted(model_b), emission_data$NOx)^2
r_squared_og

# d)
# Permutation test: H_0: b_i = 0, H_1: b_i != 0
# The high level idea: Permute the values of a given explanatory variable. This breaks the dependency structure of the response
# to that variable which essentially means that there is no effect. Using many different permutations, we can estimate the distribution
# of R^2 under the null hypothesis. The p-value is then the probability of observing at least as good of an R^2 as that computed
# from the original data under the null hypothesis.

# X: Data matrix, y: Response
fit_helper_d <- function(X, y, perm_var) {
  # Permute the values of perm_var
  X[,perm_var] <- sample(X[, perm_var])
  # LS estimate
  beta <- solve((t(X) %*% X)) %*% t(X) %*% y
  # Fitted values
  y_hat <- X %*% beta
  # R^2
  cor(y_hat, y)^2
}

perm_replicator_d <- function(n_perm, X, y, var_name) {
  # Generate n_perm permutation estimates of R^2 for var_name
  replicate(n_perm,  fit_helper_d(X, y, var_name))
}

# Sanity check
?rep
n <- nrow(emission_data)
Intercept <- rep(1, n)
X <- cbind(as.matrix(emission_data[,-1]), Intercept)
y <- emission_data$NOx
fit_helper_d(X,y, 'Temp')

n_perm <- 2000
alpha <- 0.05

expl_var <- var_names[-1]
# Compute the permutation estimates for each expl. variable individually
r_squares <- sapply(expl_var, function(name) perm_replicator_d(n_perm, X, y, name))

p_values_perm <- apply(r_squares, 2, function(x) sum(x > r_squared_og)/length(x))
p_values_perm # Not exactly the same as in the model solutions since here things are done in a different order.
p_values_perm < alpha # The null is rejected for Humidity and Temperature

# e)
model_e <- lm(NOx ~ . - Pressure, data = emission_data)
summary(model_e)
summary(model_b)
# The only substantial change is the estimate and precision of the intercept

# f)
# Drop Pressure
X_f <- X[, !(colnames(X) %in% c('Pressure'))]

# Residual variance
res_e <- resid(model_e)
res_var <- 1/(n-ncol(X_f)) * sum(res_e^2)
sqrt(res_var)

# Standard deviations of the LS estimators
LS_std <- sqrt(diag(res_var * solve((t(X_f) %*% X_f))))

LS_std <- LS_std[c(3,1,2)]

# g)

confint(model_e, level = 1-alpha)

# h)

beta <- coef(model_e)
?qt
beta - qt(1-alpha/2, n - ncol(X_f)) * LS_std
beta + qt(1-alpha/2, n - ncol(X_f)) * LS_std

# i)
# Bootstrapping. The high level idea: When we sample with replacement from the original data set,
# we are in a sense approximating the real sampling distribution of the random vectors. Using this bootstrap
# distribution allows us to approximate the desired quantiles of the true sampling distribution.

# X: Data matrix, y: Response
fit_helper_i <- function(X, y) {
  # Sample with replacement
  inds <- sample(1:nrow(X), replace = T)
  X <- X[inds,]
  y <- y[inds]
  # Bootstrap LS estimate
  solve((t(X) %*% X)) %*% t(X) %*% y
}

n_boot <- 2000

# Construct n_boot bootstrap estimates
boot_samples <- replicate(n_boot, fit_helper_i(X_f,y), simplify = 'matrix')
boot_samples <- cbind(boot_samples, beta[c(2,3,1)])
# Select the desired quantiles for the confidence interval
apply(boot_samples, 1, function(x) quantile(x, probs = c(alpha/2, 1 - alpha/2)))

par(mfrow = c(2,2))
var_names <- rownames(boot_samples)
sapply(var_names, function(x) hist(boot_samples[x,], main = x, xlab = 'Unit'))


# If we are a bit lazy...
library(boot)
?boot
?boot.ci

boot_helper <- function(data, inds) {
  data <- data[inds,]
  model <- lm(NOx ~ . - Pressure, data = data)
  coef(model)
}

boot_samps <- boot(emission_data, boot_helper, n_boot)
# Intercept, Humidity, Temp
lapply(seq(1,3), function(ind) boot.ci(boot_samps, conf = 1 - alpha, type = 'perc', index = ind)$perc)

# For HW
?plot
?abline
