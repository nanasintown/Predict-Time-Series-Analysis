#setwd('~/Prediction/Week_2')

# 2.1

tobacco_data <- read.table('tobacco.txt', header = T, sep = '\t')
tobacco_data <- tobacco_data[,c('CONSUMPTION', 'ILL')]

countries <- c("Iceland","Norway","Sweden","Canada","Denmark",
               "Austria","USA","Netherlands","Switzerland","Finland",
               "England")
tobacco_data$Country <- countries

# a)

tobacco_fit <- lm(ILL ~ CONSUMPTION, data = tobacco_data)
summary(tobacco_fit)

?plot
?abline

plot(tobacco_data$CONSUMPTION, tobacco_data$ILL, xlab = 'Cigarette consumption per capita in 1930', 
     ylab = 'Lung cancer cases per 100k in 1950', pch = 16)

abline(tobacco_fit, col = '#FF000075', lwd = 4)

text(x = tobacco_data$CONSUMPTION, y = tobacco_data$ILL, 
     labels = tobacco_data$Country, pos = 1, cex = 0.8)

# b)
FIT <- fitted(tobacco_fit)
RES <- resid(tobacco_fit)

# c)
# Here we would desire a linear one-to-one relationship between the fitted and observed values

plot(tobacco_data$ILL, FIT, xlab = 'Observed value', ylab = 'Fitted value', pch = 16)
abline(b = 1, a = 0, col = '#FF000075', lwd = 4)

text(x = tobacco_data$ILL, y = FIT, 
     labels = tobacco_data$Country, pos = 1, cex = 0.8)

# R^2
cor(tobacco_data$ILL, FIT)^2

# Couple of things to look for in this residual plot:
# If there is any systematic behavior in the residuals as a function of the fitted value,
# there is some kind of model misspecification.
# There are two main sources for it: the systematic part is misspecified or the random part
# is misspecified. Sometimes it can be hard to tell where the misspecification lies.

# If the systematic part is misspecified, then usually the estimated residuals behave in such a way
# that their sign changes systematically in different parts of fitted value range (i.e. E[e_i] != 0 but instead E[e_i](X)).
# This usually indicates the presence of some kind of non-linear structure
# (can sometimes be remedied with a transformation of certain explanatory variables)

# Example: Non-linear structure ----

x <- runif(60, min = 0, max = 7)
y <- 2 + exp(0.5 *x) + rnorm(60, sd = 1)

exampl_fit <- lm(y ~ x)
summary(exampl_fit)
plot(x,y, pch = 16)
abline(exampl_fit)

# Clearly the linear model fails to account for the exponential behavior
plot(fitted(exampl_fit), resid(exampl_fit), xlab = 'Fitted value', ylab = 'Estimated residual', pch = 16)
abline(b = 0, a = 0)

# The systematic change in the sign of the estimated residuals

# One can try to log-transform the response

log_y <- log(y)

exampl_fit_log <- lm(log_y ~ x)
summary(exampl_fit_log)
plot(x,log_y, pch=16)
abline(exampl_fit_log)

# Better but the non-linear transformation seems to have introduced some heteroscedasticity
plot(fitted(exampl_fit_log), resid(exampl_fit_log), xlab = 'Fitted value', ylab = 'Estimated residual', pch = 16)
abline(b = 0, a = 0)

# ----

# If the random part is misspecified, then the range of the estimated residuals varies as a function
# of the fitted value (i.e. Var[e_i] != sigma^2 but Var[e_i](X)).

# Example: Heteroscedasticity ----
x <- runif(80, min = 100, max = 1000)
y <- 10 + 10*x + rnorm(80, sd = x)
exampl_fit <- lm(y ~ x)
plot(x,y, pch = 16)
abline(exampl_fit)
summary(exampl_fit)

# The deviations from the fitted line seem to increase as a function of x

plot(fitted(exampl_fit), resid(exampl_fit), xlab = 'Fitted value', ylab = 'Estimated residual', pch = 16)
abline(a = 0, b = 0)

# The range of variation of the estimated residuals seems to increase as a function of the fitted value.

# ----

plot(FIT ,RES, xlab = 'Fitted value', ylab = 'Estimated residual', pch = 16)
abline(b = 0, a = 0)

text(FIT, RES, 
     labels = tobacco_data$Country, pos = 1, cex = 0.8)

# d)
# The estimated residual of observation USA deviates substantially from the other estimated residuals (with maybe
# England having a similar value)

# e)
?cooks.distance
cooks_d <- cooks.distance(tobacco_fit)

plot(cooks_d, xaxt="n", xlab=" ", ylab="Cook's distance", pch = 16)
axis(side=1, at=1:11, labels=countries,las=2)

# Clearly the Cook's Dist. of USA differs substantially from the other observations. Thus it is an influential observation.

# f) 
tobacco_data_filt <- tobacco_data[!(tobacco_data$Country %in% c('USA')),]

tobacco_filt_fit <- lm(ILL ~ CONSUMPTION, data = tobacco_data_filt)

summary(tobacco_fit)
summary(tobacco_filt_fit)

# Substantial improvements are observed in model quality measures, a larger effect on lung cancer cases is also observed.
plot(tobacco_data$CONSUMPTION,tobacco_data$ILL, xlab = 'Cigarette consumption per capita in 1930', 
     ylab = 'Lung cancer cases per 100k in 1950', pch = 16)
abline(tobacco_fit, col = '#FF000075', lwd = 4)
abline(tobacco_filt_fit, col = '#0000FF75', lwd = 4)
legend('topleft', legend = c('With USA', 'Without USA'), lty = 1, col = c('#FF000075','#0000FF75' ))

text(x = tobacco_data$CONSUMPTION, y = tobacco_data$ILL, 
     labels = tobacco_data$Country, pos = 1, cex = 0.8)

cooks_d_filt <- cooks.distance(tobacco_filt_fit)
plot(cooks_d_filt, xaxt="n", xlab=" ", ylab="Cook's distance", pch = 16)
axis(side=1, at=1:10, labels=tobacco_data_filt$Country,las=2)

# Can the removal of observation USA be justified? According to the model solution, during 1930
# tobacco was milder in the USA in comparison to the other nations. Additionally, filters had been
# introduced in the United States unlike in the other countries. The source for the claims is shrouded
# in mystery (something to do with Mellins' materials) but if we assume this to be true, then the carcinogenic
# "load" of individual cigarettes are not the same between United States and other nations. Thus the metric
# "Cigarette Consumption per Capita" cannot be used to compare countries in a meaningful fashion.

# Without knowledge of the exact carcinogenic "load", it is quite reasonable to remove the incommensurate observation.

# 2.2

library(car)

cement_data <- read.table('hald.txt', header = T)

# It might be useful to calculate some simple descriptive statistics before estimating the model
cor(cement_data)
# Each chemical has a substantial correlation with the response
# However, the chemicals also exhibit substantial pair-wise correlation
# This can indicate (though not imply) possible issues with precision of the LS estimates

# a)
cement_full_m <- lm(HEAT ~ . - SUM, data = cement_data)
summary(cement_full_m)
# While the model seems to explain the variation of the response very well (R^2, F-test etc.),
# none of the LS estimates are significant at the 5 percent significance level, at least according to the t-test.
# This situation tends to arise when the explanatory variables exhibit strong linear dependencies between themselves,
# referred to as multicollinearity (in a sense, the generalization of correlation to multiple variables)

# Some geometric intuition about the phenomenon ----
library(plotly)

multi_col_helper <- function(n, corr, sigma_e) {
  x <- rnorm(n)
  z <- corr * x + sqrt(1 - corr^2) * rnorm(n)
  y <- 0.5 * x + 0.5*z + rnorm(n, sd = sigma_e)
  
  model <- lm(y ~x + z)
  print(summary(model))
  
  eval_points <-  seq(-8, 10, by = 0.1)
  
  obj_f_val <- outer(eval_points, eval_points, Vectorize(function(b_1,b_2) sum((y - b_1 * x - b_2 * z)^2)))
  
  plot_ly(x = eval_points, y =  eval_points, z = obj_f_val, type = 'contour')
}

# Manipulate the sample size, correlation and residual SD to observe the effects on the SD of the estimators
multi_col_helper(50, 0, 0.1)
# ----

# For higher dimensional cases, we can use the R^2 of the model of explanatory variables to estimate the degree of
# linear dependency
model_CHEM2 <- lm(CHEM2 ~ . - SUM - HEAT, data = cement_data)
summary(model_CHEM2)
# R^2 is high, so the linear dependency is substantial. Its effect on the standard deviations of the LS estimators can
# be measured using the Variance Inflation Factor (VIF) which after some standardization of the model variables, can be
# shown to multiplicatively inflate the SD of the estimators i.e. SD[beta_i] = VIF_i * ...
# However, it is important to note that other factors, such as R^2 of the model, sample size etc. also affect
# the SDs of the LS estimators. Thus high VIF is not an issue on its own but only if it can be identified as
# a substantial factor in reducing the precision of the estimates.
# Inspired by: A Caution Regarding Rules of Thumb for Variance Inflation Factors

# For CHEM2, the VIF is
1/(1 - summary(model_CHEM2)$r.squared)
vif(cement_full_m)

# In this case, the issue is that
cement_data$SUM
# is nearly 100 for each batch. Thus given three chemicals the proportion of the remaining chemical
# can be quite accurately predicted by
100 - rowSums(cement_data[,!(names(cement_data) %in% c('CHEM1', 'HEAT', 'SUM'))])
cement_data$CHEM1

# Example of prediction uncertainty with bootstrap ----

pred_helper <- function(X, y, og_resid, x) {
  # Sample with replacement
  inds <- sample(1:nrow(X), replace = T)
  X <- X[inds,]
  y <- y[inds]
  # Bootstrap LS estimate
  beta_b <- solve((t(X) %*% X)) %*% t(X) %*% y
  # Bootstrap prediction
  x %*% beta_b + sample(og_resid, 1)
}

n_boot <- 2000
alpha <- 0.05

n <- nrow(cement_data)
Intercept <- rep(1,n)
X <- cbind(Intercept, as.matrix(cement_data[,-c(5,6)]))
pred_x <- apply(X, 2, function(x) quantile(x, probs = 0.3)) #colMeans(X)
y <- cement_data$HEAT

# Construct n_boot bootstrap estimates
boot_samples <- replicate(n_boot, pred_helper(X, y, resid(cement_full_m), pred_x))
pred_y <- coef(cement_full_m) %*% pred_x
boot_samples <- c(pred_y, boot_samples)

# Select the desired quantiles for the confidence interval
quantile(boot_samples, probs = c(alpha/2, 1 - alpha/2))
hist(boot_samples, xlab = 'Prediction', main = 'Bootstrap distribution')
abline(v = pred_y, col = 'red', lwd = 2)
# ----

# b)

# To perform model selection, we need a metric to evaluate the quality of the model. There are roughly two aspects
# to consider: quality of fit and size of the model. Quality of fit measures how well the model fits the observed
# data (e.g. Sum of Squared Residuals loss function). Minimizing this loss is clearly desirable. 
# However, increasing the capacity of the model by e.g. adding additional expl. variables 
# can cause the model to overfit to the observed data. 
# Overfitting tends to cause substantial increases in Out-of-Sample prediction errors (generalization error)
# and increase the uncertainty of the estimated parameters. Thus model size/capacity should be penalized.

# Akaike Information Criterion (AIC) is one such model selection criterion. It can be used to compare
# models fitted using the same likelihood function. The smaller the AIC, the better. It can be shown that
# AIC is asymptotically equivalent to Leave-One-Out Cross-Validation (CV) in which the one data point is removed,
# the remaining are used to fit the model and the prediction error is calculated using the omitted data point.
# This CV error estimates the Out-of-Sample prediction error. Thus models with lower AIC should perform better
# in Out-of-Sample evaluations, at least asymptotically.
?extractAIC

?step # Performs greedy search for the model with the smallest AIC (by default, backward elimination).
step(cement_full_m)

# The greedy search results in
cement_AIC <- lm(HEAT ~ . - SUM - CHEM3, data = cement_data)
summary(cement_AIC)

# One way of implementing this by hand

AIC_helper <- function(model_formula, excluded_var) {
  new_model <- paste0(c(model_formula, ' - ', excluded_var), collapse = '')
  model <- lm(as.formula(new_model), data = cement_data)
  extractAIC(model)[2]
}

expl_vars <- colnames(cement_data)[-c(5,6)]

while (length(expl_vars) > 0) {
  linear_part <- paste0(expl_vars, collapse = ' + ')
  model_formula <- paste0(c('HEAT', ' ~ ', linear_part), collapse = '')
  
  model <- lm(as.formula(model_formula), data = cement_data)
  AIC_vals <- extractAIC(model)[2]
  
  AIC_vals <- c(AIC_vals, sapply(expl_vars, function(name) AIC_helper(model_formula, name)))
  print(AIC_vals)
  min_index <- which.min(AIC_vals)
  
  if (min_index == 1) {
    break
  } else {
    expl_vars <- expl_vars[-(min_index-1)]
  }
  
}

linear_part <- paste0(expl_vars, collapse = ' + ')
model_formula <- paste0(c('HEAT', ' ~ ', linear_part), collapse = '')
cement_AIC_self <- lm(as.formula(model_formula), data = cement_data)
summary(cement_AIC_self)

vif(cement_AIC)
# Some variance inflation seems to remain and the corresponding variables are still insignificant at 5 % level.
# AIC only evaluates the full model so it makes sense that sometimes even small improvements in the fit compensate
# for the increased model size.

hist(resid(cement_full_m), breaks = 8) # Residual distribution does not seem normal, thus AIC might not have
# the properties that motivate its usage.

# 2.3 A good idea would be to write a function that performs the permutation test for a given model for each
# explanatory variable. Then you can either automate the process or do it manually one model at a time.

# 2.4 Scatter plots of the original data with fitted curves
# and residual plots are the most useful diagnostics. For b), check ?curve for plotting the fit.
