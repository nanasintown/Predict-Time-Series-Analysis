# 2.3
library(car)
hald <- read.table("hald.txt",header=T,sep="\t")
hald.matrix <- as.matrix(hald[0:4])

fullmodel=lm(HEAT~CHEM1+CHEM2+CHEM3+CHEM4,data=hald)
summary(fullmodel)
set.seed(123)
r_squared_og <- summary(fullmodel)$r.squared
r_squared_og

# Test full model
var_names <- colnames(hald[0:4])

fit_helper_d <- function(X, y, perm_var) {
  # Permute the values of perm_var
  X[,perm_var] <- sample(X[,perm_var])
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
n <- nrow(hald)
Intercept <- rep(1, n)
X <- cbind(hald.matrix, Intercept)
y <- hald$HEAT

n_perm <- 2000
alpha <- 0.05

expl_var <- var_names[]
# Compute the permutation estimates for each expl. variable individually
r_squares <- sapply(expl_var, function(name) perm_replicator_d(n_perm, X, y, name))

p_values_perm <- apply(r_squares, 2, function(x) sum(x > r_squared_og)/length(x))
p_values_perm # Not exactly the same as in the model solutions since here things are done in a different order.
p_values_perm < alpha

# -------------
# Test remove CHEM3
# permutation test for model without chem3 as this variable is the most insignificant
set.seed(123)
model_1 <- lm(HEAT~CHEM1+CHEM2+CHEM4,data=hald)
summary(model_1)
var_names <- c("CHEM1",'CHEM2','CHEM4')

fit_helper_d <- function(X, y, perm_var) {
  # Permute the values of perm_var
  X[,perm_var] <- sample(X[,perm_var])
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
n <- nrow(hald)
Intercept <- rep(1, n)
X <- cbind(hald.matrix[,-3], Intercept)
y <- hald$HEAT

n_perm <- 2000
alpha <- 0.05

expl_var <- var_names[]
# Compute the permutation estimates for each expl. variable individually
r_squares <- sapply(expl_var, function(name) perm_replicator_d(n_perm, X, y, name))

p_values_perm <- apply(r_squares, 2, function(x) sum(x > r_squared_og)/length(x))
p_values_perm # Not exactly the same as in the model solutions since here things are done in a different order.
p_values_perm < alpha


#---------------
# Test remove CHEM 4 as this variable is the most insignificant

set.seed(123)
model_2 <- lm(HEAT~CHEM1+CHEM2,data=hald)
summary(model_2)
var_names <- colnames(hald[0:2])
fit_helper_d <- function(X, y, perm_var) {
  # Permute the values of perm_var
  X[,perm_var] <- sample(X[,perm_var])
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
n <- nrow(hald)
Intercept <- rep(1, n)
X <- cbind(hald.matrix[,0:2], Intercept)
y <- hald$HEAT

n_perm <- 2000
alpha <- 0.05

expl_var <- var_names[]
# Compute the permutation estimates for each expl. variable individually
r_squares <- sapply(expl_var, function(name) perm_replicator_d(n_perm, X, y, name))

p_values_perm <- apply(r_squares, 2, function(x) sum(x > r_squared_og)/length(x))
p_values_perm # Not exactly the same as in the model solutions since here things are done in a different order.
p_values_perm < alpha

#-------------------------





