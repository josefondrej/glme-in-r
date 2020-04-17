# Title     : Minimal example for testing random effects models in R
# Objective : TODO
# Created by: josef
# Created on: 15.04.20
library(lme4)

# 1. Generate Data
n_obs = 10000
n_groups = 20

x1 = runif(n_obs) * 3
x2 = runif(n_obs) * 2
grp = factor(trunc(runif(n_obs) * n_groups))

eta = 2 + 3 * x1 + 4 * x2
mu = exp(eta) # = E[Y | X] (we use log link)

phi = 0.5
shape_param = 1 / phi
scale_param = mu * phi

y = rgamma(n = n_obs, shape = shape_param, scale = scale_param)

# 2. Fit the model
model = glmer("y ~ x1 + x2 + (1|grp)", family = Gamma(link='log'))

# 3. Analyze results
plot(mu, fitted(model))
abline(0,1, col = "red")