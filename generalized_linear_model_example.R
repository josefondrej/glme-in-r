# Title     : Test what the `predict` function does for generalized linear model
# Created by: josef
# Created on: 17.04.20

library(lme4)

# 1. Generate Data
n_obs = 10000

x1 = runif(n_obs) * 3
x2 = runif(n_obs) * 2

eta = 2 + 3 * x1 + 4 * x2
mu = exp(eta) # = E[Y | X] (we use log link)

# TODO: Find out if this is REALLY 100% how the Gamma should be simulated
phi = 0.5
shape_param = 1 / phi   # should correspond to the shape and scale params on wiki
scale_param = mu * phi

y = rgamma(n = n_obs, shape = shape_param, scale = scale_param)

plot(mu, y)

# 2. Fit the model
model = glm("y ~ x1 + x2 ", family=Gamma(link="log"))

# 3. Compare theoretical results with the `predict` function
beta_0 = model[["coefficients"]][["(Intercept)"]]
beta_1 = model[["coefficients"]][["x1"]]
beta_2 = model[["coefficients"]][["x2"]]

theoretical_prediction = exp(beta_0 + beta_1 * x1 + beta_2 * x2)
model_prediction = predict(model, type="response")

theoretical_prediction = beta_0 + beta_1 * x1 + beta_2 * x2
model_prediction = predict(model, type="link")

max_difference = max(abs(theoretical_prediction - model_prediction))
print(paste("Max difference in theoretical and fitted is", max_difference))



