# Title     : Test what the `predict` function does for simple linear model (`lm`)
# Created by: josef
# Created on: 17.04.20

# 1. Generate Data
n_obs = 10000

x1 = runif(n_obs) * 3
x2 = runif(n_obs) * 2

mu = 2 + 3 * x1 + 4 * x2
y = rnorm(n_obs, mean = mu, sd = 1)

plot(mu, y)

# 2. Fit the model
model = lm("y ~ x1 + x2 ")

# 3. Compare theoretical predictions with `predict` function
beta_0 = model[["coefficients"]][["(Intercept)"]]
beta_1 = model[["coefficients"]][["x1"]]
beta_2 = model[["coefficients"]][["x2"]]

theoretical_prediction = beta_0 + beta_1 * x1 + beta_2 * x2
model_prediction = predict(model)

max_difference = max(abs(theoretical_prediction - model_prediction))
print(paste("Max difference in theoretical and fitted is", max_difference))
