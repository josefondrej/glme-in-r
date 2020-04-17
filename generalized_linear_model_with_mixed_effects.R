# Title     : Test what the `predict` function does for generalized linear
#             model with mixed effects (`glmer`)
# Created by: josef
# Created on: 17.04.20

library(lme4)

# 1. Generate Data
n_obs = 10000
n_groups = 5

x1 = runif(n_obs) * 3
x2 = runif(n_obs) * 2

groups = sample.int(n_groups, n_obs, replace = TRUE)
group_means = rnorm(n_groups, 0.0, 1.0)

random_intercept = group_means[groups]

eta = 2 + 3 * x1 + 4 * x2 + random_intercept
mu = exp(eta) # = E[Y | X] (we use log link)

# TODO: Find out if this is REALLY 100% how the Gamma should be simulated
phi = 0.5
shape_param = 1 / phi   # should correspond to the shape and scale params on wiki
scale_param = mu * phi

y = rgamma(n = n_obs, shape = shape_param, scale = scale_param)

plot(mu, y)

# 2. Fit the model
model = glmer("y ~ x1 + x2 + (1 | groups)", family = Gamma(link = "log"))

# 3. Compare theoretical predictions with `predict` function
coefficients = coef(model)
random_intercepts = coefficients$groups[["(Intercept)"]]
beta_0 = mean(random_intercepts)
fitted_group_means = random_intercepts - beta_0
fitted_random_intercept = fitted_group_means[groups]

beta_1 = coefficients$groups[["x1"]][1]
beta_2 = coefficients$groups[["x2"]][1]

# With fitted random intercepts
# theoretical_prediction = beta_0 + beta_1 * x1 + beta_2 * x2 + fitted_random_intercept
# model_prediction = predict(model, re.form=NULL, type="link") # This is the default

# WithOUT fitted random intercepts
theoretical_prediction = beta_0 + beta_1 * x1 + beta_2 * x2
model_prediction = predict(model, re.form=NA, type="link")

# Try also for the response, should be just exp
# In this case it is better to compare the diff relative to the response size
# theoretical_prediction = exp(beta_0 + beta_1 * x1 + beta_2 * x2 + fitted_random_intercept)
# model_prediction = predict(model, re.form=NULL, type="response")
# theoretical_prediction = exp(beta_0 + beta_1 * x1 + beta_2 * x2)
# model_prediction = predict(model, re.form=NA, type="response")


max_difference = max(abs(theoretical_prediction - model_prediction))
print(paste("Max difference in theoretical and fitted is", max_difference))
