
library(tidyverse)
library(broom)
library(WeightIt)

set.seed(1234)
N <- 1e4

simdata <- tibble(
  x = rbinom(N, 1, 1/2),
  z = rbinom(N, 1, 1/2),
  treat_prob = ifelse(x*z == 1, 3/4, 1/4),
  treat = rbinom(N, 1, treat_prob),
  y = 2*x*z + treat + rnorm(N)
)

glimpse(simdata)

# naive estimator ---------------------------------------------------------

naive <- lm(y ~ treat, data = simdata)
tidy(naive)

# regression (wrong functional form) --------------------------------------

wrong_form <- lm(y ~ treat + x + z, data = simdata)
tidy(wrong_form)

# regression (correct functional form) ------------------------------------

correct_form <- lm(y ~ treat + x*z, data = simdata)
tidy(correct_form)

lm(y ~ treat + x:z, data = simdata) |> tidy() ## actual correct form


# propensity score (wrong answer) -----------------------------------------

ps_weight <- weightit(treat ~ x + z, data = simdata, method = "glm", estimand = "ATE")
ps_est <- lm(y ~ treat, data = simdata, weights = ps_weight$weights)
tidy(ps_est)

# CBPS (wrong answer) -----------------------------------------------------

cbps_weight <- weightit(treat ~ x + z, data = simdata, method = "cbps", estimand = "ATE", over = FALSE)
## cobalt::love.plot(cbps_weight)

cbps_est <- lm(y ~ treat, data = simdata, weights = cbps_weight$weights)
tidy(cbps_est)

# Entropy Balance (wrong) -------------------------------------------------

## moments argument is empty because covariates are binary
ebal_weight <- weightit(treat ~ x + z, data = simdata, method = "ebal", estimand = "ATE")
## cobalt::love.plot(ebal_weight)

ebal_est <- lm(y ~ treat, data = simdata, weights = ebal_weight$weights)
tidy(ebal_est)


# Doubly robust -----------------------------------------------------------

lm(y ~ treat + x*z, data = simdata, weights = ebal_weight$weights) |> tidy()

# Matching works just right! ----------------------------------------------

matching <- MatchIt::matchit(treat ~ x + z, data = simdata, method = "exact", estimand = "ATE")
matching_est <- lm(y ~ treat, data = simdata, weights = matching$weights)
tidy(matching_est)

## doubly robust (smaller standard errors)

lm(y ~ treat + x + z, data = simdata, weights = matching$weights) |> 
  tidy()

# Even better! ------------------------------------------------------------

lm(y ~ treat + x * z, data = simdata, weights = matching$weights) |> 
  tidy()

# Making weighting work ---------------------------------------------------

lm(formula = y ~ treat, 
   data = simdata, 
   weights = weightit(treat ~ x * z, data = simdata, method = "cbps")$weights
) |> 
  tidy()

lm(formula = y ~ treat, 
   data = simdata, 
   ## includes all bivariate interactions
   weights = weightit(treat ~ x * z, data = simdata, method = "ebal", int = TRUE)$weights
) |> 
  tidy()

# Bootstrap ---------------------------------------------------------------

library(future.apply)
plan(multisession, workers = parallel::detectCores())

boot <- future_replicate(n = 1e3, {
  
  i <- sample(nrow(simdata), replace = TRUE)
  w <- weightit(treat ~ x + z, data = simdata[i, ], method = "ebal", int = TRUE)
  m <- lm(y ~ treat, data = simdata[i, ], weights = w$weights)
  return(coefficients(m)["treat"])
  
})

# mean(boot) ## People usually ignore this and use the previous one!
sd(boot)
