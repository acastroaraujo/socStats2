
# Packages ----------------------------------------------------------------

library(tidyverse)
library(broom)
library(cobalt)
library(MatchIt)
library(WeightIt)

# Helpers -----------------------------------------------------------------

love_plot <- function(x, ...) {
  cobalt::love.plot(x, 
                    binary = "std" ,
                    stats = c("m", "ks") ,
                    thresholds = c(.1, .05),
                    var.order = "adjusted",
                    abs = TRUE,
                    ...
  )
}

# Data --------------------------------------------------------------------

d <- haven::read_dta("data/cattaneo2.dta")

d <- d |>  
  haven::zap_labels() 

skimr::skim(d)

d <- d |> 
  mutate(mcollege = as.integer(medu >= 16))

# Regression --------------------------------------------------------------

ate_ols <- lm(bweight ~ mbsmoke, data = d)
tidy(ate_ols, conf.int = TRUE) ## naive estimate

# Exact Matching ----------------------------------------------------------

cat_vars <- c("mmarried", "mhisp", "mrace", "foreign", "alcohol", "deadkids", "prenatal1", "mcollege")

f <- mbsmoke ~ mmarried + mhisp + mrace + foreign + alcohol + deadkids + prenatal1 + mcollege

# f2 <- reformulate(
#   response = "mbsmoke",
#   termlabels = c("mmarried", "mhisp", "foreign", "alcohol", "deadkids", "prenatal1", "mcollege")
# )


# f3 <- as.formula("mbsmoke ~ mmarried + mhisp + foreign + alcohol + deadkids + prenatal1 + mcollege")

match_obj <- matchit(f, data = d, method = "exact", estimand = "ATT")

summary(match_obj)

## gets original data, augmented with subclass and weight, and with unmatched dropped
match.data(match_obj)

mdata <- match.data(match_obj)

## Exact matching is just group_by() + drop rows without common support
d |> 
  summarise(smokers = sum(mbsmoke), nonsmokers = sum(1 - mbsmoke), .by = all_of(cat_vars))


love_plot(match_obj) ## of course they are perfectly balanced

sum(match_obj$weights == 0)

att_match_ols <- lm(bweight ~ mbsmoke, data = d, weights = match_obj$weights)
lm(bweight ~ mbsmoke, data = d, weights = match_obj$weights)
lm(bweight ~ mbsmoke, data = mdata, weights = weights)
tidy(att_match_ols, conf.int = TRUE)


# Propensity Score --------------------------------------------------------

con_vars <- c("mage", "medu", "nprenatal")

f2 <- reformulate(
  response = "mbsmoke",
  termlabels = c(cat_vars, con_vars, str_glue("I({con_vars}^2)"))
)

weight_obj <- weightit(f2, method = "ps", estimand = "ATT", data = d)

summary(weight_obj)

love_plot(weight_obj)

lm(bweight ~ mbsmoke, data = d, weights = weight_obj$weights) |> 
  tidy()

# CBPS --------------------------------------------------------------------

cbps_obj <- weightit(f2, method = "CBPS", estimand = "ATT", data = d)

summary(cbps_obj)

love_plot(cbps_obj)

lm(bweight ~ mbsmoke, data = d, weights = cbps_obj$weights) |> 
  tidy()

## over = FALSE (for overidentified) if you can't achieve convergence, 
## the only thing that matters is covariate balancing, not estimating the probability
## of the treatment. This produces a somewhat less efficient dataset (lower ESS).
cbps_obj2 <- weightit(f2, method = "CBPS", estimand = "ATT", data = d, over = FALSE)

summary(cbps_obj2)

# Lalonde Data ------------------------------------------------------------

load("data/exercise_data.RData")

glimpse(d)
glimpse(d_exper)

lalonde_vars <- setdiff(colnames(d), c("treat", "re78"))

ff <- reformulate(
  response = "treat", 
  termlabels = paste(c(lalonde_vars, "I(age^2)", "I(educ^2)"), collapse = " + ")
)

ps_obj <- weightit(ff, data = d, method = "glm", estimand = "ATT")

bal.plot(ps_obj)
love_plot(ps_obj)
summary(ps_obj)

cbps_obj <- weightit(ff, data = d, method = "CBPS", estimand = "ATT")

bal.plot(cbps_obj)
love_plot(cbps_obj)
summary(cbps_obj)

lm(re78 ~ treat, data = d, weights = cbps_obj$weights) |> 
  tidy()

# Regression --------------------------------------------------------------

## See variance weighted vs propensity weighted treatment effects

ff_reg <- reformulate(
  response = "re78", 
  termlabels = paste(c("treat", lalonde_vars, "I(age^2)", "I(educ^2)"), collapse = " + ")
)

reg_ate1 <- lm(ff_reg, data = d)

tidy(reg_ate1, conf.int = TRUE) |> 
  filter(term == "treat")

reg_ate2 <- lm(re78 ~ treat * (age + educ + black + hisp + married + nodegr + 
                 re74 + re75 + u74 + u75 + I(age^2) + I(educ^2)), data = d)

marginaleffects::avg_slopes(reg_ate1, variables = "treat")

marginaleffects::slopes(reg_ate2, variables = "treat") |> 
  marginaleffects::plot_slopes(by = "age")



# Entropy Balance ---------------------------------------------------------

ff_list <- reformulate(
  response = "treat",
  termlabels = paste(lalonde_vars, collapse = " + ")
)

ebal_obj <- weightit(ff_list, data = d, method = "ebal", estimand = "ATT", moments = 3, maxit = 1e5)

summary(ebal_obj)

lm(re78 ~ treat + re75 + , data = d, weights = ebal_obj$weights) |> 
  tidy(conf.int = TRUE)



