
# Packages ----------------------------------------------------------------

library(tidyverse)
library(broom)
library(cobalt)
library(MatchIt)
library(WeightIt)

# Helpers -----------------------------------------------------------------

love_plot <- function(x) cobalt::love.plot(x, binary = "std")

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

#att_match_ols <- lm(bweight ~ mbsmoke, data = d, weights = match_obj$weights)
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

lm(bweight ~ mbsmoke, data = d, weights = weight_obj$weights) |> 
  tidy()

