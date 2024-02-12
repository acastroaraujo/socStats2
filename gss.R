
library(tidyverse)
library(gssr)
library(marginaleffects)
library(broom)

gss <- gss_get_yr(2022)

d <- gss |> 
  select(tvhours, degree, madeg, padeg) |> 
  mutate(
    pardeg = pmax(madeg, padeg, na.rm = TRUE),
    college = as.integer(degree >= 3),
    parcol = as.integer(pardeg >= 3)
  ) |> 
  select(tvhours, college, parcol) |> 
  drop_na()

d

m <- lm(tvhours ~ college * parcol, data = d)
tidy(m)

## ATE
p <- mean(d$parcol)
(1 - p) * coef(m)["college"] + p * (coef(m)["college"] + coef(m)["college:parcol"])
marginaleffects::avg_slopes(m, variables = "college")

## Assuming symmetrical effects

##  |       | Y1 | Y0 |
##  |-------|----|----|
##  | T = 1 | 10 | 7  |
##  | T = 0 | 8  | 5  |


## ATT/ ATU estimate
marginaleffects::avg_slopes(m, variables = "college", by = "college")
## ATT
lambda <- mean(d$parcol)
coef(m)["college"] + (coef(m)["parcol"] + coef(m)["college:parcol"])


## Weight both of these to get the ATE
# marginaleffects::avg_slopes(m, variables = "college", by = "parcol")
# marginaleffects::avg_slopes(m, variables = "college", by = "college")

## Are the differences statistically significant?
# avg_slopes(m, variables = "college", by = "college", hypothesis = "pairwise")

# Going back to GLMs

d <- gss |> 
  filter(wrkstat == 1) |> ## limit sample to full time workers
  haven::zap_missing() |> 
  select(realrinc, degree, madeg, padeg, sex, age) |> 
  mutate(
    pardeg = pmax(madeg, padeg, na.rm = TRUE),
    college = as.integer(degree >= 3),
    parcol = as.integer(pardeg >= 3),
    female = ifelse(sex == 2, 1L, 0L),
    realrinc = as.integer(floor(realrinc)), ## the last category is imputed with Pareto distribution
    age = as.integer(age)
  ) |> 
  select(realrinc, college, parcol, female, age) |> 
  drop_na()

# model with heterogeneity

m <- glm(realrinc ~ college * (parcol + female + age + I(age^2)), 
    data = d,
    family = "quasipoisson"
)

m |> tidy()
avg_slopes(m, variables = "college", type = "response")
avg_slopes(m, variables = "college", type = "link")

avg_slopes(m, variables = "college", type = "response", by = "college")

## ATE estimates rowwsie

mfx <- slopes(m, variables = "college", type = "response")

mfx |> 
  group_by(college) |> 
  summarize(avg = mean(estimate))


# Logistic ----------------------------------------------------------------

d <- gss |> 
  select(abany, degree, madeg, padeg, sex, age) |> 
  mutate(
    pardeg = pmax(madeg, padeg, na.rm = TRUE),
    college = ifelse(degree >= 3, 1L, 0L),
    parcol = ifelse(pardeg >= 3L, 1L, 0L),
    female = ifelse(sex == 2, 1L, 0L),
    abany = ifelse(abany == 1, 1L, 0L)
  ) |> 
  select(abany, college, parcol, female, age) 

skimr::skim(d)

d <- drop_na(d)

## model 1

m1 <- glm(abany ~ college * (parcol + female + age + I(age^2)), data = d, family = binomial("logit"))

avg_slopes(m1, variables = "college", type = "response") 
avg_slopes(m1, variables = "college", type = "response", by = "female") 

avg_slopes(m1, variables = "college", type = "response", by = "female", hypothesis = "pairwise")

## model 2

m2 <- glm(abany ~ college + (parcol + female + age + I(age^2)), data = d, family = binomial("logit"))

avg_slopes(m2, variables = "college", type = "response") 
avg_slopes(m2, variables = "college", type = "response", by = "college") 

avg_slopes(m2, variables = "college", type = "response", by = "female") |> 
  modelsummary::msummary(shape = ~ female)
