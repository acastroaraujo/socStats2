
library(tidyverse)
library(gssr)
library(marginaleffects)

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
