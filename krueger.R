
library(tidyverse)
library(modelsummary)

d <- haven::read_dta("data/cardkrueger1994.dta") |> 
  haven::zap_labels()


# pre-post ----------------------------------------------------------------

dnj <- d |> 
  filter(treated == 1)

dnj |> 
  summarize(mean_fte = mean(fte), .by = t)

# make wide
dnj_wide <- dnj |> 
  pivot_wider(id_cols = id, names_from = t, values_from = fte, names_prefix = "fte")

## paired t test
with(dnj_wide, t.test(Pair(fte1, fte0) ~ 1))

## long form (wrong)
## assumes we have twice the amount of data
ols <- lm(fte ~ t, data = dnj)


## long form (right)
mlm <- lme4::lmer(fte ~ t + (1 | id), data = dnj) ## check that I'm using the correct dataset
# glmmTMB::glmmTMB(fte ~ t + (1 | id), data = dnj)

modelsummary(list(OLS = ols, MM = mlm))



# did ---------------------------------------------------------------------

a <- lme4::lmer(fte ~ t*treated + (1 | id), data = d) 

b <- lm(fte ~ t*treated, data = d) 

modelsummary(list(a, b))

## this is really cool, make sure to check out Steve's code
