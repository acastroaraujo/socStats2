
library(tidyverse)
library(panelr)
library(glmmTMB)
library(fixest)

data("teen_poverty", package = "panelr")

d <- teen_poverty |> 
  long_panel(
    id = "id", 
    wave = "t",
    begin = 1,
    end = 5
  )

line_plot(d, var = hours, id = "id", wave = "t", subset.ids = TRUE, n.random.subset = 20)


## figure out the t-values
fixest::feols(
  pov ~ mother | id + t,
  data = d
)

fixest::feols(
  pov ~ mother | id + t,
  data = d,
  cluster = ~ id + t
)

fixest::feols(
  pov ~ mother | id + t,
  data = d,
  vcov = "twoway"
) |> summary()

plm::plm(
  pov ~ mother,
  data = d,
  index = c("id", "t"),
  model = "within",
  effect = "twoways"
) |> 
  summary()


## asdfasdf

data(WageData)

wagemod1 <- feols(lwage ~ union + occ + ind + south + ms | id + t, data = WageData)


glmmTMB(lwage ~ 1  + t + (1 + t | id), data = WageData) |> 
  ggeffects::ggpredict(terms = c("t", "id [sample = 9]"), type = "random", ci_level = NA) |> plot()


glmmTMB(lwage ~ 1  + t + (1 + t | id), data = WageData) |> 
  ggeffects::ggpredict(terms = c("t", "id [sample = 9]"), type = "random", ci_level = NA) |> plot()

library(tidyverse); library(modelsummary); library(fixest)
od <- causaldata::organ_donations

# Treatment variable
od <- od %>%
  mutate(Treated = State == 'California' & 
           Quarter %in% c('Q32011','Q42011','Q12012'))

# feols clusters by the first
# fixed effect by default, no adjustment necessary
clfe <- feols(Rate ~ Treated | State + Quarter,
              data = od, 
              cluster = ~ State + Quarter
              )

clfe

ols <- lm(Rate ~ Treated + factor(State) + factor(Quarter), data = od)

ols |> 
  broom::tidy() |> 
  filter(str_detect(term, "Treated"))

plm <- plm::plm(
  Rate ~ Treated,
  data = od,
  index = c("State", "Quarter"),
  model = "within",
  effect = "twoways"
) 


library(plm); library(fixest)
od <- causaldata::organ_donations

clfe <- feols(Rate ~ Treated | State + Quarter,
              data = od, 
              cluster = ~ State + Quarter
)

clfe

plm <- plm::plm(
  Rate ~ Treated,
  data = od,
  index = c("State", "Quarter"),
  model = "within",
  effect = "twoways"
) 

lmtest::coeftest(plm, vcov = plm::vcovHC) 


# new illustration --------------------------------------------------------

library("tidyverse")
library("panelr")
library("glmmTMB")
library("fixest")
library("broom")

data("teen_poverty")

d <- teen_poverty |> 
  long_panel(id = "id",
             wave = "t",
             begin = 1,
             end = 5)

# ML cluster
ml1 <- femlm(pov ~ mother | id + t,
             data = d,
             family = "gaussian",
             cluster = ~ id + t)

# ML no cluster
ml2 <- femlm(pov ~ mother | id + t,
             data = d,
             family = "gaussian")

# OLS cluster
ols1 <- feols(pov ~ mother | id + t,
              data = d,
              cluster = ~ id + t)

# OLS no cluster
ols2 <- feols(pov ~ mother | id + t,
              data = d)

tidy(ml1)   # ML cluster
tidy(ols1)  # OLS cluster
tidy(ml2)   # ML no cluster
tidy(ols2)  # OLS no cluster

ml1
ml2
ols1
ols2

get_pvalue <- function(x) {
  z_value <- x$coefficients / x$se
  cat("pnorm:", pnorm(-abs(z_value)) * 2, "\n")
  cat("pt:", pt(-abs(2.84828), max(1, x$nobs - x$nparams)) * 2, "\n")
}

ml1
tidy(ml1)
get_pvalue(ols1)

plm::plm(
  pov ~ mother, 
  data = d,
  index = c("id", "t"),
  model = "within",
  effect = "twoways"
) |> 
  ## plm::vcovHC is equivalent to ml2 and ols2
  lmtest::coeftest(vcov = plm::vcovHC) 


ols1$hessian
ols2$hessian

ols1$cov.scaled
ols2$cov.scaled

lm_ols <- lm(pov ~ mother + factor(id) + factor(t), data = d)
lm_ols |> 
  tidy() |> 
  filter(str_detect(term, "mother"))
