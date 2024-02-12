
library(tidyverse)

N <- 1e5


# baseline bias -----------------------------------------------------------

sim <- tibble(
  s = rbinom(N, 1, 1/2),
  y0 = rnorm(N, 2000 + 1000*s, 500),
  y1 = rnorm(N, 3000 + 1000*s, 500),
  d = rbinom(N, 1, 0.25 + 0.5*s),
  y = ifelse(as.logical(d), y1, y0)
)

## E[Y1 | D = 0] = 3000 * 1000 * 0.25
## E[Y0 | D = 0] = 2000 * 1000 * 0.25

## E[Y1 | D = 0] = 3000 * 1000 * 0.75
## E[Y0 | D = 0] = 2000 * 1000 * 0.75

sim |> 
  group_by(d) |> 
  summarize(across(c(y0, y1), mean))

lm(y ~ d, data = sim)
lm(y ~ d + s, data = sim)

# baseline bias + treatment heterogeneity ---------------------------------

sim <- tibble(
  s = rbinom(N, 1, 1/2),
  y0 = rnorm(N, 2000 + 1000*s, 500),
  y1 = rnorm(N, 3000 + 1500*s, 500), ## this is the change
  d = rbinom(N, 1, 0.25 + 0.5*s),
  y = ifelse(as.logical(d), y1, y0)
)

## ATT = 1375 
## ATU = 1125

sim |> 
  group_by(d) |> 
  summarize(across(c(y0, y1), mean))

lm(y ~ d, data = sim)
lm(y ~ d + s, data = sim)
