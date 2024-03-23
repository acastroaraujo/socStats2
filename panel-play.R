
# Packages ----------------------------------------------------------------

library(tidyverse)
theme_set(theme_light())

# Basic Setup -------------------------------------------------------------

N <- 595 ## number of people
T <- 7   ## number of time periods

grid <- tibble(
  id = 1:N,
  t = list(1:T)
)

grid

# unrealistic -------------------------------------------------------------

sim <- grid |> 
  ## The dplyr::n() function can only be used inside dplyr things
  ## like mutate() or filter().
  ## It is similar to the nrow() function in base R.
  mutate(college = rbinom(n(), 1, .2)) |> 
  unnest(t) |> 
  mutate(wage_pred = 6.58 + 0.353*college) |> 
  mutate(wage = rnorm(n(), wage_pred, sd = 0.434)) 

sim |> 
  ## subset 42 individuals for plotting
  filter(id %in% sample(1:N, 42)) |> 
  mutate(college = factor(college)) |> 
  ggplot(aes(t, wage_pred, group = id, color = college)) + 
  geom_line() + 
  ylim(4, 9)

sim |> 
  ## subset 42 individuals for plotting
  filter(id %in% sample(1:N, 42)) |> 
  mutate(college = factor(college)) |> 
  ggplot(aes(t, wage, group = id, color = college)) + 
  geom_line() + 
  ylim(4, 9)

# linear regression doesn't know who you are

# more realistic ----------------------------------------------------------

# make individuals more stable, which you can by adding within and between
# person error terms

tau <- 0.2
sigma <- 0.05

## ICC

tau / (tau + sigma)

sim2 <- grid |> 
  ## add college
  mutate(college = rbinom(n(), 1, .2)) |> 
  ## add between person variance
  mutate(a = rnorm(n(), 0, tau)) |> 
  ## add within person variance,
  ## you must do this row-wise
  rowwise() |> 
  mutate(e = list(rnorm(T, 0, sigma))) 

## take a second to look at the resulting dataset,
## we usually don't deal with "nested" datasets

sim2 

sim2 <- sim2 |> 
  unnest(cols = c(e, t)) 

sim2 ## a varies between people, e varies within people

sim2 <- sim2 |> 
  mutate(wage_pred = 6.58 + college*0.353 ) |> 
  mutate(wage = 6.58 + college*0.353 + a + e)

sim2 |> 
  ## subset 42 individuals for plotting
  filter(id %in% sample(1:N, 42)) |> 
  mutate(college = factor(college)) |> 
  ggplot(aes(t, wage_pred, group = id, color = college)) + 
  geom_line() 

sim2 |> 
  ## subset 42 individuals for plotting
  filter(id %in% sample(1:N, 42)) |> 
  mutate(college = factor(college)) |> 
  ggplot(aes(t, wage, group = id, color = college)) + 
  geom_line() 

# moooorreeee realistic ---------------------------------------------------

sim3 <- sim2 |> 
  ## add time trend (e.g., by adding a slope for time),
  ## 0.1 is the slope we give time
  mutate(wage_pred = 6.58 + college*0.353 + (t - 1)*0.1 + a + e) |> 
  mutate(wage = wage_pred + e)

## Plots

sim3 |> 
  filter(id %in% sample(1:N, 100)) |> 
  mutate(college = factor(college)) |> 
  ggplot(aes(t, wage_pred, group = id, color = college)) + 
  geom_line(alpha = 1/2) 

sim3 |> 
  filter(id %in% sample(1:N, 40)) |> 
  mutate(college = factor(college)) |> 
  ggplot(aes(t, wage, group = id, color = college)) + 
  geom_line(alpha = 1/2) 

