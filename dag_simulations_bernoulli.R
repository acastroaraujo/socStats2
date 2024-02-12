
library(tidyverse)
library(broom)
library(dagitty)


# Helpers -----------------------------------------------------------------

rbern <- function(n, p = 0.5) rbinom(n, size = 1, p)
coef_tbl <- function(obj) broom::tidy(obj) |> dplyr::select(term, estimate, std.error)


# Settings ----------------------------------------------------------------
set.seed(123)

N <- 1e5

# DAG ---------------------------------------------------------------------

dag <- dagitty("dag{
  U [unobserved];
  Y [outcome];
  T [exposure];
  U -> S; 
  U -> X;
  S -> T;
  X -> Y;
  T -> Y
}")

coordinates(dag) <- list(
  x =  c(T = 1, S = 1, U = 2, X = 3, Y = 3),
  y = -c(T = 1, S = 2, U = 3, X = 2, Y = 1)
)

plot(dag)

dagitty::impliedCovarianceMatrix(dag)


# Simulation 1 ------------------------------------------------------------

d <- tibble(
  U = rbern(N, 0.5),
  X = rbern(N, U*0.7 + (1 - U)*0.3), ## U increases P(X) by 0.4
  S = rbern(N, U*0.5 + (1 - U)*0.5),
  T = rbern(N, S*0.9 + (1 - S)*0.1),
  Y = 1*T + 1*X + rnorm(N, 0, 1)
)

# Regression

## No adjustment

m0 <- lm(Y ~ T, data = d)
coef_tbl(m0)

## Proper adjustment

m1 <- lm(Y ~ T + X, data = d)
coef_tbl(m1)

## Sub-optimal adjustment (note larger standard error!)
## Adjusting for S reduces the variance of T!
m2 <- lm(Y ~ T + S, data = d)
coef_tbl(m2)

# Simulation 2 ------------------------------------------------------------

d <- tibble(
  U = rbern(N, 0.5),
  X = rbern(N, U*0.7 + (1 - U)*0.3), ## U increases P(X) by 0.4
  S = rbern(N, U*0.5 + (1 - U)*0.5),
  T = rbern(N, S*0.9 + (1 - S)*0.1),
  Z = 0.5*T + rnorm(N, 0, 1),
  Y = 1*T + 2*X + rnorm(N, 0, 1)
)

# Regression

## No adjustment

m0 <- lm(Y ~ T, data = d)
coef_tbl(m0)

m1 <- lm(Y ~ T + Z, data = d)
coef_tbl(m1)

## Proper adjustment

m2 <- lm(Y ~ T + X + Z, data = d)
coef_tbl(m2)

m3 <- lm(Y ~ T + X, data = d)
coef_tbl(m3)

## Sub-optimal adjustment (note larger standard error!)
## Adjusting for S reduces the variance of T!
m4 <- lm(Y ~ T + Z + S, data = d)
coef_tbl(m4)

# More daggity ------------------------------------------------------------

# https://www.quantargo.com/help/r/latest/packages/dagitty/0.3-1/simulateSEM

## Simulate data with pre-defined path coefficients of -.6
g <- dagitty('dag{z -> x [beta=-.6] x <- y [beta=-.6] }')
impliedCovarianceMatrix(g)
x <- simulateSEM(g, N = 1e5) 
cov(x)
