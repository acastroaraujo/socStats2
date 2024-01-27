

hod_simulation <- function(
    N = 1e3,
    rho = 0.5,
    Bt = 1,
    Bx = 1
) {
  
  stopifnot(dplyr::between(rho, -1, 1))
  stopifnot(N > 0)
  
  Mu <- c(s = 1, x = 1)
  sigmas <- c(s = 1, x = 1)
  Rho <- rbind(c(1, rho), c(rho, 1))
  Sigma <- diag(sigmas) %*% Rho %*% diag(sigmas)
  d <- tibble::as_tibble(mvtnorm::rmvnorm(N, Mu, Sigma))
  ## approximately 50% of sample gets treatment with this hack
  d$t <- rbinom(N, 1, pnorm(d$s, Mu[["s"]], sigmas[["s"]]))
  
  e <- rnorm(N, 0, 5)
  
  d$y0 <- d$x*Bx + e
  d$y1 <- Bt + d$x*Bx + e
  d$y <- ifelse(as.logical(d$t), d$y1, d$y0)
  
  std_error <- sqrt(2*(5^2 + Bx^2) / (N/2))
  pwr <- pnorm(std_error*qnorm(0.975), Bt, std_error, lower.tail = FALSE)
  message("Standard Error ~ ", round(std_error, 3))
  message("Power ~ ", round(pwr, 3))
  
  out <- d[, c("x", "y0", "y1", "t", "y")]
  
  structure(out, class = c("simulation", class(out)), pars = list(N = N, Bt = Bt, Bx = Bx, rho = rho))
  
}

# hod_coverage_and_significance <- function(d, conf_interval = 0.95, S = 1e3) {
#   
#   stopifnot(inherits(d, "simulation"))
#   pars <- attr(d, "pars")
#   
#   out <- replicate(S, {
#     d$t <- sample(d$t)
#     d$y <- ifelse(as.logical(d$t), d$y1, d$y0)
#     ols <- lm(y ~ t, data = d) 
#     confint(ols, "t", level = conf_interval)
#     
#   }, simplify = TRUE)  
#   
#   out <- t(out) 
#   colnames(out) <- c("lower", "upper")
#   out <- tibble::as_tibble(out)  
#   
#   out <- out |> 
#     tibble::rowid_to_column("s") |> 
#     dplyr::mutate(cov = pars$Bt > lower & pars$Bt < upper) |> 
#     dplyr::mutate(sig = !(lower < 0 & upper > 0)) 
#   
#   structure(out, class = c("simulation", class(out)), pars = pars)
#   
# }

# hod_cov_sig_plot <- function(sim) {
#  
#  stopifnot(inherits(d, "simulation"))
# pars <- attr(d, "pars")
  
#  sim |> 
#    dplyr::sample_n(size = 500) |> 
#    tibble::rowid_to_column("id") |> 
#    ggplot2::ggplot(aes(y = id, color = sig)) + 
#    ggplot2::geom_segment(ggplot2::aes(x = lower, xend = upper, yend = id), linewidth = 1/5) +
#    ggplot2::geom_vline(xintercept = pars$Bt, linetype = "dashed")
  
# }
