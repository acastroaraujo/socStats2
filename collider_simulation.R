
library(tidyverse)
library(broom)
theme_set(theme_light(base_family = "Optima"))

set.seed(12345)
N <- 1e3
x <- rnorm(N, 0, 1)
y <- rnorm(N, 0, 1)
p <- plogis(2*x + 2*y)
z <- rbinom(N, 1, p)

d <- tibble(x, y, z)
d <- d |> mutate(z = ifelse(as.logical(z), "yes", "no"))

lm(y ~ x, data = d) |> summary()
lm(y ~ x + z, data = d) |> summary()

d |> 
  ggplot(aes(x, y)) + 
  geom_point(shape = 21, fill = "grey90") +
  geom_ribbon(stat = "smooth", method = "lm", color = "black", alpha = 1/4, linetype = "dashed", linewidth = 1/2, show.legend = FALSE) +
  geom_smooth(method = "lm", show.legend = FALSE, color = "black", linewidth = 1/2) +
  coord_fixed(ratio = 1) 

ggsave("images/collider0.png", device = "png", dpi = "print", width = 5, height = 5)

d |> 
  ggplot(aes(x, y, fill = z)) + 
  geom_point(alpha = 3/4, shape = 21) + 
  geom_ribbon(stat = "smooth", method = "lm", color = "black", alpha = 1/4, linetype = "dashed", linewidth = 1/2, show.legend = FALSE) +
  geom_smooth(method = "lm", show.legend = FALSE, color = "black", linewidth = 1/2) +
  coord_fixed(ratio = 1) +
  scale_fill_manual(values = c("grey90", "black")) +
  guides(fill = guide_legend(override.aes = list(size = 3, alpha = 1))) +
  theme(legend.position = c(.9, .9), legend.background = element_blank())

ggsave("images/collider4.png", device = "png", dpi = "print", width = 5, height = 5)

z <- x + y <= 0

d <- tibble(x, y, z)
d <- d |> mutate(z = ifelse(as.logical(z), "yes", "no"))


d |> 
  filter(z == "no") |> 
  ggplot(aes(x, y)) + 
  geom_point() + 
  geom_point(
    data = d |> filter(z == "yes"),
    color = "grey90", alpha = 1/2
  ) +
  geom_ribbon(stat = "smooth", method = "lm", color = "black", alpha = 1/4, linetype = "dashed", linewidth = 1/2, show.legend = FALSE) +
  geom_smooth(method = "lm", show.legend = FALSE, color = "red", linewidth = 1/2) +
  coord_fixed(ratio = 1) +
  scale_fill_manual(values = c("black", "grey90")) +
  guides(fill = guide_legend(override.aes = list(size = 3, alpha = 1))) + 
  geom_abline(intercept = 0, slope = -1)

ggsave("images/collider2.png", device = "png", dpi = "print", width = 5, height = 5)

z <- x <= 0 & y <= 0

d <- tibble(x, y, z)
d <- d |> mutate(z = ifelse(as.logical(z), "yes", "no"))

d |> 
  filter(z == "no") |> 
  ggplot(aes(x, y)) + 
  geom_point() + 
  geom_point(
    data = d |> filter(z == "yes"),
    color = "grey90", alpha = 1/2
  ) +
  geom_ribbon(stat = "smooth", method = "lm", color = "black", alpha = 1/4, linetype = "dashed", linewidth = 1/2, show.legend = FALSE) +
  geom_smooth(method = "lm", show.legend = FALSE, color = "red", linewidth = 1/2) +
  coord_fixed(ratio = 1) +
  scale_fill_manual(values = c("black", "grey90")) +
  guides(fill = guide_legend(override.aes = list(size = 3, alpha = 1))) + 
  geom_abline(intercept = 0, slope = -1)

ggsave("images/collider1.png", device = "png", dpi = "print", width = 5, height = 5)


z <- (x + y >= -1.5) & (x + y <= 1.5)
d <- tibble(x, y, z)
d <- d |> mutate(z = ifelse(as.logical(z), "yes", "no"))

d |> 
  filter(z == "yes") |> 
  ggplot(aes(x, y)) + 
  geom_point() + 
  geom_point(
    data = d |> filter(z == "no"),
    color = "grey90", alpha = 1/2
  ) +
  geom_ribbon(stat = "smooth", method = "lm", color = "black", alpha = 1/4, linetype = "dashed", linewidth = 1/2, show.legend = FALSE) +
  geom_smooth(method = "lm", show.legend = FALSE, color = "red", linewidth = 1/2) +
  coord_fixed(ratio = 1) +
  scale_fill_manual(values = c("black", "grey90")) +
  guides(fill = guide_legend(override.aes = list(size = 3, alpha = 1))) + 
  geom_abline(intercept = c(-1.5, 1.5), slope = -1)

ggsave("images/collider3.png", device = "png", dpi = "print", width = 5, height = 5)


