###--- Libraries
library(tidyverse)
library(patchwork)
library(hrbrthemes)
library(MetBrewer)
source("scripts/99function_sim_homophily.R")
set.seed(061295)

###---- Graphic theme
colors <- met.brewer("Java", n = 3)
theme_set(theme_ipsum(axis_title_size = 15, base_size = 15))

###--- Observed homophily
obs <-
  readRDS("data/observed_homophily.rds") |>
  select(ego_ntile = ego_ideology, av_dist) |>
  mutate(condition = "Observed Network") |>
  drop_na()

###--- Ego ideology
tbl <-
  readRDS("data/ego_ideology.rds") |>
  select(ego_int, ideology = ideo_est) |>
  mutate(q = ntile(ideology, n = 100)) |>
  group_by(q) |>
  filter(ideology != Inf & ideology != -Inf) |>
  summarise(ideo_q = mean(ideology, na.rm = TRUE))


###--- Test on Null Model
k = c(rep(0, 100)) # Parameter that regulates assymetry [0 = symetry]
h = c(0) # parameter controlling the amount of homophily {0 = uniform over distance}

params <- expand.grid(k = k, h = h)
dist_null <- c()
dist_null2 <- c()
ex_null <- list()

for (i in 1:nrow(params)) {
  sim <- homophily(k = params$k[i], h = params$h[i])
  dist_null[i] <- sum(abs(obs$av_dist - sim$av_dist)) / 100
  dist_null2[i] <- as.numeric(ks.test(obs$av_dist, sim$av_dist)$statistic)
  ex_null[[i]] <- sim
  if (i %% 10 == 0) print(i)
}

ex_null <-
  ex_null[[which.min(dist_null)]] |>
  mutate(model = "Null model")


###---- Symmetric Model
k = c(0) # Parameter that regulates assymetry [0 = symetry]
h = seq(0, 2, .1) # parameter controlling the amount of homophily {0 = uniform over distance}

params <- expand.grid(k = k, h = h)
dist_sym <- c()
dist_sym2 <- c()

for (i in 1:nrow(params)) {
  sim <- homophily(k = params$k[i], h = params$h[i])
  dist_sym[i] <- sum(abs(obs$av_dist - sim$av_dist)) / 100
  dist_sym2[i] <- as.numeric(ks.test(obs$av_dist, sim$av_dist)$statistic)
  if (i %% 10 == 0) print(i)
}

m1 <-
  params |>
  bind_cols("dist" = dist_sym)

###--- Now run it i times for the best fitting parameter
k = 0
h = rep(m1 |> filter(dist == min(dist)) |> pull(h), 100)

params <- expand.grid(k = k, h = h)
dist_sym <- c()
ex_sym <- list()

for (i in 1:nrow(params)) {
  sim <- homophily(k = 0, h = params$h[i])
  dist_sym[i] <- sum(abs(obs$av_dist - sim$av_dist)) / 100
  ex_sym[[i]] <- sim
  if (i %% 10 == 0) print(i)
}

ex_sym <-
  ex_sym[[which.min(dist_sym)]] |>
  mutate(model = "Symmetric model")


###--- Asymmetric model
k = seq(0, 1, .1) # Parameter that regulates assymetry [0 = symetry]
h = seq(0, 1, .1) # parameter that controls the amount of homophily {0 = uniform over distance}

params <- expand.grid(k = k, h = h)
dist_asym <- c()

for (i in 1:nrow(params)) {
  sim <- homophily(k = params$k[i], h = params$h[i])
  dist_asym[i] <- sum(abs(obs$av_dist - sim$av_dist)) / 100
  if (i %% 10 == 0) print(i)
}

m2 <-
  params |>
  bind_cols("dist" = dist_asym)


###--- Now run it i times for the best fitting parameter
k <- rep(m2 |> filter(dist == min(dist)) |> pull(k), 100)
h <- m2 |> filter(dist == min(dist)) |> pull(h)

params <- expand.grid(k = k, h = h)
dist_asym <- c()
ex_asym <- list()

for (i in 1:nrow(params)) {
  sim <- homophily(k = params$k[i], h = params$h[i])
  dist_asym[i] <- sum(abs(obs$av_dist - sim$av_dist)) / 100
  ex_asym[[i]] <- sim
  if (i %% 10 == 0) print(i)
}

ex_asym <- ex_asym[[which.min(dist_asym)]] |> mutate(model = "Asymmetric model")


###--- Model fit
p1 <-
  tibble(
    "Null model" = dist_null,
    "Equal homophily model" = dist_sym,
    "Unequal homophily model" = dist_asym
  ) |>
  pivot_longer(cols = 1:3, names_to = "model", values_to = "dist") |>
  group_by(model) |>
  summarise(mean_dist = mean(dist), se = sd(dist)) |>
  mutate(model = fct_reorder(model, mean_dist)) |>
  ggplot(aes(mean_dist, model)) +
  geom_point(size = 2) +
  geom_linerange(aes(xmin = mean_dist - se, xmax = mean_dist + se)) +
  labs(x = "Model fit", y = "")


###----
tbl2 <-
  bind_rows(
    ex_null,
    ex_sym |> mutate(model = "Equal homophily model"),
    ex_asym |> mutate(model = "Differential homophily model"),
    obs |> mutate(model = "Observed network")
  )


###--- Figure A2
p2 <-
  tbl2 |>
  mutate(
    model = factor(
      model,
      levels = c(
        "Observed network",
        "Null model",
        "Equal homophily model",
        "Differential homophily model"
      )
    )
  ) |>
  ggplot(aes(ego_ntile, av_dist, color = model)) +
  geom_line(linewidth = 1) +
  labs(
    color = "",
    y = "Mean ego-alter distance",
    x = "Ego ideology percentile"
  ) +
  scale_color_manual(values = c("grey10", colors)) +
  theme(legend.position = "top")


ggsave("output/figure_A2.png", dpi = 400, width = 10, height = 5)
