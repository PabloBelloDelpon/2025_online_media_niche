##---- Differential homophily ---###
library(tidyverse)
source("sims/99function_sim_homophily.R")

###--- Observed homophily
obs <- 
  readRDS("data/homophily/homophily.RDS") |> 
  select(ego_ntile = ego_ideology, av_dist) |> 
  mutate(condition = "Observed Network")

###--- Ego ideology
tbl <- 
  readRDS("data/ideology/ego_ideology.RDS") |> 
  select(user, ideology = ideology_new) |> 
  mutate(q = ntile(ideology, n = 100)) |> 
  group_by(q) |> 
  filter(ideology != Inf & ideology != -Inf) |> 
  summarise(ideo_q = mean(ideology, na.rm = TRUE))

###--- Homophily function
k = c(0, 1, 2) # Parameter that regulates assymetry [0 = symetry]
h = c(0, .5, 1) # parameter that controls the amount of homophily {0 = uniform over distance}

params <- expand.grid(k = k, h = h)
res <- list()
for(i in 1:nrow(params)) {
  res[[i]] <- homophily(k = params$k[i], h = params$h[i])
  print(i)
}

res |> 
  bind_rows() |> 
  mutate(condition = paste0("K=",k, ", H=", h)) |> 
  ggplot(aes(ego_ntile, av_dist, color = condition)) +
  geom_point() +
  geom_line()

