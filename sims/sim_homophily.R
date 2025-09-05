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


###--- Small example
###--- Homophily function
k = c(0, 2) # Parameter that regulates assymetry [0 = symetry]
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




###--- Test on Null Model
k = c(rep(0, 100)) # Parameter that regulates assymetry [0 = symetry]
h = c(0) # parameter that controls the amount of homophily {0 = uniform over distance}

params <- expand.grid(k = k, h = h)
dist <- c()
for(i in 1:nrow(params)) {
  
  sim <- homophily(k = params$k[i], h = params$h[i])
  dist[i] <- as.numeric(ks.test(obs$av_dist, sim$av_dist)$statistic)
  print(i)
  
}


hist(dist)
mean(dist)



###--- Test symmetric model with homophily
k = c(0) # Parameter that regulates assymetry [0 = symetry]
h = seq(0,4,.1) # parameter that controls the amount of homophily {0 = uniform over distance}

params <- expand.grid(k = k, h = h)
dist <- c()
for(i in 1:nrow(params)) {
  
  sim <- homophily(k = params$k[i], h = params$h[i])
  dist[i] <- as.numeric(ks.test(obs$av_dist, sim$av_dist)$statistic)
  print(i)
  
}

m1 <- 
  params |> 
  bind_cols("dist" = dist) 

m1 |> 
  ggplot(aes(h, dist)) +
  geom_point()


m1 |> filter(dist == min(dist))

###--- Fit on best fitting value
k = c(0) # Parameter that regulates assymetry [0 = symetry]
h = rep(0.8, 100) # parameter that controls the amount of homophily {0 = uniform over distance}

params <- expand.grid(k = k, h = h)
dist <- c()
for(i in 1:nrow(params)) {
  
  sim <- homophily(k = params$k[i], h = params$h[i])
  dist[i] <- as.numeric(ks.test(obs$av_dist, sim$av_dist)$statistic)
  print(i)
  
}

m1_fit <- 
  params |> 
  bind_cols("dist" = dist) 

mean(m1_fit$dist)



###--- Test asymmetric model with homophily
k = seq(0, 2, .1) # Parameter that regulates assymetry [0 = symetry]
h = seq(0,2,.1) # parameter that controls the amount of homophily {0 = uniform over distance}

params <- expand.grid(k = k, h = h)
dist <- c()
for(i in 1:nrow(params)) {
  
  sim <- homophily(k = params$k[i], h = params$h[i])
  dist[i] <- as.numeric(ks.test(obs$av_dist, sim$av_dist)$statistic)
  print(i)
  
}

m2 <- 
  params |> 
  bind_cols("dist" = dist) 

m2 |> 
  ggplot(aes(k, dist)) +
  geom_point()

m2 |> 
  ggplot(aes(k, h, fill = dist)) +
  geom_tile()

m2 |> filter(dist == min(dist))
