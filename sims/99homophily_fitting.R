
###--- Libraries
library(fs)
library(tidyverse)
library(data.table)
library(hrbrthemes)
library(patchwork)
###--- Load the data

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


###--- Define the homophily function
c = 1
alpha = .8
hom_tbl = expand.grid(ego_ntile = 1:100, alter_ntile = 1:100)
hom_tbl <- 
  hom_tbl |> 
  left_join(tbl |> select(ego_ntile = q, ego_ideo = ideo_q)) |> 
  left_join(tbl |> select(alter_ntile = q, alter_ideo = ideo_q)) |> 
  mutate(dist = abs(ego_ideo - alter_ideo),
         prob = exp(-alpha*dist)) %>% 
  mutate(prob = prob/sum(prob))

hom_tbl %>%
  ggplot(aes(dist, prob)) +
  geom_point()

n_alters <-  1e5
mean_diff <- c()
for(i in 1:100){
  
  df = hom_tbl %>% filter(ego_ntile == i)
  ego_ideo <- unique(df$ego_ideo)
  alter_ideo = sample(x = df$alter_ntile, size = n_alters, replace = TRUE, prob = df$prob)
  #abs_diff <-  abs(ego_ideo - alter_ideo)
  abs_diff <-  abs(i - alter_ideo)
  
  mean_diff[i] <-  mean(abs_diff)
  print(mean(abs_diff))
}


hom = tibble(ego_ntile = 1:100, av_dist = mean_diff)

###--- Plot
p1 <-  
  hom |>
  mutate(condition = "Simulated Network") |> 
  bind_rows(obs) |> 
  ggplot(aes(ego_ntile, av_dist, color = condition)) +
  geom_point() +
  labs(x = "Ego Ideology Percentile",
       y = "Average Ego - Alter Distance (percentiles)",
       title = "Without differential homophily",
       color = "") +
  theme_ipsum(base_size = 15, axis_title_size = 17) +
  theme(legend.position = "top")

#######################################################
###---- Differential homophily ---###
alpha = exp(1:100*.1)
###---
alpha <- ((1:100-1)/99)^3
###---
alpha <- tibble(ego_ntile = 1:100, alpha) 
alpha |> 
  ggplot(aes(ego_ntile, -alpha)) + geom_point()


hom_tbl = expand.grid(ego_ntile = 1:100, alter_ntile = 1:100)
hom_tbl <- 
  hom_tbl |> 
  left_join(alpha) |> 
  left_join(tbl |> select(ego_ntile = q, ego_ideo = ideo_q)) |> 
  left_join(tbl |> select(alter_ntile = q, alter_ideo = ideo_q)) |> 
  mutate(dist = abs(ego_ideo - alter_ideo),
         prob = exp(-alpha*dist)) %>% 
  group_by(ego_ntile) |> 
  mutate(prob = prob/sum(prob))

hom_tbl |> filter(dist == 0)
hom_tbl %>%
  filter(ego_ntile %in% seq(10,100,20)) |> 
  ggplot(aes(dist, prob, color = ego_ntile, group = ego_ntile)) +
  geom_point() +
  geom_line() +
  ylim(c(0,.1))

n_alters <-  1e5
mean_diff <- c()
for(i in 1:100){
  
  df = hom_tbl %>% filter(ego_ntile == i)
  ego_ideo <- unique(df$ego_ideo)
  alter_ideo = sample(x = df$alter_ntile, size = n_alters, replace = TRUE, prob = df$prob)
  #abs_diff <-  abs(ego_ideo - alter_ideo)
  abs_diff <-  abs(i - alter_ideo)
  
  mean_diff[i] <-  mean(abs_diff)
  print(mean(abs_diff))
}


hom = tibble(ego_ntile = 1:100, av_dist = mean_diff)

###--- Plot
p2 <- 
  hom |>
  mutate(condition = "Simulated Network") |> 
  bind_rows(obs) |> 
  ggplot(aes(ego_ntile, av_dist, color = condition)) +
  geom_point() +
  labs(x = "Ego Ideology Percentile",
       y = "Average Ego - Alter Distance (percentiles)",
       title = "With differential homophily",
       color = "") +
  theme_ipsum(base_size = 15, axis_title_size = 17) +
  theme(legend.position = "top")

(plot <- p1 + p2)
ggsave(plot = plot, filename = "output/figure_A3.png", dpi = 400)
