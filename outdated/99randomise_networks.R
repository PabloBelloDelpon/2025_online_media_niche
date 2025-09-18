
###--- Libraries
library(fs)
library(tidyverse)
library(data.table)

###--- Ideology files
ideo_alters <- readRDS("data/ideology/alter_ideology.RDS")
ideo_egos <- readRDS("data/ideology/ego_ideology.RDS") 
alters_file <- readRDS("data/ids/alter_to_int.RDS")

###--- Means
ego_means <-
  ideo_egos |> 
  select(user, ideology = ideology_new) |> 
  mutate(ego_q = ntile(ideology, n = 100)) |> 
  group_by(ego_q) |> 
  filter(ideology != Inf & ideology != -Inf) |> 
  summarise(ego_ideo = mean(ideology, na.rm = TRUE))


alter_means <-
  ideo_alters |> 
  select(user, ideology = ideology_new) |> 
  mutate(alter_q = ntile(ideology, n = 100)) |> 
  group_by(alter_q) |> 
  filter(ideology != Inf & ideology != -Inf) |> 
  summarise(alter_ideo = mean(ideology, na.rm = TRUE))

###--- Ideology alters prepare
ideo_alters <- 
  alters_file |> 
  left_join(ideo_alters |>  
              rename(user_id = user)) |> 
  drop_na() |> 
  transmute(
    alter_int,
    alter_ideo = ideology_new,
    alter_q = ntile(ideology_new,100))

###--- Homophily
c <- 1
beta <- 1
hom_tbl <- expand.grid(ego_q = 1:100, alter_q = 1:100)

hom_tbl <- 
  hom_tbl %>% 
  left_join(ego_means) |> 
  left_join(alter_means) |> 
  mutate(dist = abs(ego_ideo - alter_ideo),
         prob = exp(-beta*dist)) %>% 
  mutate(prob = prob/sum(prob))
  
hist(hom_tbl$dist)
hist(hom_tbl$prob)

hom_tbl %>% 
  ggplot(aes(dist, prob)) +
  geom_point()

hom_tbl |> 
  filter(ego_q %in% c(1, 10, 50, 90, 99)) |> 
  mutate(ego_q = as.character(ego_q)) |> 
  ggplot(aes(alter_q, prob, color = ego_q)) +
  geom_line()

###---
n_alters = 1e6
ideo_alters = 
  ideo_alters %>% 
  group_by(alter_q) %>% 
  group_split()

network = list()
for(i in 1:100){
  
  df = hom_tbl %>% filter(ego_q == i)
  alters = sample(x = df$alter_q, size = n_alters, replace = TRUE, prob = df$prob)
  alters = tibble(alter_ntile = alters) %>% count(alter_ntile)
  
  new_network = list()
  for(j in 1:nrow(alters)) {
    
    alter_ntile = alters[j,]$alter_ntile
    n_alters_ntile = alters[j,]$n
    alter_int = sample(x = ideo_alters[[alter_ntile]]$alter_int, size = n_alters_ntile, replace = TRUE)
    new_network[[j]] = tibble(alter_ntile, alter_int)

  }
  
  
  network[[i]] =
    bind_rows(new_network) %>% 
    mutate(ego_ntile = i)
  
  print(i)
}


network = bind_rows(network)

saveRDS(network, paste0("data/network/randomized_network_h",beta,".rds"))


