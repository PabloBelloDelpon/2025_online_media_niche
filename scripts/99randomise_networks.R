
###--- Libraries
library(fs)
library(tidyverse)
library(data.table)

###--- Ideology files
ideo_alters <- readRDS("20ideology_estimation/ideology_estimates_alters.RDS")
alters_file <- readRDS("14networks/alter_to_int.RDS")

###--- Ideology alters prepare
ideo_alters <- 
  alters_file |> 
  left_join(ideo_alters |>  
              rename(user_id = user)) |> 
  drop_na() |> 
  transmute(
    alter_int,
    q = ntile(ideology_new,100))

###--- Homophily
c = 1
alpha = .001
hom_tbl = expand.grid(ego_ntile = 1:100, alter_ntile = 1:100)

hom_tbl = hom_tbl %>% 
  mutate(dist = abs(ego_ntile - alter_ntile),
         prob = exp(-alpha*dist)) %>% 
  mutate(prob = prob/sum(prob))
  
hist(hom_tbl$dist)
hist(hom_tbl$prob)

hom_tbl %>% 
  ggplot(aes(dist, prob)) +
  geom_point()


###---
n_alters = 1e6
ideo_alters = 
  ideo_alters %>% 
  group_by(q) %>% 
  group_split()

network = list()
for(i in 1:100){
  
  df = hom_tbl %>% filter(ego_ntile == i)
  alters = sample(x = df$alter_ntile, size = n_alters, replace = TRUE, prob = df$prob)
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

saveRDS(network, paste0("14networks/random_network_h",alpha,".rds"))


