###--- Calculate homophily in the network
library(tidyverse)


###--- Load data
network <- readRDS("data/network/observed_network.RDS")
ideo_alters <- readRDS("data/ideology/alter_ideology.RDS")
ideo_egos <- readRDS("data/ideology/ego_ideology.RDS")
ego_ids <- readRDS("data/ids/ego_to_int.RDS")
alter_ids <- readRDS("data/ids/alter_to_int.RDS")

###--- Prepare ego data
egos <- 
  ideo_egos |> 
  mutate(ego_ideology = ntile(ideology_new, 100)) |> 
  select(user_id = user, ego_ideology) |> 
  left_join(ego_ids) |> 
  select(ego_int, ego_ideology)

###--- Prepare alter data
alters <- 
  ideo_alters |> 
  mutate(alter_ideology = ntile(ideology_new, 100)) |> 
  select(user_id = user, alter_ideology) |> 
  left_join(alter_ids) |> 
  select(alter_int, alter_ideology)

###--- Merge with network data
network <- 
  network |> 
  left_join(egos) |> 
  left_join(alters)

###--- Compute
hom <- 
  network |> 
  mutate(abs_distance = abs(ego_ideology - alter_ideology)) |> 
  group_by(ego_ideology) |> 
  summarise(av_dist = mean(abs_distance, na.rm = TRUE),
            sd_dist = sd(abs_distance, na.rm = TRUE))

saveRDS(hom, "data/network/observed_homophily.rds")



###--- Randomize
rn <- sample(network$alter_ideology, size = nrow(network), replace = FALSE)

network_random <- tibble(ego_ideology = network$ego_ideology, 
                         alter_ideology = rn) 

network_random <-
  network_random |> 
  mutate(abs_distance = abs(ego_ideology - alter_ideology)) |> 
  group_by(ego_ideology) |> 
  summarise(av_dist = mean(abs_distance, na.rm = TRUE),
            sd_dist = sd(abs_distance, na.rm = TRUE))

network_random |> 
  ggplot(aes(ego_ideology, av_dist)) +
  geom_point()

saveRDS(network_random, "data/homophily/homophily_random.RDS")

