
###--- Libraries
library(fs)
library(tidyverse)
library(data.table)
library(foreach)
library(doParallel)

###--- Ideology files
output_folder <- "/Volumes/Samsung_T5/28randomized_networks2/"
ideo_alters <- readRDS("data/ideology/alter_ideology.RDS") |> filter(ideology_new != Inf & ideology_new != -Inf)
ideo_egos <- readRDS("data/ideology/ego_ideology.RDS") |> filter(ideology_new != Inf & ideology_new != -Inf)
alters_file <- readRDS("data/ids/alter_to_int.RDS")


#################--- PREPARE THE DATA ---#################

###--- Means
ego_means <-
  ideo_egos |> 
  select(user, ideology = ideology_new) |> 
  mutate(ego_ntile = ntile(ideology, n = 100)) |> 
  group_by(ego_ntile) |> 
  summarise(ego_ideo = mean(ideology, na.rm = TRUE))


alter_means <-
  ideo_alters |> 
  select(user, ideology = ideology_new) |> 
  mutate(alter_ntile = ntile(ideology, n = 100)) |> 
  group_by(alter_ntile) |> 
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
    alter_ntile = ntile(ideology_new,100))

ideo_alters <-  
  ideo_alters %>% 
  group_by(alter_ntile) %>% 
  group_split()

#################---DATA FRAME FOR HOMOPHILY VALUES ---#################
hom_tbl <- expand.grid(ego_ntile = 1:100, alter_ntile = 1:100)
hom_tbl <- 
  hom_tbl |> 
  left_join(ego_means, by = join_by(ego_ntile)) |> 
  left_join(alter_means, by = join_by(alter_ntile)) |> 
  mutate(dist = abs(ego_ideo - alter_ideo))


#################--- PARAMETER SPACE ---#################
n_alters <- 1e6

###--- Null model 
k <- 0 # Parameter that regulates assymetry [0 = symetry]
h <- 0 # parameter that controls the amount of homophily {0 = uniform over distance}
params_null <- expand.grid(k = k, h = h, model = "null")

###--- Symmetric model
k <- c(0) # Parameter that regulates assymetry [0 = symetry]
h <- seq(0, 4, .1) # parameter that controls the amount of homophily {0 = uniform over
params_sym <- expand.grid(k = k, h = h, model = "symmetric")

###--- Asymmetric model 
k <- seq(0, 2, .1) # Parameter that regulates assymetry [0 = symetry]
h <- seq(0, 4, .1) # parameter that controls the amount of homophily {0 = uniform over distance}
params_asym <- expand.grid(k = k, h = h, model = "asymmetric")

###---- All parameters
params <-  bind_rows(params_null, params_sym, params_asym)


#################--- MAIN ---#################
done <- 
  output_folder |> 
  dir_ls() |> 
  as_tibble() |> 
  mutate(network_id = str_remove(value, output_folder),
         network_id = as.integer(str_extract(network_id, "[0-9]+"))) |> 
  arrange(desc(network_id)) |> 
  filter(network_id == max(network_id)) |> 
  pull(network_id)

cl <- makeCluster(parallel::detectCores() - 1)
registerDoParallel(cl)


foreach(i = (done + 1):nrow(params), .packages = c("dplyr", "tibble")) %dopar% {
  
  k <- params$k[i]
  h <- params$h[i]
  model <- params$model[i]
  ###--- Homophily function
  beta <- (1:100/100)^k # beta sub i
  beta <- tibble(ego_ntile = 1:100, beta)
  
  hom <- 
    hom_tbl |> 
    left_join(beta, by = join_by(ego_ntile)) |> 
    mutate(dist = abs(ego_ideo - alter_ideo),
           prob = exp(-beta*dist*h)) 
  

  network = list()
  
  for(j in 1:100){
    
    df <- hom %>% filter(ego_ntile == j)
    alters <- sample(x = df$alter_ntile, size = n_alters, replace = TRUE, prob = df$prob)
    alters <- tibble(alter_ntile = alters) |>  count(alter_ntile)
    
    new_network <- list()
    for(p in 1:nrow(alters)) {
      
      alter_ntile <- alters[p,]$alter_ntile
      n_alters_ntile <- alters[p,]$n
      alter_int <- sample(x = ideo_alters[[alter_ntile]]$alter_int, size = n_alters_ntile, replace = TRUE)
      new_network[[p]] <- tibble(alter_ntile, alter_int)
      
    }
  
  
  network[[j]] <- 
    bind_rows(new_network) %>% 
    mutate(ego_ntile = j)
  
  }
  
  network <- bind_rows(network)
  parameters <- tibble(model, k, h, n_alters)
  network <- list("network" = network, 
                  "parameters" = parameters)
  saveRDS(network, paste0(output_folder, "netowrk_", i, ".rds"))
  
}



