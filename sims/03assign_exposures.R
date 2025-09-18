
###--- Libraries
library(tidyverse)
library(glue)
library(fs)
library(data.table)
library(doParallel)
library(foreach)

###--- Folders
exposures_folder <- "/Users/pb216/Desktop/projects/social_influence/17clean_adopt_treat_tables_complete2_summarized/"
output_folder <- "/Users/pb216/Desktop/projects/social_influence/29randomized_networks_exposures2/"
temp <- "/Users/pb216/Desktop/projects/social_influence/temp/"
networks_folder <- "/Volumes/Samsung_T5/28randomized_networks2/"

###--- Load the data
alters_file <- readRDS("data/ids/alter_to_int.RDS")
ideo_alters <- readRDS("data/ideology/alter_ideology.RDS") |> filter(ideology_new != Inf & ideology_new != -Inf)
domain_ideology <-
  readRDS("data/ideology/domain_ideology.RDS") |>  
  select(domain, domain_ideo = mean) %>% 
  mutate(domain_ideo_tile = ntile(domain_ideo, 100))


###--- Ideology alters prepare
ideo_alters <- 
  alters_file |> 
  left_join(ideo_alters |>  
              rename(user_id = user)) |> 
  drop_na() |> 
  transmute(
    alter_int,
    q = ntile(ideology_new,100))

rm(alters_file)



###--- List randomized network files
network_files <- 
  dir_ls(networks_folder) |> 
  as_tibble() |> 
  mutate(network_id = str_remove(value, networks_folder), 
         network_id = as.integer(str_extract(network_id, "[0-9]+"))) |> 
  arrange(network_id)

###--- List exposure files
exposure_files <- 
  dir_ls(exposures_folder) |> 
  as_tibble() |> 
  mutate(domain = str_remove(value, exposures_folder),
         domain = str_remove(domain, ".RDS"))



#################--- MAIN ---#################
cl <- makeCluster(parallel::detectCores() - 1)
registerDoParallel(cl)


foreach(i = 1:nrow(network_files), .packages = c("dplyr", "tibble", "data.table", "tidyr")) %dopar% {
  
random_net <- readRDS(network_files[i,]$value)
parameters <- random_net$parameters
random_net <- random_net$network |> as.data.table()
setkey(random_net, alter_int)


exposures <- list()

for(j in 1:nrow(exposure_files)) {
  
  domain <-  exposure_files[j,]$domain

  tbl <- readRDS(exposure_files[j,]$value)
  alters <- tbl$alter_int
  ego_net <- random_net[.(alters)] |> drop_na()
  
  exposures[[j]] <- 
    ego_net %>% 
    left_join(tbl, multiple = "first", by = join_by(alter_int)) %>% 
    select(- alter_int) %>% 
    mutate(domain = domain) %>% 
    left_join(domain_ideology, by = join_by(domain))
  

}

exposures <- bind_rows(exposures)
result <- list("exposures" = exposures,
               "parameters" = parameters)

saveRDS(result, paste0(output_folder, "network", i, ".rds"))
}


