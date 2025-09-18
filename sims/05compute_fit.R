library(tidyverse)
library(fs)
library(foreach)
library(doParallel)

###--- Observed Exposures
tbl_obs <-
  readRDS("data/main/observed_exposures.rds") |> 
  rename(perc_obs = perc)
domains <- unique(tbl_obs$domain)
total_exposures <- 
  tbl_obs |> 
  distinct(domain, total_domain_exposures) |> 
  mutate(domain_weight = total_domain_exposures/sum(total_domain_exposures)) |> 
  select(domain, domain_weight)

###--- Input files
input_folder <- "/Users/pb216/Desktop/projects/social_influence/29randomized_networks_exposures2/"
input_files <- dir_ls(input_folder)

###--- Parallel setup
n_cores <- parallel::detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

results <- foreach(i = 1:length(input_files), 
                   .combine = bind_rows, 
                   .packages = c("tidyverse", "fs")) %dopar% {
                     

  data <- readRDS(input_files[i])
  tbl_sim <- data$exposures
  params <- data$parameters
  network_id <- str_remove(input_files[i], input_folder)
  network_id <- str_remove(network_id, ".rds")
  
  
  tbl_sim <- 
    tbl_sim |> 
    group_by(domain, ego_ntile) %>% 
    summarise(n_domain_exposures = sum(n_exposures), .groups = "drop_last") |> 
    group_by(ego_ntile) %>% 
    mutate(total_exposures = sum(n_domain_exposures)) |> 
    ungroup() |> 
    mutate(perc_sim = n_domain_exposures / total_exposures * 100) |> 
    filter(domain %in% domains)
  
  tbl <- 
    tbl_sim |> 
    select(domain, ego_ntile, perc_sim) |> 
    full_join(tbl_obs, by = c("domain", "ego_ntile")) |> 
    mutate(
      perc_sim = replace_na(perc_sim, 0),
      perc_obs = replace_na(perc_obs, 0),
      diff = abs(perc_sim - perc_obs)
    )
  
  ###--- Visual checks
  # tbl |> 
  #   ggplot(aes(diff)) +
  #   geom_histogram() +
  #   facet_wrap(~ domain, scales = "free")
  # 
  # 
  # tbl |> 
  #   pivot_longer(cols = c(perc_obs, perc_sim), names_to = "type", values_to = "perc") |> 
  #   ggplot(aes(ego_ntile, perc, color = type)) +
  #   geom_line() +
  #   facet_wrap(~ domain, scale = "free")
  
  out <- 
    tbl |> 
    group_by(domain) |> 
    summarise(mean_diff = mean(diff), 
              sd_diff = sd(diff),
              .groups = "drop") |> 
    mutate(model = params$model,
           k = params$k,
           h = params$h,
           n_alters = params$n_alters,
           network_id)
  
  out

}

###--- Stop cluster
stopCluster(cl)

###--- Combine with weights
results <- results |> 
  left_join(total_exposures)

saveRDS(results, "data/main/simulations_fit2.rds")
