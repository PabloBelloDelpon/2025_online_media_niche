###--- Libraries
library(fs)
library(tidyverse)
library(data.table)
library(foreach)
library(doParallel)

###--- Ideology files
output_folder <- "simulations/randomized_networks/"
if (dir.exists(output_folder) == FALSE) {
  dir.create(output_folder, recursive = TRUE)
}

###--- Ego ideology data
ideo_egos <-
  readRDS("data/ego_ideology.RDS") |>
  filter(ideo_est != Inf & ideo_est != -Inf) |>
  mutate(ego_ntile = ntile(ideo_est, n = 100))

###--- Alter ideology data
ideo_alters <-
  readRDS("data/alter_ideology.RDS") |>
  filter(ideo_est != Inf & ideo_est != -Inf) |>
  mutate(alter_ntile = ntile(ideo_est, n = 100))


###--- Ego percentile ideology means
ego_means <-
  ideo_egos |>
  group_by(ego_ntile) |>
  summarise(ego_ideo = mean(ideo_est, na.rm = TRUE))

###--- Alter percentile ideology means
alter_means <-
  ideo_alters |>
  group_by(alter_ntile) |>
  summarise(alter_ideo = mean(ideo_est, na.rm = TRUE))

###--- Ideology alters prepare
ideo_alters <-
  ideo_alters |>
  group_by(alter_ntile) |>
  group_split()


###--- Ideology distance between percentile means of egos and alters
hom_tbl <-
  expand.grid(ego_ntile = 1:100, alter_ntile = 1:100) |>
  left_join(ego_means, by = join_by(ego_ntile)) |>
  left_join(alter_means, by = join_by(alter_ntile)) |>
  mutate(dist = abs(ego_ideo - alter_ideo))


###--- Define the parameter space
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

###--- All parameters
params <- bind_rows(params_null, params_sym, params_asym)


###--- Check how many simulations are done (will throw warning if folder is empty)
done <-
  output_folder |>
  dir_ls() |>
  as_tibble() |>
  mutate(
    network_id = str_remove(value, output_folder),
    network_id = as.integer(str_extract(network_id, "[0-9]+"))
  ) |>
  arrange(desc(network_id)) |>
  filter(network_id == max(network_id)) |>
  pull(network_id)


###--- Prepare cluster
cl <- makeCluster(parallel::detectCores() - 1)
registerDoParallel(cl)

###--- Simulation
foreach(i = (done + 1):nrow(params), .packages = c("dplyr", "tibble")) %dopar%
  {
    k <- params$k[i]
    h <- params$h[i]
    model <- params$model[i]

    ###--- Homophily function
    beta <- (1:100 / 100)^k # beta sub i
    beta <- tibble(ego_ntile = 1:100, beta)

    ###--- Compute tie likelihoods
    hom <-
      hom_tbl |>
      left_join(beta, by = join_by(ego_ntile)) |>
      mutate(dist = abs(ego_ideo - alter_ideo), prob = exp(-beta * dist * h))

    ###--- Store output
    network <- list()

    ###--- Loop through ego percentiles
    for (j in 1:100) {
      df <- hom |> filter(ego_ntile == j)

      ###--- Sample number of alters of each percentile
      alters <- sample(
        x = df$alter_ntile,
        size = n_alters,
        replace = TRUE,
        prob = df$prob
      )
      alters <- tibble(alter_ntile = alters) |> count(alter_ntile)

      ###--- Store simulated ties
      new_network <- list()

      ###--- Iterate through alter percentiles
      for (p in 1:nrow(alters)) {
        alter_ntile <- alters[p, ]$alter_ntile
        n_alters_ntile <- alters[p, ]$n

        ###--- Sample actual alters
        alter_int <- sample(
          x = ideo_alters[[alter_ntile]]$alter_int,
          size = n_alters_ntile,
          replace = TRUE
        )
        new_network[[p]] <- tibble(alter_ntile, alter_int)
      }

      ###--- Put together complete simulated network
      network[[j]] <-
        bind_rows(new_network) %>%
        mutate(ego_ntile = j)
    }

    ###--- Store results
    network <- bind_rows(network)
    parameters <- tibble(model, k, h, n_alters)
    network <- list("network" = network, "parameters" = parameters)
    saveRDS(network, paste0(output_folder, "netowrk_", i, ".rds"))
  }
