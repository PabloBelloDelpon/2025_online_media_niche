###--- Libraries
library(patchwork)
library(tidyverse)
library(hrbrthemes)


###--- Graphic options
lib_colors = c("#A7C7E7", "#1E3A5F")
cons_colors = c("#FF9999", "#990000")

theme_set(
  theme_ipsum(
    base_size = 22,
    strip_text_size = 20,
    axis_title_size = 20,
    subtitle_size = 18
  ) +
    theme(
      legend.position = "none",
      plot.background = element_rect(color = "white")
    )
)


####--- Domain Selection
domains <- readRDS("data/ideology/extreme_domains.rds")

###---Simulated: Null and Symmetric model
best_fit_sims <- readRDS(
  "/Users/pb216/Desktop/projects/2025_online_media_niche/data/main/simulations_fit.rds"
)


# best_fit_sims <-
#   best_fit_sims |>
#   group_by(model, network_id, k , h) |>
#   summarise(fit = mean(mean_diff), .groups = "drop") |>
#   group_by(model) |>
#   filter(fit == min(fit)) |>
#   filter(model != "asymmetric") |>
#   mutate(file =  paste0("/Users/pb216/Desktop/projects/social_influence/29randomized_networks_exposures2/", network_id, ".rds"))

best_fit_sims <-
  best_fit_sims |>
  filter(network_id %in% c("network22", "network1")) |>
  distinct(network_id, model) |>
  filter(model != "asymmetric") |>
  mutate(
    file = paste0(
      "/Users/pb216/Desktop/projects/social_influence/29randomized_networks_exposures2/",
      network_id,
      ".rds"
    )
  )

read_net <- function(x) {
  data <- readRDS(x)
  tbl_sim <- data$exposures

  tbl_sim |>
    group_by(domain, ego_ntile) %>%
    summarise(n_domain_exposures = sum(n_exposures), .groups = "drop_last") |>
    group_by(ego_ntile) %>%
    mutate(total_exposures = sum(n_domain_exposures)) |>
    ungroup() |>
    mutate(perc = n_domain_exposures / total_exposures * 100) |>
    filter(domain %in% unique(domains$domain)) |>
    select(domain, ego_ntile, perc) |>
    mutate(model = data$parameters$model)
}

best_fit_sims <-
  best_fit_sims |>
  pull(file) |>
  map_dfr(read_net)

best_fit_sims_top10 <-
  best_fit_sims |>
  left_join(domains |> select(domain, type)) |>
  group_by(type, model, ego_ntile) |>
  summarise(perc = sum(perc) / n()) |>
  rename(domain = type)

best_fit_sims <-
  best_fit_sims |>
  bind_rows(best_fit_sims_top10)


###--- Observed Exposures
exposures <-
  readRDS("data/main/observed_exposures.rds") |>
  filter(domain %in% domains$domain)


###--- Compute average top 10 domains
average_top10_exp <-
  exposures %>%
  left_join(domains |> select(domain, type)) |>
  group_by(type, ego_ntile) %>%
  summarise(perc = sum(perc) / n()) |>
  rename(domain = type)

exposures <-
  exposures |>
  bind_rows(average_top10_exp)


###--- Observed Adoptions
adoptions <-
  readRDS("data/main/domain_egos_adoptions.RDS") |>
  rename(ego_ntile = ntile) |>
  group_by(ego_ntile) %>%
  mutate(total_exposures = sum(n)) |>
  ungroup() %>%
  mutate(perc = n / total_exposures * 100) %>%
  group_by(domain, ego_ntile) %>%
  summarise(perc = sum(perc)) |>
  filter(domain %in% domains$domain)


###--- Compute average top 10 domains
average_top10_ado <-
  adoptions %>%
  left_join(domains |> select(domain, type)) |>
  group_by(type, ego_ntile) %>%
  summarise(perc = sum(perc) / n()) |>
  rename(domain = type)

adoptions <-
  adoptions |>
  bind_rows(average_top10_ado)


###--- First row of plots
adoptions_lib <-
  adoptions |>
  ungroup() |>
  left_join(domains, by = "domain") |>
  filter(type == "Liberal domains") |>
  filter(top4 == TRUE | domain == "Liberal domains") |>
  mutate(domain = domain_name) |>
  arrange(domain)


plots <- list()
for (d in unique(adoptions_lib$domain)) {
  plots[[length(plots) + 1]] =
    adoptions_lib %>%
    filter(domain == d) |>
    ggplot(aes(ego_ntile, perc)) +
    geom_col(color = "#00AEF3", fill = "#00AEF3") +
    labs(
      y = "Percentage",
      x = "Ego ideology percentile",
      title = d,
      subtitle = "Sharing"
    ) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 3))
}
names(plots) <- unique(adoptions_lib$domain)


###--- Second row of plots
exposures_lib <-
  exposures |>
  ungroup() |>
  left_join(domains, by = "domain") |>
  filter(type == "Liberal domains") |>
  filter(top4 == TRUE | domain == "Liberal domains") |>
  mutate(domain = domain_name) |>
  arrange(domain)


plots2 <- list()
for (d in unique(exposures_lib$domain)) {
  plots2[[length(plots2) + 1]] <-
    exposures_lib %>%
    filter(domain == d) |>
    ggplot(aes(ego_ntile, perc)) +
    geom_col(color = "#00AEF3", fill = "#00AEF3") +
    labs(
      y = "Percentage",
      x = "Ego ideology percentile",
      subtitle = "Exposure"
    ) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 3))
}
names(plots2) <- unique(exposures_lib$domain)

###--- Third row of plots
sims_lib <-
  best_fit_sims |>
  left_join(domains, by = "domain") |>
  filter(type == "Liberal domains") |>
  filter(top4 == TRUE | domain == "Liberal domains") |>
  mutate(domain = domain_name) |>
  arrange(domain)


plots3 <- list()
for (d in unique(sims_lib$domain)) {
  plots3[[length(plots3) + 1]] <-
    sims_lib %>%
    filter(domain == d) |>
    ggplot(aes(ego_ntile, perc, group = model, fill = model, alpha = model)) +
    geom_col(position = "identity", linetype = 0) +
    labs(
      y = "Percentage",
      x = "Ego ideology percentile",
      subtitle = "Simulated exposure"
    ) +
    scale_fill_manual(values = lib_colors) +
    scale_alpha_manual(values = c(.9, .7)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 3))
}
names(plots3) <- unique(sims_lib$domain)


###--- Put plot together
figure2_pt1 <- wrap_plots(c(plots, plots2, plots3)) +
  plot_layout(axis_titles = 'collect', nrow = 3)
ggsave(plot = figure2_pt1, "output/figure2_pt1.png", scale = 1.5, dpi = 500)


###--- Part 2 Conservative Domains
###--- First row of plots
adoptions_cons <-
  adoptions |>
  ungroup() |>
  left_join(domains, by = "domain") |>
  filter(type == "Conservative domains") |>
  filter(top4 == TRUE | domain == "Conservative domains") |>
  mutate(domain = domain_name) |>
  arrange(domain)


plots <- list()
for (d in unique(adoptions_cons$domain)) {
  plots[[length(plots) + 1]] =
    adoptions_cons %>%
    filter(domain == d) |>
    ggplot(aes(ego_ntile, perc)) +
    geom_col(color = "#de0100", fill = "#de0100") +
    labs(
      y = "Percentage",
      x = "Ego ideology percentile",
      title = d,
      subtitle = "Sharing"
    ) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 3))
}
names(plots) <- unique(adoptions_cons$domain)


###--- Second row of plots
exposures_cons <-
  exposures |>
  ungroup() |>
  left_join(domains, by = "domain") |>
  filter(type == "Conservative domains") |>
  filter(top4 == TRUE | domain == "Conservative domains") |>
  mutate(domain = domain_name) |>
  arrange(domain)

plots2 <- list()
for (d in unique(exposures_cons$domain)) {
  plots2[[length(plots2) + 1]] <-
    exposures_cons %>%
    filter(domain == d) |>
    ggplot(aes(ego_ntile, perc)) +
    geom_col(color = "#de0100", fill = "#de0100") +
    labs(
      y = "Percentage",
      x = "Ego ideology percentile",
      subtitle = "Exposure"
    ) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 3))
}
names(plots2) <- unique(exposures_cons$domain)

###--- Third row of plots
sims_cons <-
  best_fit_sims |>
  left_join(domains, by = "domain") |>
  filter(type == "Conservative domains") |>
  filter(top4 == TRUE | domain == "Conservative domains") |>
  mutate(domain = domain_name) |>
  arrange(domain)


plots3 <- list()
for (d in unique(sims_cons$domain)) {
  plots3[[length(plots3) + 1]] <-
    sims_cons %>%
    filter(domain == d) |>
    ggplot(aes(ego_ntile, perc, group = model, fill = model, alpha = model)) +
    geom_col(position = "identity", linetype = 0) +
    labs(
      y = "Percentage",
      x = "Ego ideology percentile",
      subtitle = "Simulated exposure"
    ) +
    scale_fill_manual(values = cons_colors) +
    scale_alpha_manual(values = c(.9, .7)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 3))
}
names(plots3) <- unique(sims_cons$domain)


###--- Part 2 of Figure 2 (Conservatives)

figure2_pt2 <- wrap_plots(c(plots, plots2, plots3)) +
  plot_layout(axis_titles = 'collect', nrow = 3)
ggsave(plot = figure2_pt2, "output/figure2_pt2.png", scale = 1.5, dpi = 500)
