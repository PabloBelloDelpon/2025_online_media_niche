###--- Libraries
library(patchwork)
library(tidyverse)
library(hrbrthemes)

###--- Graphic options
lib_colors <- c("#A7C7E7", "#1E3A5F")
cons_colors <- c("#FF9999", "#990000")

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


###--- Load the data ---###

####--- Domains to be displayed
domains <-
  readRDS("data/extreme_domains.rds")

###--- Simulations to be displayed
best_fit_sims <-
  readRDS("data/figure2_best_fitting_sims.rds")

###--- Observed exposures
exposures <-
  readRDS("data/observed_exposures.rds") |>
  filter(domain %in% domains$domain)


###--- Prepare the data for the plots ---###

###--- Compute average top 10 domains
average_top10_exp <-
  exposures |>
  left_join(domains |> select(domain, type)) |>
  group_by(type, ego_ntile) |>
  summarise(perc = sum(perc) / n()) |>
  rename(domain = type)

###--- Add top 10 to other domains
exposures <-
  exposures |>
  bind_rows(average_top10_exp)


###--- Observed Adoptions
adoptions <-
  readRDS("data/domain_egos_adoptions.rds") |>
  rename(ego_ntile = ntile) |>
  group_by(ego_ntile) |>
  mutate(total_exposures = sum(n)) |>
  ungroup() |>
  mutate(perc = n / total_exposures * 100) |>
  group_by(domain, ego_ntile) |>
  summarise(perc = sum(perc)) |>
  filter(domain %in% domains$domain)


###--- Compute average top 10 domains
average_top10_ado <-
  adoptions |>
  left_join(domains |> select(domain, type)) |>
  group_by(type, ego_ntile) |>
  summarise(perc = sum(perc) / n()) |>
  rename(domain = type)

###--- Add top 10 adoptions to other domains
adoptions <-
  adoptions |>
  bind_rows(average_top10_ado)


###--- Plot ---###

###--- Produce first row of plots
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
  plots[[length(plots) + 1]] <-
    adoptions_lib |>
    filter(domain == d) |>
    ggplot(aes(ego_ntile, perc)) +
    geom_col(color = "#00AEF3", fill = "#00AEF3", na.rm = TRUE) +
    labs(
      y = "Percentage",
      x = "Ego ideology percentile",
      title = d,
      subtitle = "Sharing"
    ) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 3))
}

names(plots) <- unique(adoptions_lib$domain)


###--- Produce second row of plots
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
    exposures_lib |>
    filter(domain == d) |>
    ggplot(aes(ego_ntile, perc)) +
    geom_col(color = "#00AEF3", fill = "#00AEF3", na.rm = TRUE) +
    labs(
      y = "Percentage",
      x = "Ego ideology percentile",
      subtitle = "Exposure"
    ) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 3))
}
names(plots2) <- unique(exposures_lib$domain)


###--- Produce third row of plots
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
    sims_lib |>
    filter(domain == d) |>
    ggplot(aes(ego_ntile, perc, group = model, fill = model, alpha = model)) +
    geom_col(position = "identity", linetype = 0, na.rm = TRUE) +
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
figure2_pt1 <-
  wrap_plots(c(plots, plots2, plots3)) +
  plot_layout(axis_titles = 'collect', nrow = 3)

ggsave(plot = figure2_pt1, "output/figure2_pt1.png", scale = 2, dpi = 500)


###--- Part 2 Conservative Domains

###--- Produce first row of plots
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
  plots[[length(plots) + 1]] <-
    adoptions_cons |>
    filter(domain == d) |>
    ggplot(aes(ego_ntile, perc)) +
    geom_col(color = "#de0100", fill = "#de0100", na.rm = TRUE) +
    labs(
      y = "Percentage",
      x = "Ego ideology percentile",
      title = d,
      subtitle = "Sharing"
    ) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 3))
}

names(plots) <- unique(adoptions_cons$domain)


###--- Produce second row of plots
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
    exposures_cons |>
    filter(domain == d) |>
    ggplot(aes(ego_ntile, perc)) +
    geom_col(color = "#de0100", fill = "#de0100", na.rm = TRUE) +
    labs(
      y = "Percentage",
      x = "Ego ideology percentile",
      subtitle = "Exposure"
    ) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 3))
}

names(plots2) <- unique(exposures_cons$domain)

###--- Produce third row of plots
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
    sims_cons |>
    filter(domain == d) |>
    ggplot(aes(ego_ntile, perc, group = model, fill = model, alpha = model)) +
    geom_col(position = "identity", linetype = 0, na.rm = TRUE) +
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

figure2_pt2 <-
  wrap_plots(c(plots, plots2, plots3)) +
  plot_layout(axis_titles = 'collect', nrow = 3)

ggsave(plot = figure2_pt2, "output/figure2_pt2.png", scale = 2, dpi = 500)
