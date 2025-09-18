###--- Libraries
library(tidyverse)
library(hrbrthemes)
library(virids)
library(patchwork)


theme_set(theme_ipsum(base_size = 22, 
                      strip_text_size = 20, 
                      axis_title_size = 20, 
                      subtitle_size = 18))
###--- Load the data
tbl <- readRDS("/Users/pb216/Desktop/projects/2025_online_media_niche/data/main/simulations_fit2.rds")

###---
null_model <- 
  tbl |> 
  filter(model == "null") |>
  group_by(network_id, k , h) |> 
  summarise(fit = mean(mean_diff), .groups = "drop")

sym_model <- 
  tbl |> 
  filter(model == "symmetric") |> 
  group_by(network_id, k , h) |> 
  summarise(fit = mean(mean_diff), .groups = "drop")

asym_model <- 
  tbl |> 
  filter(model == "asymmetric") |> 
  group_by(network_id, k , h) |> 
  summarise(fit = mean(mean_diff), .groups = "drop")

p1 <- 
  asym_model |>
  mutate(
    k_label = factor(k, levels = sort(unique(k))) |> 
      fct_recode("Symmetric" = "0")  # label k=0 as Symmetric
  ) |>
  ggplot(aes(h, k_label, fill = fit)) +
  geom_tile() +
  scale_fill_viridis(option = "magma",
                     name = "Fit", 
                     trans = "log") + 
  scale_y_discrete(
    breaks = c("Symmetric", "0.5", "1", "1.5", "2")
  ) +
  labs(
    y = "Asymmetry parameter (k)",
    x = "Homophily strength parameter (Beta)",
    fill = "Fit"
  ) 



tbl <- 
  asym_model |>
  mutate(model = ifelse(k == 0, "Symmetric homophily", "Asymmetric homophily")) |> 
  mutate(model = ifelse(h == 0 & k == 0, "Null", model)) 



p2 <- 
  tbl |>
  ggplot(aes(h, fit, group = k)) +
  # Asymmetric models
  geom_line(
    data = tbl |> filter(model == "Asymmetric homophily"),
    aes(color = "Asymmetric homophily", alpha = k)
  ) +
  # Symmetric model
  geom_line(
    data = tbl |> filter(model == "Symmetric homophily"),
    aes(color = "Symmetric homophily"),
    linewidth = 1
  ) +
  scale_color_manual(
    name = "Condition",
    values = c("Asymmetric homophily" = "grey40", "Symmetric homophily" = "red")
  ) +
  scale_alpha_continuous(name = "Asymmetry parameter (k)") +
  labs(
    y = "Fit",
    x = "Homophily strength parameter (Beta)"
  ) +
  theme(
    legend.position = c(1, .5),
    legend.justification = c(1, 0)
  )


figure_3 <- p1 | p2
figure_3
ggsave(plot = figure_3, "output/figure3.png", scale = 1.1, dpi = 500)
