###--- Libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)
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
      fct_recode("Equal" = "0")  # label k=0 as Symmetric
  ) |>
  ggplot(aes(h, k_label, fill = fit)) +
  geom_tile() +
  scale_fill_viridis(option = "magma",
                     name = "MAE") + 
  scale_y_discrete(
    breaks = c("Equal", "0.5", "1", "1.5", "2")
  ) +
  labs(
    y = "Homophily variance parameter (k)",
    x = expression("Homophily strength parameter (" * italic(beta) * ")"),
    fill = "Fit"
  ) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p1


###-- Names
tbl <- 
  asym_model |>
  mutate(model = ifelse(k == 0, "Equal homophily", "Differential homophily")) |> 
  mutate(model = ifelse(h == 0 & k == 0, "Null", model)) 


p2 <- 
  tbl |> 
  ggplot(aes(h, fit, group = k)) +
  # Asymmetric models
  geom_line(
    data = tbl |> filter(model == "Differential homophily"),
    aes(color = "Differential homophily", alpha = k)
  ) +
  # Symmetric model
  geom_line(
    data = tbl |> filter(model == "Equal homophily"),
    aes(color = "Equal homophily"),
    linewidth = 1
  ) +
  scale_color_manual(
    name = "Condition",
    values = c("Differential homophily" = "grey40", "Equal homophily" = "red")
  ) +
  scale_alpha_continuous(name = "Homophily variance parameter (k)") +
  labs(
    y = "MAE",
    x = expression("Homophily strength parameter (" * italic(beta) * ")"),
  ) +
  theme(
    legend.position = c(1.05, .4),
    legend.justification = c(1, 0),
    legend.title = element_text(size = 15),
    legend.text  = element_text(size = 15))


figure_3 <- p1 | p2 
figure_3 <- figure_3 + plot_annotation(tag_levels = "A")
figure_3
ggsave(plot = figure_3, "output/figure3.png", scale = 1.3, dpi = 500)

