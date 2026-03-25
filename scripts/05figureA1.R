###--- Generate figure A1

###--- Libraries
library(tidyverse)
library(hrbrthemes)

###--- Graphic theme
theme_set(
  theme_ipsum(axis_title_size = 14, strip_text_size = 14) +
    theme(legend.position = "none")
)

###--- Data
tbl <- readRDS("data/domain_ideology_external.rds")

###--- Plot 1
(p1 <-
  tbl |>
  ggplot(aes(domain_ideo, value)) +
  geom_point(aes(color = domain_ideo)) +
  geom_smooth(method = "lm") +
  facet_wrap(~measure, scales = "free") +
  labs(x = "Twitter-based domain ideology", y = "External domain ideology") +
  scale_color_gradient(low = "#00AEF3", high = "#de0100"))


ggsave("output/figure_A1.png", dpi = 400, scale = .7)

###--- Correlations

###--- Between external measures and Twitter based one
tbl |>
  group_by(measure) |>
  summarise(cor(value, domain_ideo))

###--- Between the two external measures
tbl |>
  select(domain, measure, value) |>
  pivot_wider(names_from = measure, values_from = value) |>
  select(allsides = "AllSides Rating", adfontes = "Ad Fontes Bias") |>
  drop_na() |>
  summarise(cor(allsides, adfontes))
