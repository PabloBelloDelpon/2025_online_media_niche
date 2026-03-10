###--- Generate figure A1

###--- Libraries
library(tidyverse)
library(hrbrthemes)

theme_set(theme_ipsum(axis_title_size = 14, strip_text_size =  14) + 
            theme(
  legend.position = "none"))

###--- Data
domain_final <- readRDS("data/main/ego_alter_domain_tabs.RDS") |> distinct(domain) |> pull()
domain_ratings <- read.csv("data/domains/domain_tbl_300_ratings.csv")
domain_ideology <- 
  readRDS("data/ideology/domain_ideology.RDS") |>  
  select(domain, domain_ideo = mean) |> 
  mutate(domain_ideo = 
           (domain_ideo - min(domain_ideo, na.rm = TRUE))/
           (max(domain_ideo, na.rm = TRUE) - min(domain_ideo, na.rm = TRUE)))


tbl <- 
  domain_ratings |> 
  as_tibble() |> 
  filter(category == "News") |> 
  mutate(domain = str_remove(domain, "www."),
         domain = str_remove(domain, ".com")) |> 
  select(domain, allsides_confidence, allsides_rating, adfontes_reliability, adfontes_bias) |> 
  left_join(domain_ideology) |> 
  filter(domain %in% domain_final) |> 
  pivot_longer(cols = c(allsides_rating, adfontes_reliability, adfontes_bias),
               names_to = "measure") |> 
  mutate(measure = str_replace_all(measure, "_",  " "),
         measure = str_to_title(measure)) |> 
  group_by(measure) |> 
  mutate(value = (value - min(value, na.rm = TRUE))/(max(value, na.rm = TRUE) - min(value, na.rm = TRUE))) |> 
  drop_na(value, domain_ideo)


###--- Plot 1
p1 <-  
  tbl |> 
  filter(measure != "Adfontes Reliability") |> 
  mutate(measure = ifelse(measure == "Adfontes Bias", "Ad Fontes Bias", "AllSides Rating"), 
         measure = factor(measure, levels = c("AllSides Rating", "Ad Fontes Bias"))) |> 
  ggplot(aes(domain_ideo, value)) +
  geom_point(aes(color = domain_ideo)) +
  geom_smooth(method = "lm") +
  facet_wrap(~ measure, scales = "free") +
  labs(x = "Twitter-based domain ideology",
       y = "External domain ideology") +
  scale_color_gradient(low = "#00AEF3", high = "#de0100")

p1
ggsave("output/figure_A1.png", dpi = 400, scale = .7)

###--- Correlations

###--- Between external measures and Twitter based one
tbl |> 
  group_by(measure) |> 
  summarise(cor(value, domain_ideo))


###--- Between the two external measures
tbl |> 
  pivot_wider(names_from = measure, values_from = value) |> 
  select(allsides = "Allsides Rating", adfontes = "Adfontes Bias") |>
  drop_na() |> 
  summarise(cor(allsides, adfontes))

###--- Count domains covered by external measures
domains |> 
  group_by(measure) |> 
  summarise(n_collected = n_distinct(domain),
            n_total = length(domain_final)) |> 
  mutate(p = n_collected/n_total)

sum(domain_final %in% unique(domains$domain))
domain_final[which(!domain_final %in% unique(domains$domain))]
