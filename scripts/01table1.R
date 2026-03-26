###--- Libraries
library(tidyverse)
library(xtable)
library(data.table)
library(DescTools)

###--- Load the data ---###

###--- NYT to use as ideology cutoff point
nyt_ideo <-
  readRDS("data/domain_ideology.rds") |>
  filter(domain == "nytimes") |>
  pull(mean)

###--- Ideology estimates for egos
ideo_egos <-
  readRDS("data/ego_ideology.rds") |>
  mutate(ideo_bi = ifelse(ideo_est > nyt_ideo, "Conservative", "Liberal"))

###--- Ideology estimates for alters
ideo_alters <-
  readRDS("data/alter_ideology.rds") |>
  mutate(ideo_bi = ifelse(ideo_est > nyt_ideo, "Conservative", "Liberal"))

###--- Network data
# You might run out of memory. If so, use mem.maxVSize(32e9).
# Alternatively, substitute with data.table code
network <-
  readRDS("data/observed_network.rds") |>
  left_join(ideo_egos |> select(ego_int, ideo_bi)) |>
  left_join(ideo_alters |> select(alter_int, alter_ideo_bi = ideo_bi))

###--- Ego domain adoption data
ego_data <- readRDS("data/domain_egos_adoptions.rds")

###--- Exposure volume
exposure_data <- readRDS("data/ego_alter_domain_tabs_binary.rds")


###--- Calculations for table 1 ---###

###--- Proportion of egos and alters of each ideology
(ideo_egos2 <-
  ideo_egos |>
  count(ideo_bi) |>
  drop_na() |>
  mutate(prop = n / sum(n)))

(r1 <-
  ideo_egos2 |>
  mutate(value = paste0(n, "(", round(prop, 3) * 100, "%)")) |>
  select(ideo = ideo_bi, value) |>
  mutate(cat = "Number (%) of egos"))

(ideo_alters2 <-
  ideo_alters |>
  count(ideo_bi) |>
  drop_na() |>
  mutate(prop = n / sum(n)))

(r2 <-
  ideo_alters2 |>
  mutate(value = paste0(n, "(", round(prop, 3) * 100, "%)")) |>
  select(ideo = ideo_bi, value) |>
  mutate(cat = "Number (%) of alters"))


###--- Number of ties
ties <-
  network |>
  drop_na(alter_ideo_bi) |>
  group_by(ideo_bi, ego_int) |>
  summarise(n_ties = n()) |>
  group_by(ideo_bi) |>
  summarise(av_n_ties = sum(n_ties) / n())


(r3 <-
  ties |>
  select(ideo = ideo_bi, value = av_n_ties) |>
  drop_na() |>
  mutate(cat = "Average number of ties", value = as.character(round(value, 3))))

###--- Homophily
homophily <-
  network |>
  group_by(ego_int) |>
  count(ideo_bi, alter_ideo_bi)

(homophily <-
  homophily |>
  drop_na() |>
  rename(ego_ideo_bi = ideo_bi) |>
  group_by(ego_int) |>
  mutate(prop = n / sum(n)))

(homophily <-
  homophily |>
  group_by(ego_ideo_bi, alter_ideo_bi) |>
  summarise(av_n = mean(n), av_prop = mean(prop), sd_prop = sd(prop)))


(r4 <-
  homophily |>
  filter(ego_ideo_bi == alter_ideo_bi) |>
  mutate(value = paste0(round(av_n, 3), "(", round(av_prop, 3) * 100, "%)")) |>
  select(ideo = ego_ideo_bi, value) |>
  mutate(cat = "Average number (%) of ingroup ties"))


###--- Exposure Volume
(r5 <-
  exposure_data |>
  as_tibble() |>
  group_by(q_ego) |>
  summarise(n_exposed = sum(n)) |>
  left_join(ideo_egos2 |> rename(q_ego = ideo_bi)) |>
  mutate(value = as.character(round(n_exposed / n, 3))) |>
  select(ideo = q_ego, value) |>
  mutate(cat = "Average exposed media content"))

###---- Outdegree Distribution
ties2 <-
  network |>
  drop_na(alter_ideo_bi) |>
  group_by(ideo_bi, ego_int) |>
  summarise(n_ties = n())

ties_cons <- ties2 |> filter(ideo_bi == "Conservative") |> pull(n_ties)
ties_libs <- ties2 |> filter(ideo_bi == "Liberal") |> pull(n_ties)

gini_cons <- Gini(ties_cons)
gini_libs <- Gini(ties_libs)

r6 <-
  tibble(
    ideo = c("Conservative", "Liberal"),
    value = as.character(round(c(gini_cons, gini_libs), 3))
  ) |>
  mutate(cat = "Outdegree inequality")


###--- Alter indegree inequality
setDT(network)
degree <- network[, .(n = .N), by = .(ideo_bi, alter_ideo_bi, alter_int)]


r7 <-
  degree |>
  filter(ideo_bi == alter_ideo_bi) |>
  group_by(alter_ideo_bi) |>
  summarise(value = as.character(round(Gini(n), 3))) |>
  rename(ideo = alter_ideo_bi) |>
  mutate(cat = "Ingroup alters' indegree inequality")


###--- Create the table
tbl <-
  bind_rows(r1, r2, r3, r4, r5, r6, r7) |>
  pivot_wider(names_from = ideo, values_from = value) |>
  select(" " = cat, Liberals = Liberal, Conservatives = Conservative) |>
  as.data.frame()

table1 <- xtable(tbl)
print(
  table1,
  type = "html",
  file = "output/table1.html",
  include.rownames = FALSE
)
