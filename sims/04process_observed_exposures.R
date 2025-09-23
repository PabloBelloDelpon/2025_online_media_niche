###--- 
library(tidyverse)


###--- Pretty Names
domains <- tribble(~ "domain", ~ "domain_name",
                   "democracydocket","Democracy Docket",
                   "dailykos", "Daily Kos",
                   "politicususa", "Politicus USA",
                   "rawstory", "Raw Story",
                   "thedcpatriot", "The DC Patriot",
                   "oann", "One America",
                   "thepostmillennial", "The Post Millenial",
                   "amgreatness", "American Greatness",
                   "Liberal domains", "Liberal domains",
                   "Conservative domains", "Conservative domains"
)


###--- Exposures dataset
data <- readRDS("data/main/ego_alter_domain_tabs.RDS")
data <- data %>% mutate(domain_ideo_tile = ntile(domain_ideo, 100))

####--- Domain Selections
lib_top10 <- data %>% distinct(domain, .keep_all = TRUE) %>% arrange(domain_ideo) %>% slice_head(n = 10) %>% pull(domain)
cons_top10 <- data %>% distinct(domain, .keep_all = TRUE) %>% arrange(desc(domain_ideo)) %>% slice_head(n = 10) %>% pull(domain)
total_exposures <-data |> group_by(domain) |> summarise(total_domain_exposures = sum(n))

extreme_domains_tbl <- 
  tibble(domain = c(lib_top10, cons_top10),
         type = c(rep("Liberal domains", length(lib_top10)), 
                  rep("Conservative domains", length(cons_top10)))) |> 
  full_join(domains) |> 
  mutate(type = ifelse(is.na(type), domain, type)) |> 
  mutate(top4 = ifelse(is.na(domain_name) == FALSE, TRUE, FALSE)) |> 
  mutate(domain_name = factor(domain_name, levels = domains$domain_name))

saveRDS(extreme_domains_tbl, "data/extreme_domains.rds")

###---  All Alters
tbl_exposures <-  
  data %>% 
  group_by(q_ego) %>% 
  mutate(total_exposures = sum(n)) %>% 
  ungroup() %>% 
  mutate(perc = n/total_exposures*100) %>% 
  group_by(domain, q_ego) %>% 
  summarise(perc = sum(perc)) |> 
  filter(domain %in% c(lib_top10, cons_top10)) |> 
  rename(ego_ntile = q_ego) |> 
  ungroup() |> 
  left_join(total_exposures)


tot_exposures <- 
  data |> 
  group_by(domain) |> 
  summarise(total_domain_exposures = sum(n))


saveRDS(tbl_exposures, "data/main/observed_exposures.rds")

# ###--- 
# av_lib <- 
#   tbl_exposures %>% 
#   filter(domain %in% lib_top10) %>% 
#   group_by(domain) %>% 
#   mutate(perc = perc/sum(perc)) %>% 
#   group_by(q_ego) %>% 
#   summarise(perc = sum(perc)/n()) %>% 
#   mutate(domain = "Liberal domains")
# 
# ###--- 
# av_cons <-  
#   tbl_exposures %>% 
#   filter(domain %in% cons_top10) %>% 
#   group_by(domain) %>% 
#   mutate(perc = perc/sum(perc)) %>% 
#   group_by(q_ego) %>% 
#   summarise(perc = sum(perc)/n()) %>% 
#   mutate(domain = "Conservative domains")
  

