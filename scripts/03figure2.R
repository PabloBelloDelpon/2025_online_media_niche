
###--- Libraries
library(patchwork)
library(tidyverse)
library(hrbrthemes)


###--- Graphic options
lib_colors = c("#A7C7E7",  "#1E3A5F")
cons_colors = c("#FF9999", "#CC3333", "#990000")

theme_set(theme_ipsum(base_size = 22, 
                      strip_text_size = 20, 
                      axis_title_size = 20, 
                      subtitle_size = 18) + 
            theme(legend.position = "none", plot.background = element_rect(color = "white")))

###--- Color Scale
colfunc <- colorRampPalette(c("#de0100", "#00AEF3"))

domain_colors <- 
  tibble(
    domain_ideo_ntile = c(1,5),
    domain_color = rev(colfunc(2)))

###---Read the data
alpha <-  c("0.15", "0.005") # homophily level

###--- Exposures dataset
data <- readRDS("data/main/ego_alter_domain_tabs.RDS")
data <- data %>% mutate(domain_ideo_tile = ntile(domain_ideo, 100))

###--- Randomized networks
network_files <-  paste0("data/network/randomized_network_h", alpha, ".RDS")
data_rn <-  map_dfr(network_files, readRDS, .id = "homophily")
data_rn <-  
  data_rn %>% 
  rename(q_ego = ego_ntile,
         q_alter = alter_ntile,
         n = n_exposures) %>% 
  arrange(homophily) %>% 
  mutate(homophily = ifelse(homophily == 1, "Strong", "Weak")) |> 
  mutate(domain_ideo_tile = ntile(domain_ideo, 100))

###--- Ago adoptions dataset
ego_data <- readRDS("data/main/domain_egos_adoptions.RDS")


####--- Domain Selections
lib <- data %>% distinct(domain, .keep_all = TRUE) %>% arrange(domain_ideo) %>% slice_head(n = 4) %>% pull(domain)
lib <- c(lib, "Liberal domains")
lib10 <- data %>% distinct(domain, .keep_all = TRUE) %>% arrange(domain_ideo) %>% slice_head(n = 10) %>% pull(domain)
cons <- data %>% distinct(domain, .keep_all = TRUE) %>% arrange(desc(domain_ideo)) %>% slice_head(n = 4) %>% pull(domain)
cons <- c(cons, "Conservative domains")
cons10 <- data %>% distinct(domain, .keep_all = TRUE) %>% arrange(desc(domain_ideo)) %>% slice_head(n = 10) %>% pull(domain)


###--- Pretty Names
names <- tribble(~ "domain", ~ "domain_name",
                 "democracydocket","Democracy Docket",
                 "dailykos", "Daily Kos",
                 "politicususa", "Politicus USA",
                 "rawstory", "Raw Story",
                 "motherjones", "Mother Jones",
                 "thedcpatriot", "The DC Patriot",
                 "oann", "One America",
                 "thepostmillennial", "The Post Millenial",
                 "amgreatness", "American Greatness",
                 "justthenews", "Just the News",
                 "Liberal domains", "Liberal domains",
                 "Conservative domains", "Conservative domains"
)


###---  All Alters
tbl_alters <-  
  data %>% 
  group_by(q_ego) %>% 
  mutate(s = sum(n)) %>% 
  ungroup() %>% 
  mutate(perc = n/s*100) %>% 
  filter(domain %in% c(cons, lib)) %>% 
  group_by(domain, q_ego) %>% 
  summarise(perc = sum(perc))

###--- 
av_lib <- 
  tbl_alters %>% 
  filter(domain %in% lib10) %>% 
  group_by(domain) %>% 
  mutate(perc = perc/sum(perc)) %>% 
  group_by(q_ego) %>% 
  summarise(perc = sum(perc)/n()) %>% 
  mutate(domain = "Liberal domains")

###--- 
av_cons <-  
  tbl_alters %>% 
  filter(domain %in% cons10) %>% 
  group_by(domain) %>% 
  mutate(perc = perc/sum(perc)) %>% 
  group_by(q_ego) %>% 
  summarise(perc = sum(perc)/n()) %>% 
  mutate(domain = "Conservative domains")

###--- Only most extreme domains
tbl_alters <- 
  tbl_alters %>% 
  bind_rows(av_lib, av_cons)

###--- All adoptions
tbl_adoptions <- 
  ego_data %>% 
  group_by(ntile) %>% 
  mutate(perc = n/sum(n)*100) %>%  
  rename(q_ego = ntile) %>% 
  ungroup() %>% 
  filter(domain %in% c(cons, lib)) %>% 
  group_by(domain, q_ego) %>% 
  mutate(perc = sum(perc))

###--- Average adoption liberals
av_lib <- 
  tbl_adoptions %>% 
  drop_na() %>% 
  filter(domain %in% lib10) %>% 
  group_by(domain) %>% 
  mutate(perc = perc/sum(perc)) %>% 
  group_by(q_ego) %>% 
  summarise(perc = sum(perc)/n()) %>% 
  mutate(domain = "Liberal domains")

###--- Average adoption conservatives
av_cons <- 
  tbl_adoptions %>% 
  drop_na() %>% 
  filter(domain %in% cons10) %>% 
  group_by(domain) %>% 
  mutate(perc = perc/sum(perc)) %>% 
  group_by(q_ego) %>% 
  summarise(perc = sum(perc)/n()) %>% 
  mutate(domain = "Conservative domains")

###--- Adoptions: Only most extreme domains
tbl_adoptions <- 
  tbl_adoptions %>% 
  bind_rows(av_lib, av_cons)

###--- Randomized network
tbl_rn <-  
  data_rn %>% 
  mutate(homophily = factor(homophily, levels = c("Weak","Strong"))) %>% 
  group_by(homophily, domain, q_ego) %>% 
  summarise(n = sum(n)) %>% 
  group_by(homophily, q_ego) %>% 
  mutate(s = sum(n)) %>% 
  ungroup() %>% 
  mutate(perc = n/s*100) %>% 
  filter(domain %in% c(cons, lib)) %>% 
  mutate(type = "Counterfactual Exposure") %>% 
  ungroup()

av_lib <- 
  tbl_rn %>% 
  filter(domain %in% lib10) %>% 
  group_by(homophily, domain) %>% 
  mutate(perc = perc/sum(perc)) %>% 
  group_by(homophily, q_ego) %>% 
  summarise(perc = sum(perc)/n()) %>% 
  mutate(domain = "Liberal domains")

av_cons <- 
  tbl_rn %>% 
  filter(domain %in% cons10) %>% 
  group_by(homophily, domain) %>% 
  mutate(perc = perc/sum(perc)) %>% 
  group_by(homophily, q_ego) %>% 
  summarise(perc = sum(perc)/n()) %>% 
  mutate(domain = "Conservative domains")


tbl_rn <-  
  tbl_rn %>% 
  bind_rows(av_lib, av_cons)

###--- Real exposures and adoptions
tbl2 <- 
  bind_rows('Sharing' = tbl_adoptions,
                 'Exposure' = tbl_alters, 
                 .id = 'type') %>% 
  mutate(type = factor(type, 
                       levels = c("Sharing", 
                                  "Exposure")))

tbl2_libs <-  
  tbl2 %>% 
  filter(domain %in% lib) %>% 
  left_join(names) %>% 
  ungroup() %>% 
  select( -domain) %>% 
  rename(domain = domain_name) %>% 
  mutate(domain = factor(domain, levels = names$domain_name)) %>% 
  group_by(domain) %>% 
  group_split()

###--- First row of plots
plots = list()
for(i in 1:length(tbl2_libs)){
  
  domain = unique(tbl2_libs[[i]]$domain)
  
  plots[[i]] =
    tbl2_libs[[i]] %>% 
    filter(type == "Sharing") %>% 
    ggplot(aes(q_ego, perc)) +
    geom_col(color = "#00AEF3", fill = "#00AEF3") +
    labs(y = "Percentage",
         x = "Ego ideology percentile", 
         title = domain, 
         subtitle = "Sharing")
  
}

###--- Second row of plots
plots2 <- list()
for(i in 1:length(tbl2_libs)){
  
  domain <- unique(tbl2_libs[[i]]$domain)
  
  plots2[[i]] <-
    tbl2_libs[[i]] %>% 
    filter(type == "Exposure") %>% 
    ggplot(aes(q_ego, perc)) +
    geom_col(color = "#00AEF3", fill = "#00AEF3") +
    labs(y = "Percentage",
         x = "Ego ideology percentile",
         subtitle = "Exposure")
  
}

###--- Third row of plots
row3 <-  
  tbl_rn %>% 
  filter(domain %in% lib) %>% 
  left_join(names) %>% 
  select( -domain) %>% 
  rename(domain = domain_name) %>% 
  mutate(domain = factor(domain, levels = names$domain_name)) %>% 
  ungroup() %>% 
  group_by(domain) %>% 
  group_split()

plots3 <- list()
for(i in 1:length(row3)){
  
  domain <- unique(row3[[i]]$domain)
  
  plots3[[i]] <-
    row3[[i]] %>% 
    ggplot(aes(q_ego, perc, group = homophily, fill = homophily, alpha = homophily)) +
    geom_col(position = "identity", linetype = 0) +
    labs(y = "Percentage",
         x = "Ego ideology percentile",
         subtitle = "Simulated exposure") +
    scale_fill_manual(values = lib_colors[c(1,2)]) +
    scale_alpha_manual(values = c(.9,.7)) 
  
}

###--- Put plot together
figure2_pt1 <- wrap_plots(c(plots, plots2, plots3)) + plot_layout(axis_titles = 'collect', nrow = 3)
ggsave(plot = figure2_pt1, "output/figure2_pt1.png", scale = 1.5, dpi = 500)



###--- Part 2 of Figure 2 (Conservatives)
tbl2_cons <- 
  tbl2 %>% 
  filter(domain %in% cons) %>% 
  left_join(names) %>% 
  ungroup() %>% 
  select( -domain) %>% 
  rename(domain = domain_name) %>% 
  mutate(domain = factor(domain, levels = names$domain_name)) %>% 
  group_by(domain) %>% 
  group_split()

plots <- list()
for(i in 1:length(tbl2_cons)){
  
  domain <- unique(tbl2_cons[[i]]$domain)
  
  plots[[i]] <-
    tbl2_cons[[i]] %>% 
    filter(type == "Sharing") %>% 
    ggplot(aes(q_ego, perc)) +
    geom_col(color = "#de0100", fill = "#de0100") +
    labs(y = "Percentage",
         x = "Ego ideology percentile", 
         title = domain, 
         subtitle = "Sharing")
  
}

plots2 = list()
for(i in 1:length(tbl2_cons)){
  
  domain <- unique(tbl2_cons[[i]]$domain)
  
  plots2[[i]] <-
    tbl2_cons[[i]] %>% 
    filter(type == "Exposure") %>% 
    ggplot(aes(q_ego, perc)) +
    geom_col(color = "#de0100", fill = "#de0100") +
    labs(y = "Percentage",
         x = "Ego ideology percentile",
         subtitle = "Exposure")
  
}

row3 <- 
  tbl_rn %>% 
  filter(domain %in% cons) %>% 
  left_join(names) %>% 
  select( -domain) %>% 
  rename(domain = domain_name) %>% 
  mutate(domain = factor(domain, levels = names$domain_name)) %>% 
  ungroup() %>% 
  group_by(domain) %>% 
  group_split()

plots3 <- list()

for(i in 1:length(row3)){
  
  domain <- unique(row3[[i]]$domain)
  
  plots3[[i]] <-
    row3[[i]] %>% 
    ggplot(aes(q_ego, perc, group = homophily, fill = homophily, alpha = homophily)) +
    geom_col(position = "identity", linetype = 0) +
    labs(y = "Percentage",
         x = "Ego ideology percentile",
         subtitle = "Simulated exposure") +
    scale_fill_manual(values = cons_colors[c(1,3)]) +
    scale_alpha_manual(values = c(.9,.7)) 
  
}

figure2_pt2 <- wrap_plots(c(plots, plots2, plots3)) + plot_layout(axis_titles = 'collect', nrow = 3)
ggsave(plot = figure2_pt2, "output/figure2_pt2.png", scale = 1.5, dpi = 500)



