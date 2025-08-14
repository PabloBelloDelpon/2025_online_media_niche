###--- First figure on the paper

###--- Libraries
library(tidyverse)
library(showtext)
library(sysfonts)
library(ggridges)
library(glue)
library(hrbrthemes)
library(patchwork)

###--- Graphic  Options
theme_set(theme_ipsum(axis_title_size = 14, 
                      plot_margin = margin(15, 15, 15, 15), 
                      plot_title_size = 14) +
            theme(
              plot.background = element_rect(fill = "white",linewidth = 0),
              panel.border = element_rect(colour = "white", fill = NA)))


###--- Load data

###--- Manually classified domain categories
domain_type <-
  read_tsv("data/main/domain_annotated.tsv") |> 
  mutate(type = ifelse(category == "News", "News", "Other")) |> 
  select(domain, type) |> 
  mutate(domain = str_remove(domain, "www."),
         domain = str_remove(domain, ".com"))

###--- Domain ideology
domain_ideology <- 
  readRDS("data/ideology/domain_ideology.RDS") |> 
  arrange(mean) |> 
  mutate(id = row_number()) |> 
  left_join(domain_type) 


###--- Lists of domains to plot
dom_list <- list(
  "Democracy Docket" = "democracydocket",
  "Daily Kos" = "dailykos",
  "Politicus USA" = "politicususa",
  "Raw Story" = "rawstory",
  "The DC Patriot" = "thedcpatriot",
  "One America" = "oann",
  "The Post Millenial" = "thepostmillennial",
  "American Greatness" = "amgreatness",
  "New York Times" = "nytimes",
  "FoxNews" = "foxnews"
 )
  

domain_selection <- 
  dom_list |> 
  stack() |> 
  as_tibble() |> 
  rename(domain_name = ind,
         domain = values)


###--- Plot
tbl <- 
  domain_ideology |> 
  filter(domain %in% domain_selection$domain) |> 
  left_join(domain_selection) |> 
  mutate(domain_name = fct_reorder(domain_name, mean,.desc = TRUE)) |>
  arrange(domain_name) |> 
  select(domain_name,tiles) |> 
  unnest(tiles)  |> 
  group_by(domain_name) |> 
  mutate(
    s = sum(n),
    prop = n/sum(n), 
    prop_roll = zoo::rollmean(prop,10,fill = NA)) |> 
  drop_na() 


###--- Plot
p1 <- 
  tbl |> 
  arrange(domain_name) |> 
  ggplot(aes(x = ntile, # x values
             y = domain_name, # the groupping variable
             height = prop_roll,
             fill = stat(x)) # the y axis height
  ) +
  geom_density_ridges_gradient(stat = "identity", scale = 1) +
  scale_fill_gradient(low = '#00AEF3', high = '#de0100') +
  #scale_x_continuous(breaks = c(0,25,50,75,100), limits = c(0,100)) +
  labs(title = "",
       y = "",
       x = "Ideology percentile")

p1 <- 
  p1 +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        text = element_text(size = 10),
        axis.text = element_text(vjust=-0.5)) 


###--- Second Plot (Domain Ideology Density plot)

###--- "Other" category
other <- 
  domain_ideology |> 
  filter(type != "News")


x <- other$mean
y <- density(x,n = 2^12)
q <- quantile(x,probs = c(.2,.4,.6,.8))
tbl <- tibble(x = y$x, y = y$y)

(p2 <- 
    tbl |> 
    ggplot(aes(x, y)) + 
    geom_line() + 
    geom_segment(aes(xend = x, yend = 0, colour = x)) + 
    scale_color_gradient(low = '#00AEF3', high = '#de0100') +
    labs(title = "Other domains",
         y = "Density",
         x = "Ideology") +
    xlim(c(-1,2.2)) +
    theme(legend.position = "none"))


#############--------
news <- 
  domain_ideology |> 
  filter(type == "News")

x <- news$mean
y <- density(x,n = 2^12)
q <- quantile(x,probs = c(.2,.4,.6,.8))
tbl <- tibble(x = y$x, y = y$y)

(p3 <- 
    tbl |> 
    ggplot(aes(x, y)) + 
    geom_line() + 
    geom_segment(aes(xend = x, yend = 0, colour = x)) + 
    scale_color_gradient(low = '#00AEF3', high = '#de0100') +
    labs(title = "News media domains",
         y = "Density",
         x = "Ideology") +
    xlim(c(-1,2.2)) +
    theme(legend.position = "none"))




###--- Put the plots together
ptw <- (p3 / p2) | p1 
ptw + plot_annotation(tag_levels = 'A')

ggsave(glue("output/figure1.png"),
       scale = 1.2,
       dpi = 600)
  


