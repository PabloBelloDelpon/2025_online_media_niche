
###--- Libraries
library(tidyverse)
library(fs)
library(glue)
library(data.table)

###--- main
alpha = "0.001" # homophily level
folder <- "17clean_adopt_treat_tables_complete2/"
output <- paste0("27ego_alter_ideology_cross_tabs_random_network_h",alpha,"/")
if(dir.exists(output) == FALSE) dir.create(output)

###--- Ideology and int files
random_net = readRDS(paste0("14networks/random_network_h",alpha,".rds")) %>% as.data.table()
alters_file <- "14networks/alter_to_int.RDS"
ideo_alters <- readRDS("20ideology_estimation/ideology_estimates_alters.RDS")
domain_ideology <- readRDS("20ideology_estimation/domain_ideology.RDS") %>% 
  select(domain, domain_ideo = mean) %>% 
  mutate(domain_ideo_tile = ntile(domain_ideo, 100))


###--- List files
done <- dir_ls(output)

files_tbl <- 
  dir_ls(folder) |> 
  as_tibble() |> 
  mutate(domain = str_remove(value, folder),
         output = str_replace(value, folder, output)) |> 
  filter(!output %in% done)


###--- Ideology alters prepare
ideo_alters <- 
  readRDS(alters_file) |> 
  left_join(ideo_alters |>  
              rename(user_id = user)) |> 
  drop_na() |> 
  transmute(
    alter_int,
    q = ntile(ideology_new,100))

 setkey(random_net, alter_int)

###--- Read adoptions table by domain

###--- Clean up table
for(i in 1:nrow(files_tbl)) {
  
  domain <-  files_tbl[i,]$domain
  domain = str_remove(domain, ".RDS")
  print(domain)
  
  tbl <- 
    readRDS(files_tbl[i,]$value) |> 
    filter(treated == 1) %>% 
    count(alter_int, ego_int) %>% 
    group_by(alter_int) %>% 
    summarise(mean_exp = round(mean(n))) %>% 
    select(alter_int, n_exposures = mean_exp)
  
  
  alters = unique(tbl$alter_int)
  ego_net = random_net[.(alters)]
  
  tbl = 
    ego_net %>% 
    left_join(tbl, multiple = "first") %>% 
    drop_na() %>% 
    select(- alter_int) %>% 
    mutate(domain = domain) %>% 
    left_join(domain_ideology)
  
  saveRDS(tbl, files_tbl[i,]$output)
  print(i)
  
}


res = c()

for(i in 1:nrow(files_tbl)){
  
  res[[i]] = readRDS(files_tbl$output[i]) %>% left_join(domain_ideology)
  print(i)
}


res = bind_rows(res)
saveRDS(res, paste0("99process_data/data/ego_alter_domain_tabs_random_network_h",alpha,".RDS"))


