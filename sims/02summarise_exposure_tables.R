###--- Libraries
library(tidyverse)

###--- Folders
exposures_folder <- "/Users/pb216/Desktop/projects/social_influence/17clean_adopt_treat_tables_complete2"
output_folder <- "/Users/pb216/Desktop/projects/social_influence/17clean_adopt_treat_tables_complete2_summarized"

files_tbl <- 
  dir_ls(exposures_folder) |> 
  as_tibble() |> 
  mutate(domain = str_remove(value, folder),
         output = str_replace(value, folder, output_folder))
  

for(i in 1:nrow(files_tbl)) {
  
  domain <-  files_tbl[i,]$domain
  domain = str_remove(domain, ".RDS")
  domain = str_remove(domain, "/")
  
  print(domain)
  
  tbl <- 
    readRDS(files_tbl[i,]$value) |> 
    filter(treated == 1) %>% 
    count(alter_int, ego_int) %>% 
    group_by(alter_int) %>% 
    summarise(mean_exp = round(mean(n))) %>% 
    select(alter_int, n_exposures = mean_exp)

  saveRDS(tbl, files_tbl[i,]$output)  
  print(i)
}
  