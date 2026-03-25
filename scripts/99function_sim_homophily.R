
homophily <- function(k, h) {
  
  beta <- (1:100/100)^k # beta sub i
  beta <- tibble(ego_ntile = 1:100, beta)
  
  hom_tbl = expand.grid(ego_ntile = 1:100, alter_ntile = 1:100)
  hom_tbl <- 
    hom_tbl |> 
    left_join(beta, by = join_by(ego_ntile)) |> 
    left_join(tbl |> select(ego_ntile = q, ego_ideo = ideo_q), by = join_by(ego_ntile)) |> 
    left_join(tbl |> select(alter_ntile = q, alter_ideo = ideo_q), by = join_by(alter_ntile)) |> 
    mutate(dist = abs(ego_ideo - alter_ideo),
           prob = exp(-beta*dist*h)) 
  
  ###--- Sample alters
  n_alters <-  1e5
  mean_diff <- c()
  for(i in 1:100){
    
    df = hom_tbl %>% filter(ego_ntile == i)
    ego_ideo <- unique(df$ego_ideo)
    alter_ideo = sample(x = df$alter_ntile, size = n_alters, replace = TRUE, prob = df$prob)
    abs_diff <-  abs(i - alter_ideo)
    mean_diff[i] <-  mean(abs_diff)
  }
  
  
  hom = tibble(ego_ntile = 1:100, av_dist = mean_diff, k = k, h = h)
  return(hom)
}

