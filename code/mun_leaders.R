library(dplyr)
library(tidyr)
library(plyr)
library(fixest)

load("~/mexico_mun/data/full_dataset_mexelec.Rdata")

summary(big_df$year)

df <- subset(big_df, year >= 1995)

# Initialize an empty list to store results
results_list <- list()

municipal_ids <- unique(df$mun_id)[1:100]

start_time <- Sys.time()
for (mun_id in municipal_ids) {
  # Filter for the current municipal ID
  current_mun <- df %>% 
    filter(mun_id == mun_id)
  current_mun$year_lead3 <- current_mun$year + 3
  current_mun$year_lead2 <- current_mun$year + 2
  current_mun$year_lead1 <- current_mun$year + 1
  
  
  refs_leader <- tibble(ref_id = unique(df$mun_id), current_mun = mun_id)
  
  # Get unique years from the current municipal data
  unique_years <- unique(current_mun$year)
  
  # Expand refs_leader for each unique year
  expanded_refs_leader <- refs_leader %>%
    crossing(year = unique_years)
  
  merge_leader <- current_mun %>%
    left_join(expanded_refs_leader, by = c("mun_id" = "current_mun", "year" = "year"))
  
  leader <- bind_rows(
    df %>% left_join(merge_leader, by = c("mun_id" = "ref_id", "year" = "year_lead1")) %>% mutate(lead = 1),
    df %>% left_join(merge_leader, by = c("mun_id" = "ref_id", "year" = "year_lead2")) %>% mutate(lead = 2),
    df %>% left_join(merge_leader, by = c("mun_id" = "ref_id", "year" = "year_lead3")) %>% mutate(lead = 3)
  )
  
  m <- feols(PAN.x ~ PAN.y | mun_id + lead, data = leader)
  results_list[[mun_id]] <- cbind(m$coeftable,mun_id)
}
stop_time <- Sys.time()
print(stop_time - start_time)

final_results <- bind_rows(results_list)
colnames(final_results) <- c("est", "se", "tvalue", "pvalue","mun_id")
rownames(final_results) <- NULL

hist(final_results$est)
summary(final_results$tvalue)

significant <- subset(final_results, pvalue <= 0.05)

summary(significant$est)