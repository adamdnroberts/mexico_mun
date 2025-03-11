library(dplyr)
library(tidyr)
library(plyr)
library(fixest)

load("~/mexico_mun/data/full_dataset_mexelec.Rdata")

summary(big_df$year)

df <- subset(big_df, year >= 1995)

prd_columns_full <- grep("PRD", names(big_df), value = TRUE)
prd_columns <- setdiff(prd_columns_full,c("PRD_pct","PRD_treat"))
pan_columns <- c("PAN","PAN-PRD","PAN-ADC","PAN-CONV","PAN-PANAL","PAN-PANAL-PCP","PAN-PANAL-PES","PAN-PAC","PAN-PRD-CONV","PAN-PRD-CONV-PANAL","PAN-PRD-PT","PAN-PRD-PT-CONV","PAN-PRD-PVEM-PT","PAN-PRD-PT-PVEM-PCP-MC-PANAL","PAN-PRD-PUDC","PAN-PRD-PVEM","PAN-PRI","PAN-PRI-PANAL","PAN-PRI-PVEM","PAN-PRI-PVEM-PANAL","PAN-PRS","PAN-PSD","PAN-PT","PAN-PT-PANAL","PAN-PT-PANAL-MC","PAN-PT-PCDT-PJS","PAN-PVEM","PAN-PVEM-PANAL")
pri_columns <- grep("PRI", names(big_df), value = TRUE)

# Sum the values of these columns row-wise
df$PRD_sum <- rowSums(df[, prd_columns], na.rm = TRUE)
df$PAN_sum <- rowSums(df[, pan_columns], na.rm = TRUE)
df$PRI_sum <- rowSums(df[, pri_columns], na.rm = TRUE)

df$PRD_pct <- df$PRD_sum/df$TOTAL
df$PAN_pct <- df$PAN_sum/df$TOTAL
df$PRI_pct <- df$PRI_sum/df$TOTAL

# Initialize an empty list to store results
results_list <- list()

municipal_ids <- unique(df$mun_id)

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
  
  m <- NULL
  m <- tryCatch({
    feols(PAN_pct.x ~ PAN_pct.y | mun_id + year.y, data = leader)
  }, error = function(e) {
    cat("Error encountered:", e$message, "\n")
    NULL  # Return NULL or any other value to indicate an error
  })
  if (!is.null(m)){
    results_list[[mun_id]] <- cbind(m$coeftable,mun_id)
  } 
}
stop_time <- Sys.time()
print(stop_time - start_time)

clean_list <- Filter(Negate(is.logical), results_list)

final_results <- bind_rows(clean_list)
colnames(final_results) <- c("est", "se", "tvalue", "pvalue","mun_id")
rownames(final_results) <- NULL

hist(final_results$est)
summary(final_results$tvalue)

significant <- subset(final_results, pvalue <= 0.05)

summary(significant$est)

save(final_results, file = "~/mexico_mun/data/leader_results.Rdata")

#just looking at wins




