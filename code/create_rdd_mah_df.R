library(dplyr)

load("~/mexico_mun/data/md_normalized.Rdata")

load("~/mexico_mun/data/full_dataset_mexelec_pcts.Rdata")
big_df$mun_id <- gsub(" ", "", big_df$Municipio)

df <- subset(big_df, year>= 1995 & year <= 1997 &  (p1_name == "PRI" | p2_name == "PRI"))

df_ref <- subset(big_df, year>= 1995 & year <= 1997 )

treated_df <- subset(big_df, year <= 1997 & PAN_treat == 1)
treated <- unique(treated_df$mun_id)

treated_df_before <- subset(big_df, year <= 1994 & PAN_treat == 1)
treated_before <- unique(treated_df_before$mun_id)

#create smaller datasets for merge
ref_PAN <- subset(df_ref, select = c(year, mun_id, next_PAN_pct, PAN_pct, next_PRD_pct, PRD_pct, estado, PAN_margin))
ref_PAN_not_treated <- ref_PAN[!ref_PAN$mun_id %in% treated, ]

main_mun_PAN <- subset(df, select = c(year, mun_id, PAN_pct, PAN_margin, PRD_pct, estado))
main_mun_PAN_not_treated <- main_mun_PAN[!main_mun_PAN$mun_id %in% treated_before, ]

save(main_mun_PAN_not_treated, file = "C:/Users/adamd/Documents/mexico_mun/data/PAN_untreated.Rdata")

#merge datasets using adjacent municipalities index
ref2 <- merge(md_norm_final,ref_PAN_not_treated, by = c("mun_id"))
ref2 <- ref2 %>% rename(ref_PAN_pct = PAN_pct, ref_next_PAN_pct = next_PAN_pct, 
                        ref_PRD_pct = PRD_pct, ref_next_PRD_pct = next_PRD_pct, 
                        ref_estado = estado, ref_year = year, ref_PAN_margin = PAN_margin)
ref2$ref_PAN_wins <- ifelse(ref2$ref_PAN_margin > 0, 1, 0)

df_rdd <- merge(main_mun_PAN_not_treated,ref2, by = c("mun_id"))
df_rdd <- df_rdd %>% 
  rename(main_year = year, main_estado = estado)

df_rdd$weight <- df_rdd$mah_d

# First, sort by mun_id and then by d
df_rdd_sorted <- df_rdd %>%
  arrange(mun_id, mah_d)

df_rdd_PAN_md <- df_rdd_sorted

save(df_rdd_PAN_md, file = "C:/Users/adamd/Documents/mexico_mun/data/rdd_mah_distance_PAN.Rdata")
