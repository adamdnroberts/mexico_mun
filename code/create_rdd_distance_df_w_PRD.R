library(dplyr)

load("~/mexico_mun/data/pairwise_km.Rdata")

load("~/mexico_mun/data/full_dataset_mexelec_pcts.Rdata")
big_df$mun_id <- gsub(" ", "", big_df$Municipio)

df <- subset(big_df, year>= 1995 & year <= 1997 &  (p1_name == "PRI" | p2_name == "PRI"))

#df <- df[, colSums(is.na(df)) != nrow(df)] No coalitions in this!

df_ref <- subset(big_df, year>= 1995 & year <= 1997 
                 #& estado!="Tlaxcala" 
                 #& (p1_name == "PRI" | p2_name == "PRI") & (p1_name == "PRD" | p2_name == "PRD")
)

treated_df <- subset(big_df, year <= 1997 & PRD_treat == 1)
treated <- unique(treated_df$mun_id)

treated_df_before <- subset(big_df, year <= 1994 & PRD_treat == 1)
treated_before <- unique(treated_df_before$mun_id)

#create smaller datasets for merge
ref_PRD <- subset(df_ref, select = c(year, mun_id, next_PRD_pct, PRD_pct, next_PAN_pct, PAN_pct, estado))
ref_PRD_not_treated <- ref_PRD[!ref_PRD$mun_id %in% treated, ]

main_mun_PRD <- subset(df, select = c(year, mun_id, PRD_margin, PAN_pct, estado))
main_mun_PRD_not_treated <- main_mun_PRD[!main_mun_PRD$mun_id %in% treated_before, ]

#merge datasets using adjacent municipalities index
ref2 <- merge(dH_df,ref_PRD_not_treated, by.x = c("neighbor"), by.y = c("mun_id"))
ref2 <- ref2 %>% rename(ref_PRD_pct = PRD_pct, ref_next_PRD_pct = next_PRD_pct, 
                        ref_PAN_pct = PAN_pct, ref_next_PAN_pct = next_PAN_pct, 
                        ref_estado = estado, ref_year = year)
ref2$ref_PRD_wins <- ifelse(ref2$ref_PRD_pct > 0, 1, 0)

df_rdd <- merge(main_mun_PRD_not_treated,ref2, by.x = c("mun_id"), by.y = c("mun"))
df_rdd <- df_rdd %>% 
  rename(main_year = year, main_estado = estado)

df_rdd$weight <- 1/df_rdd$dH

# First, sort by mun_id and then by d
df_rdd_sorted <- df_rdd %>%
  arrange(mun_id, dH)

df_rdd_PRD <- df_rdd_sorted

save(df_rdd_PRD, file = "C:/Users/adamd/Documents/mexico_mun/data/rdd_distance_PRD.Rdata")