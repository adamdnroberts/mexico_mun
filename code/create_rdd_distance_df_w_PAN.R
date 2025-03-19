library(dplyr)
library(rdd)

load("~/mexico_mun/data/pairwise_km.Rdata")

load("~/mexico_mun/data/full_dataset_mexelec_pcts.Rdata")
big_df$mun_id <- gsub(" ", "", big_df$Municipio)

df <- subset(big_df, year>= 1995 & year <= 1997 
             #& estado!="Tlaxcala" 
             #& (p1_name == "PRI" | p2_name == "PRI") & (p1_name == "PAN" | p2_name == "PAN")
)

df_ref <- subset(big_df, year>= 1995 & year <= 1997 
                 #& estado!="Tlaxcala" 
                 #& (p1_name == "PRI" | p2_name == "PRI") & (p1_name == "PAN" | p2_name == "PAN")
)

treated_df <- subset(big_df, year <= 1997 & PAN_treat == 1)
treated <- unique(treated_df$mun_id)

treated_df_before <- subset(big_df, year <= 1994 & PAN_treat == 1)
treated_before <- unique(treated_df_before$mun_id)

#create smaller datasets for merge
ref_PAN <- subset(df_ref, select = c(year, mun_id, next_PAN_pct, PAN_pct, estado))
ref_PAN_not_treated <- ref_PAN[!ref_PAN$mun_id %in% treated, ]

main_mun_PAN <- subset(df, select = c(year, mun_id, PAN_pct, estado))
main_mun_PAN_not_treated <- main_mun_PAN[!main_mun_PAN$mun_id %in% treated_before, ]

#merge datasets using adjacent municipalities index
ref2 <- merge(dH_df,ref_PAN_not_treated, by.x = c("neighbor"), by.y = c("mun_id"))
ref2 <- ref2 %>% rename(ref_PAN_pct = PAN_pct, ref_next_PAN_pct = next_PAN_pct, ref_estado = estado, ref_year = year)
ref2$ref_PAN_wins <- ifelse(ref2$ref_PAN_pct > 0, 1, 0)

df_rdd <- merge(main_mun_PAN_not_treated,ref2, by.x = c("mun_id"), by.y = c("mun"))
df_rdd <- df_rdd %>% 
  rename(main_year = year, main_estado = estado)

df_rdd$weight <- 1/df_rdd$dH

# First, sort by mun_id and then by d
df_rdd_sorted <- df_rdd %>%
  arrange(mun_id, dH)

df_rdd <- subset(df_rdd_sorted, ref_PAN_wins == 0 & main_estado == ref_estado & ref_next_PAN_pct > -0.5)

df_rdd_PAN <- df_rdd

save(df_rdd_PAN, file = "C:/Users/adamd/Documents/mexico_mun/data/rdd_distance_PAN.Rdata")