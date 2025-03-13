library(dplyr)

load("~/mexico_mun/data/pairwise_km.Rdata")

load("~/mexico_mun/data/full_dataset_mexelec_pcts.Rdata")
big_df$mun_id <- gsub(" ", "", big_df$Municipio)

df <- subset(big_df, year>= 1995 & year <= 1997 
             #& estado!="Tlaxcala" 
             & (p1_name == "PRI" | p2_name == "PRI") & (p1_name == "PRD" | p2_name == "PRD")
)
#df$mun_id <- gsub(" ", "", df$Municipio)
df$PRD_pct <- df$PRD / (df$p1 + df$p2) # PRD percentage compared to top two
df$PRD_treat <- ifelse(df$PRD_pct > 0.5, 1, 0)

df_ref <- subset(big_df, year>= 1995 & year <= 1997 
                 #& estado!="Tlaxcala" 
                 #& (p1_name == "PRI" | p2_name == "PRI") & (p1_name == "PRD" | p2_name == "PRD")
)

#create smaller datasets for merge
ref_PRD <- subset(df_ref, select = c(year, mun_id, next_PRD_pct, PRD_pct, estado))
main_mun_PRD <- subset(df, select = c(year, mun_id, PRD_pct, estado))

#merge datasets using adjacent municipalities index
ref2 <- merge(dH_df,ref_PRD, by.x = c("neighbor"), by.y = c("mun_id"))
ref2 <- ref2 %>% rename(ref_PRD_pct = PRD_pct, ref_next_PRD_pct = next_PRD_pct, ref_estado = estado, ref_year = year)
ref2$ref_PRD_wins <- ifelse(ref2$ref_PRD_pct > 0, 1, 0)

df_rdd <- merge(main_mun_PRD,ref2, by.x = c("mun_id"), by.y = c("mun"))
df_rdd <- df_rdd %>% 
  rename(main_year = year, main_estado = estado)

df_rdd$weight <- 1/df_rdd$dH

save(df_rdd, file = "C:/Users/adamd/Documents/mexico_mun/data/rdd_distance.Rdata")