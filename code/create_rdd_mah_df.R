library(dplyr)

load("~/mexico_mun/data/md_normalized.Rdata")

load("~/mexico_mun/data/full_dataset_mexelec.Rdata")
big_df$mun_id <- gsub(" ", "", big_df$Municipio)
big_df$PAN_pct <- big_df$PAN_pct - 0.5

df <- subset(big_df, year>= 1995 & year <= 1997 
             #& estado!="Tlaxcala" 
             & (p1_name == "PRI" | p2_name == "PRI") & (p1_name == "PAN" | p2_name == "PAN")
)
#df$mun_id <- gsub(" ", "", df$Municipio)
df$PRD_pct <- df$PRD / (df$p1 + df$p2) # PRD percentage compared to top two
df$PRD_treat <- ifelse(df$PRD_pct > 0.5, 1, 0)

df_ref <- subset(big_df, year>= 1995 & year <= 1997 
                 #& estado!="Tlaxcala" 
                 #& (p1_name == "PRI" | p2_name == "PRI") & (p1_name == "PAN" | p2_name == "PAN")
)

#create smaller datasets for merge
ref_PAN <- subset(df_ref, select = c(year, mun_id, next_PAN_pct, PAN_pct, estado))
main_mun_PAN <- subset(df, select = c(year, mun_id, PAN_pct, estado))

#merge datasets using adjacent municipalities index
ref2 <- merge(md_norm_final,ref_PAN, by.x = "ref_mun_id", by.y = "mun_id")
ref2 <- ref2 %>% rename(ref_PAN_pct = PAN_pct, ref_next_PAN_pct = next_PAN_pct, ref_estado = estado, ref_year = year)
ref2$ref_PAN_wins <- ifelse(ref2$ref_PAN_pct > 0, 1, 0)

df_rdd <- merge(main_mun_PAN,ref2, by = "mun_id")
df_rdd <- df_rdd %>% 
  rename(main_year = year, main_estado = estado)

save(df_rdd, file = "C:/Users/adamd/Documents/mexico_mun/data/rdd_mah_dist.Rdata")
