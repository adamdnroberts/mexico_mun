library(dplyr)
library(ggplot2)
library(data.table)
library(readxl)
library(rdd)

setwd("~/mexico_mun/raw")

##LOOPS##

source("~/mexico_mun/code/process_elec_df_old_idea.R")

estados <- c("Aguascalientes","Baja California Sur","Baja California","Campeche","Chiapas","Chihuahua","Coahuila","Colima","Distrito Federal","Durango","Guanajuato","Guerrero","Hidalgo","Jalisco","Mexico","Michoacan","Morelos","Nayarit","Nuevo Leon","Oaxaca","Puebla","Queretaro","Quintana Roo","San Luis Potos","Sinaloa","Sonora","Tabasco","Tamaulipas","Tlaxcala","Veracruz","Yucatan","Zacatecas")

for (est in estados){
  df <- read_excel(paste0("CIDAC_MEXICO_DATA/",est,".xlsx"))
  df$estado <- est
  df_name <- paste0(gsub(" ", "", est),"_raw")
  assign(df_name, df)
}

raw_list <- list(Aguascalientes_raw,BajaCalifornia_raw,BajaCaliforniaSur_raw,Campeche_raw,Chiapas_raw,Chihuahua_raw,Coahuila_raw,Colima_raw ,DistritoFederal_raw,Durango_raw ,Guanajuato_raw,Guerrero_raw,Hidalgo_raw,Jalisco_raw,Mexico_raw,Michoacan_raw,Morelos_raw,Nayarit_raw,NuevoLeon_raw,Oaxaca_raw ,Puebla_raw,Queretaro_raw,QuintanaRoo_raw,SanLuisPotos_raw,Sinaloa_raw,Sonora_raw,Tabasco_raw,Tamaulipas_raw,Tlaxcala_raw,Veracruz_raw,Yucatan_raw,Zacatecas_raw)

processed_dfs <- lapply(raw_list, process_df)

big_df <- do.call(rbind, processed_dfs)

big_df$PPS <- ifelse(is.na(big_df$PPS...114),big_df$PPS...115,big_df$PPS...114)
big_df <- big_df %>% select(-PPS...115, -PPS...114)

big_df$PSN <- ifelse(is.na(big_df$PSN...184),
                     ifelse(is.na(big_df$PSN...185),big_df$PSN...186,big_df$PSN...185),
                     big_df$PSN...184)
big_df <- big_df %>% select(-PSN...184, -PSN...185, -PSN...186)

big_df$mun_id <- gsub(" ", "", big_df$Municipio)

#check pairs
df <- subset(big_df, year>= 1995 & year <= 1997)

no_PRI <- subset(df, p1_name != "PRI" & p2_name != "PRI")

no_PRI$parties <- paste(no_PRI$p1_name, no_PRI$p2_name)
unique(no_PRI$parties)

#######

big_df$PRI_pct <- big_df$PRI/big_df$TOTAL
big_df$PAN_pct <- big_df$PAN/big_df$TOTAL
big_df$PRD_pct <- big_df$PRD/big_df$TOTAL

big_df$PAN_margin <- big_df$PAN_pct - big_df$PRI_pct
big_df$PRD_margin <- big_df$PRD_pct - big_df$PRI_pct

big_df$PAN_treat <- ifelse(big_df$PAN_margin > 0, 1, 0)
big_df$PRD_treat <- ifelse(big_df$PRD_margin > 0, 1, 0) 

big_df <- big_df %>%
  arrange(mun_id, year) %>%
  mutate(prev_PAN_pct = lag(PAN_pct, n = 1), 
         next_PAN_pct = lead(PAN_pct, n = 1), 
         prev_PRD_pct = lag(PRD_pct, n = 1), 
         next_PRD_pct = lead(PRD_pct, n = 1),
         prev_PRI_pct = lag(PRI_pct, n = 1), 
         next_PRI_pct = lead(PRI_pct, n = 1),
         prev_PAN_margin = lag(PAN_margin, n = 1), 
         next_PAN_margin = lead(PAN_margin, n = 1), 
         prev_PRD_margin = lag(PRD_margin, n = 1), 
         next_PRD_margin = lead(PRD_margin, n = 1),
         prev_total = lag(TOTAL, n = 1),
         next_total = lead(TOTAL, n = 1))

big_df$next_turnout_pct <- ifelse(big_df$TOTAL > 0, big_df$next_total/big_df$TOTAL, NA)

save(big_df, file = "~/mexico_mun/data/full_dataset_mexelec_pcts.Rdata")
