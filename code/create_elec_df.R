library(dplyr)
library(ggplot2)
library(data.table)
library(readxl)
library(rdd)

setwd("~/mexico_RD")

##LOOPS##

source("process_df.R")

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

save(big_df, file = "~/mexico_RD/full_dataset_mexelec.Rdata")
