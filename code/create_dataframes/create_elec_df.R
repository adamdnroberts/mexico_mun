library(dplyr)
library(ggplot2)
library(data.table)
library(readxl)
library(rdd)

setwd("~/mexico_mun/raw")
source("~/mexico_mun/code/process_elec_df.R")

estados <- c("Aguascalientes", "Baja California Sur", "Baja California", "Campeche", 
             "Chiapas", "Chihuahua", "Coahuila", "Colima", "Distrito Federal", 
             "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "Mexico", 
             "Michoacan", "Morelos", "Nayarit", "Nuevo Leon", "Oaxaca", "Puebla", 
             "Queretaro", "Quintana Roo", "San Luis Potos", "Sinaloa", "Sonora", 
             "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatan", "Zacatecas")

read_and_process_estado <- function(estado) {
  file_path <- paste0("CIDAC_MEXICO_DATA/", estado, ".xlsx")
  df <- read_excel(file_path)
  df$estado <- estado
  return(process_df(df))  # Process immediately instead of storing raw data
}

processed_dfs <- lapply(estados, read_and_process_estado)

mexico_municipal_elections <- data.table::rbindlist(processed_dfs)

pps_cols <- c("PPS...114", "PPS...115")
mexico_municipal_elections$PPS <- coalesce(mexico_municipal_elections[[pps_cols[1]]], mexico_municipal_elections[[pps_cols[2]]])
mexico_municipal_elections <- mexico_municipal_elections %>% select(-all_of(pps_cols))

psn_cols <- c("PSN...184", "PSN...185", "PSN...186")
mexico_municipal_elections$PSN <- coalesce(mexico_municipal_elections[[psn_cols[1]]], mexico_municipal_elections[[psn_cols[2]]], mexico_municipal_elections[[psn_cols[3]]])
mexico_municipal_elections <- mexico_municipal_elections %>% select(-all_of(psn_cols))

#create municipal ID
mexico_municipal_elections$mun_id <- gsub(" ", "", mexico_municipal_elections$Municipio)

mexico_municipal_elections$PRI_pct <- mexico_municipal_elections$PRI/mexico_municipal_elections$TOTAL
mexico_municipal_elections$PAN_pct <- mexico_municipal_elections$PAN/mexico_municipal_elections$TOTAL
mexico_municipal_elections$PRD_pct <- mexico_municipal_elections$PRD/mexico_municipal_elections$TOTAL

mexico_municipal_elections$PAN_margin <- mexico_municipal_elections$PAN_pct - mexico_municipal_elections$PRI_pct
mexico_municipal_elections$PRD_margin <- mexico_municipal_elections$PRD_pct - mexico_municipal_elections$PRI_pct

mexico_municipal_elections$PAN_treat <- as.integer(mexico_municipal_elections$PAN_margin > 0)
mexico_municipal_elections$PRD_treat <- as.integer(mexico_municipal_elections$PRD_margin > 0)

mexico_municipal_elections <- mexico_municipal_elections %>%
  arrange(mun_id, year) %>%
  mutate(
    # Previous period variables
    prev_PAN_pct = lag(PAN_pct),
    prev_PRD_pct = lag(PRD_pct), 
    prev_PRI_pct = lag(PRI_pct),
    prev_PAN_margin = lag(PAN_margin),
    prev_PRD_margin = lag(PRD_margin),
    prev_total = lag(TOTAL),
    # Next period variables  
    next_PAN_pct = lead(PAN_pct),
    next_PRD_pct = lead(PRD_pct),
    next_PRI_pct = lead(PRI_pct), 
    next_PAN_margin = lead(PAN_margin),
    next_PRD_margin = lead(PRD_margin),
    next_total = lead(TOTAL)
    )

mexico_municipal_elections$next_turnout_pct <- ifelse(mexico_municipal_elections$TOTAL > 0, mexico_municipal_elections$next_total/mexico_municipal_elections$TOTAL, NA)

save(mexico_municipal_elections, file = "~/mexico_mun/data/mexico_municipal_elections.Rdata")
