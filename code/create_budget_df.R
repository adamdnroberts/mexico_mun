library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(readxl)
library(rdd)

setwd("~/mexico_RD")

##LOOPS##

bdfs <- list()
for (year in 1989:2023){
   df <- read.csv(paste0("~/mexico_RD/efipem_municipal_csv/conjunto_de_datos/efipem_municipal_anual_tr_cifra_",year,".csv"))
   #df_name <- paste0("y",year)
   #assign(df_name, df)
   bdfs <- append(bdfs,list(df = df))
}

budget_df <- do.call(rbind, bdfs)

budget_df$est_CVE <- sprintf("%02d", budget_df$ID_ENTIDAD)
budget_df$mun_CVE <- sprintf("%03d", budget_df$ID_MUNICIPIO)
budget_df$mun_id <- paste0(budget_df$est_CVE,budget_df$mun_CVE)

budget_df$PROD_EST <- NULL #get rid of pointless column

# Assuming budget_df is your dataframe
budget_df <- budget_df %>%
  distinct(TEMA, DESCRIPCION_CATEGORIA, VALOR, ANIO, .keep_all = TRUE)

bdf <- subset(budget_df, select = c(mun_id, ANIO, TEMA, CATEGORIA, DESCRIPCION_CATEGORIA, VALOR))
bdf2 <- subset(bdf, DESCRIPCION_CATEGORIA != "Bienes inmuebles")
bdf2$categoria <- paste(bdf2$TEMA,bdf2$CATEGORIA, bdf2$DESCRIPCION_CATEGORIA)
bdf2$categoria <- gsub(" ", ".", bdf2$categoria)
bdf2$DESCRIPCION_CATEGORIA <- NULL
bdf2$CATEGORIA <- NULL
bdf2$TEMA <- NULL

bdf3 <- subset(bdf2, ANIO >= 1995 & ANIO <= 2000)

wide_budget_df <- bdf3 %>%
  pivot_wider(names_from = c(categoria), values_from = VALOR)

# test <- bdf2 |>
#   dplyr::summarise(n = dplyr::n(), .by = c(mun_id, ANIO, categoria)) |>
#   dplyr::filter(n > 1L) 

#subset(bdf2, mun_id == 10015 & ANIO == 2001 & categoria == "Egresos.Otros.servicios.generales")

#test2 <- subset(bdf2, categoria == "Ingresos.Capítulo.Aportaciones.federales.y.estatales")

save(wide_budget_df, file = "~/mexico_RD/full_dataset_mexbudget.Rdata")

long_ing <- subset(bdf2, categoria == "Ingresos.Tema.Total.de.ingresos" | categoria == "Ingresos.Capítulo.Impuestos" | categoria == "Ingresos.Partida.Genérica.Recursos.federales" | categoria == "Ingresos.Capítulo.Participaciones.federales" | categoria == "Ingresos.Capítulo.Aportaciones.federales.y.estatales" | categoria == "Egresos.Concepto.Ayudas.sociales" | categoria == "Egresos.Tema.Total.de.egresos")

ing <- long_ing %>%
  pivot_wider(names_from = c(categoria), values_from = VALOR)

ing <-ing %>% 
  rename(year = ANIO, ing0 = Ingresos.Tema.Total.de.ingresos, imp0 = Ingresos.Capítulo.Impuestos, rec = Ingresos.Partida.Genérica.Recursos.federales, part0 = Ingresos.Capítulo.Participaciones.federales, aport = Ingresos.Capítulo.Aportaciones.federales.y.estatales, ayd0 = Egresos.Concepto.Ayudas.sociales, eg0 = Egresos.Tema.Total.de.egresos)

#summary(ing$part0)

ing$pct_imp0 <- ing$imp0/ing$ing0
ing$pct_part0 <- ing$part0/ing$ing0
ing$pct_ayd0 <- ing$ayd0/ing$eg0

# Define the lags you want to include
lags <- 1:3

# Order the dataframe ing by year
ing <- ing %>% arrange(year)

# Use shift function with multiple lags
setDT(ing)[, paste0("ing", lags) := lapply(lags, function(x) shift(ing0, x, fill = NA, type = "lead")), by = mun_id]

setDT(ing)[, paste0("imp", lags) := lapply(lags, function(x) shift(imp0, x, fill = NA, type = "lead")), by = mun_id]

setDT(ing)[, paste0("pct_imp", lags) := lapply(lags, function(x) shift(pct_imp0, x, fill = NA, type = "lead")), by = mun_id]

setDT(ing)[, paste0("pct_part", lags) := lapply(lags, function(x) shift(pct_part0, x, fill = NA, type = "lead")), by = mun_id]

setDT(ing)[, paste0("pct_ayd", lags) := lapply(lags, function(x) shift(pct_ayd0, x, fill = NA, type = "lead")), by = mun_id]

#test <- subset(ing, mun_id == "01001")

#create change variables
ing$ing_diff1 <- ing$ing1 - ing$ing0
ing$ing_diff2 <- ing$ing2 - ing$ing0
ing$ing_diff3 <- ing$ing3 - ing$ing0

ing$imp_diff1 <- ing$imp1 - ing$imp0
ing$imp_diff2 <- ing$imp2 - ing$imp0
ing$imp_diff3 <- ing$imp3 - ing$imp0

ing$pi_diff1 <- ing$pct_imp1 - ing$pct_imp0
ing$pi_diff2 <- ing$pct_imp2 - ing$pct_imp0
ing$pi_diff3 <- ing$pct_imp3 - ing$pct_imp0

ing$pp_diff1 <- ing$pct_part1 - ing$pct_part0
ing$pp_diff2 <- ing$pct_part2 - ing$pct_part0
ing$pp_diff3 <- ing$pct_part3 - ing$pct_part0

ing$pa_diff1 <- ing$pct_ayd1 - ing$pct_ayd0
ing$pa_diff2 <- ing$pct_ayd2 - ing$pct_ayd0
ing$pa_diff3 <- ing$pct_ayd3 - ing$pct_ayd0

save(ing, file = "~/mexico_RD/ingresos.Rdata")
