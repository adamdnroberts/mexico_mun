library(dplyr)
library(tidyr)

##LOOPS##

bdfs <- list()
for (year in 1989:2023){
  df <- read.csv(paste0("~/mexico_mun/raw/efipem_municipal_csv/conjunto_de_datos/efipem_municipal_anual_tr_cifra_",year,".csv"))
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

wide_budget_df <- budget_df %>%
  pivot_wider(names_from = c(DESCRIPCION_CATEGORIA), values_from = VALOR)

# Assuming budget_df is your dataframe
budget_df <- budget_df %>%
  distinct(TEMA, DESCRIPCION_CATEGORIA, VALOR, ANIO, .keep_all = TRUE)

bdf <- subset(budget_df, select = c(mun_id, ANIO, TEMA, CATEGORIA, DESCRIPCION_CATEGORIA, VALOR))
bdf2 <- budget_df #subset(bdf, DESCRIPCION_CATEGORIA != "Bienes inmuebles")
bdf2$categoria <- paste(bdf2$TEMA,bdf2$CATEGORIA, bdf2$DESCRIPCION_CATEGORIA)
bdf2$categoria <- gsub(" ", ".", bdf2$categoria)
bdf2$DESCRIPCION_CATEGORIA <- NULL
bdf2$CATEGORIA <- NULL
bdf2$TEMA <- NULL

bdf3 <- bdf2 %>%
  group_by(mun_id,ANIO,categoria) %>%
  summarize(VALOR_total = sum(VALOR))
  

wide_budget_df <- bdf3 %>%
  pivot_wider(names_from = c(categoria), values_from = VALOR_total)

save(wide_budget_df, file = "~/sismos_y_media/data/full_dataset_efipem.Rdata")

