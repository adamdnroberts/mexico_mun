library(readxl)
library(dplyr)

#UNZIP FILES
files = c("01_aguascalientes_1995_iter_xls.zip", "02_baja_california_1995_iter_xls.zip", "03_baja_california_sur_1995_iter_xls.zip", "04_campeche_1995_iter_xls.zip", "05_coahuila_1995_iter_xls.zip", "06_colima_1995_iter_xls.zip", "07_chiapas_1995_iter_xls.zip", "08_chihuahua_1995_iter_xls.zip", "09_distrito_federal_1995_iter_xls.zip", "10_durango_1995_iter_xls.zip", "11_guanajuato_1995_iter_xls.zip", "12_guerrero_1995_iter_xls.zip", "13_hidalgo_1995_iter_xls.zip", "14_jalisco_1995_iter_xls.zip", "15_mexico_1995_iter_xls.zip", "16_michoacan_1995_iter_xls.zip", "17_morelos_1995_iter_xls.zip", "18_nayarit_1995_iter_xls.zip", "19_nuevo_leon_1995_iter_xls.zip", "20_oaxaca_1995_iter_xls.zip", "21_puebla_1995_iter_xls.zip", "22_queretaro_1995_iter_xls.zip", "23_quintana_roo_1995_iter_xls.zip", "24_san_luis_potosi_1995_iter_xls.zip", "25_sinaloa_1995_iter_xls.zip", "26_sonora_1995_iter_xls.zip", "27_tabasco_1995_iter_xls.zip", "28_tamaulipas_1995_iter_xls.zip", "29_tlaxcala_1995_iter_xls.zip", "30_veracruz_1995_iter_xls.zip", "31_yucatan_1995_iter_xls.zip", "32_zacatecas_1995_iter_xls.zip")

# for (i in 1:length(files)) {
#   unzip(paste0("~/mexico_mun/raw/rural/",files[i]), exdir = "~/mexico_mun/data/rural")
# }

#CREATE % RURAL

est1 <- read_excel("~/mexico_mun/data/rural/ITER_01XLS95.xls")

est1 <- est1[!grepl("Total de la entidad", est1$NOM_MUN), ]
est1 <- est1[!grepl("TOTAL MUNICIPAL", est1$NOM_LOC), ]


rural1 <- est1 %>%
  group_by(MUN) %>%
  summarize(
    rural = sum(POBTOTAL < 2500, na.rm = T),
    urban = sum(POBTOTAL >= 2500, na.rm = T)
  )

rural1$pop_rural <- rural1$rural/(rural1$rural+rural1$urban)

#Function#

process_data <- function(number) {
  # Construct the file path dynamically
  file_path <- paste0("~/mexico_mun/data/rural/ITER_", sprintf("%02d", number), "XLS95.xls")
  
  # Read the Excel file
  est <- read_excel(file_path)
  
  # Filter out rows with specific patterns
  est <- est[!grepl("Total de la entidad", est$NOM_MUN), ]
  est <- est[!grepl("TOTAL MUNICIPAL", est$NOM_LOC), ]
  est$POBTOTAL <- as.numeric(est$POBTOTAL)
  
  #Summarize data
  rural <- est %>%
    group_by(MUN) %>%
    summarize(
      estado = sprintf("%02d", number),
      rural = sum(POBTOTAL[POBTOTAL < 2500], na.rm = TRUE),
      urban = sum(POBTOTAL[POBTOTAL >= 2500], na.rm = TRUE),
      total = sum(POBTOTAL, na.rm = T)
    )
  
  # Calculate the rural population proportion
  rural$pop_rural <- rural$rural / (rural$rural + rural$urban)
  
  return(rural)
}

# LOOP WITH FUNCTION #
rural <- process_data(1)

for (i in 2:length(files)) {
  rural_new <- process_data(i)
  rural <- rbind.data.frame(rural,rural_new)
}

rural$mun_id <- paste0(rural$estado,rural$MUN)

save(rural, file = "C:/Users/adamd/Documents/mexico_mun/data/rural_mun_pop.Rdata")
