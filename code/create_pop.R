library(dplyr)
#library(stringr)
library(tidyr)

#data from here: https://www.inegi.org.mx/programas/ccpv/1995/#tabulados
census <- read.csv("C:/Users/adamd/Documents/mexico_mun/raw/INEGI_exporta_6_1_2025_11_28_56.csv", comment.char="#")
colnames(census)[1] <- "mun_id"
colnames(census)[2] <- "mun_nom"

#remove blank rows
census <- census %>% filter(mun_nom != "")

# Remove the last two rows 
census <- census %>% slice(1:(n() - 2))

#remove spaces from IDs
census <- census %>% mutate(mun_id = gsub(" ", "", mun_id))

# Create different datasets for muns vs estados
est_cen <- census %>% filter(nchar(mun_id) < 5)
mun_cen <- census %>% filter(nchar(mun_id) == 5)

# Pivot longer 
mun_cen_long <- mun_cen %>% pivot_longer(cols = -c(mun_id, mun_nom), names_to = "year", values_to = "pop")

#remove X from years
mun_cen_final <- mun_cen_long %>% mutate(year = gsub("X", "", year))

# Create a new column for estado id (first two characters of mun_id)
mun_cen_final <- mun_cen_final %>% mutate(est_id = substr(mun_id, 1, 2))

# Group by est_id, year and count rows
count_by_est <- mun_cen_final %>% group_by(est_id,year) %>% summarise(count = n()) #looks good

save(mun_cen_final, file = "~/mexico_RD/mexpop.Rdata")



