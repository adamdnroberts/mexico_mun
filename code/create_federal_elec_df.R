library(ggplot2)
library(dplyr)
library(sf)

mex94_depPR <- read.csv("~/mexico_mun/data/mex94_drp.csv")
mex94_depPR <- mex94_depPR %>% 
  mutate(across(where(is.character), ~gsub(",", "", .))) %>%
  mutate(across(!Municipio, as.numeric))

mex94_depPR$depPR_PAN_pct <- mex94_depPR$PAN/mex94_depPR$TOTAL

ggplot(mex94_depPR) +
  geom_histogram(aes(x = depPR_PAN_pct)) +
  facet_wrap(~Estado)

# Load the dataset
mex94_senate <- read.csv("~/mexico_mun/data/mex94_senate.csv")

# Apply transformations
mex94_senate <- mex94_senate %>% 
  mutate(across(where(is.character), ~gsub(",", "", .))) %>%
  mutate(across(!Municipio, as.numeric))

# Calculate the percentage
mex94_senate$senate_PAN_pct <- mex94_senate$PAN / mex94_senate$TOTAL

# Plot the histogram
ggplot(mex94_senate) +
  geom_histogram(aes(x = senate_PAN_pct)) +
  facet_wrap(~Estado)

## Load the dataset ##
mex94_depMR <- read.csv("~/mexico_mun/data/mex94_dmr.csv")

# Apply transformations
mex94_depMR <- mex94_depMR %>% 
  mutate(across(where(is.character), ~gsub(",", "", .))) %>%
  mutate(across(!Municipio, as.numeric))

# Calculate the percentage
mex94_depMR$depMR_PAN_pct <- mex94_depMR$PAN / mex94_depMR$TOTAL


## Load the dataset ##
mex94_pres <- read.csv("~/mexico_mun/data/mex94_pres.csv")

# Apply transformations
mex94_pres <- mex94_pres %>% 
  mutate(across(where(is.character), ~gsub(",", "", .))) %>%
  mutate(across(!Municipio, as.numeric))

# Calculate the percentage
mex94_pres$pres_PAN_pct <- mex94_pres$PAN / mex94_pres$TOTAL

# Plot the histogram
ggplot(mex94_pres) +
  geom_histogram(aes(x = pres_PAN_pct)) +
  facet_wrap(~Estado)

#create dataset
depmr <- subset(mex94_depMR, select = c(Estado, Municipio, depMR_PAN_pct))
deppr <- subset(mex94_depPR, select = c(Estado, Municipio, depPR_PAN_pct))
senate <- subset(mex94_senate, select = c(Estado, Municipio, senate_PAN_pct))
pres <- subset(mex94_pres, select = c(Estado, Municipio, pres_PAN_pct))

#merge
fed_elec <- depmr %>%
  left_join(deppr, by = c("Estado", "Municipio")) %>%
  left_join(senate, by = c("Estado", "Municipio")) %>%
  left_join(pres, by = c("Estado", "Municipio"))

fed_elec <- subset(fed_elec, Municipio != " TOTAL POR ENTIDAD FEDERATIVA")

# Create an index grouped by Municipio
fed_elec <- fed_elec %>%
  arrange(Estado,Municipio) %>%
  group_by(Municipio) %>%
  mutate(test = row_number()) %>%
  ungroup()

# Create an index grouped by Municipio
fed_elec <- fed_elec %>%
  group_by(Estado) %>%
  mutate(mun = row_number()) %>%
  ungroup()

# View the updated dataframe
head(fed_elec)

#merge for mun_id
load("~/mexico_mun/data/full_dataset_mexelec.Rdata")

mex_sf <- read_sf("~/mexico_mun/raw/mun1995shp/Municipios_1995.shp")
mex_sf$mun_id <- paste0(mex_sf$CVE_ENT,mex_sf$CVE_MUN)
mex_sf$NOM_MUN <- toupper(iconv(mex_sf$NOM_MUN, from = "UTF-8", to = "ASCII//TRANSLIT"))

new <- subset(mex_sf, NOM_ENT != "Distrito Federal")
new <- subset(new, select = c(mun_id, CVE_ENT, CVE_MUN, NOM_MUN))
new$Estado <- as.numeric(new$CVE_ENT)
new$mun <- as.numeric(new$CVE_MUN)

test <- fed_elec %>%
  left_join(new, by = c("Estado", "mun"))

test$NOM_MUN_up <- toupper(iconv(test$NOM_MUN, from = "UTF-8", to = "ASCII//TRANSLIT"))

check <- subset(test, Municipio != NOM_MUN_up)
check2 <- as.data.frame(cbind(check$Municipio,check$NOM_MUN_up))


ac <- subset(fed_elec, Estado == 1)
new_ac <- subset(new, CVE_ENT == 1)
test <- ac %>%
  left_join(new_ac, by = c("Estado", "mun"))
test$NOM_MUN_up <- toupper(iconv(test$NOM_MUN, from = "UTF-8", to = "ASCII//TRANSLIT"))

check2 <- as.data.frame(cbind(test$Municipio,test$NOM_MUN_up))


save(fed_elec, file = "C:/Users/adamd/Documents/mexico_mun/data/federal_elections.Rdata")
