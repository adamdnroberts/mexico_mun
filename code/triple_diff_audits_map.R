library(sf)
library(dplyr)
library(ggplot2)
library(fixest)

audits <- read.csv("~/mexico_mun/raw/ueaa046_replication_package/audits_sample.csv")

audits$uniqueid <- sprintf("%05d", audits$uniqueid)

mex_sf <- read_sf("~/mexico_mun/raw/mun2005shp/Municipios_2005.shp")  # Read the shapefile

asum <- audits %>%
  group_by(uniqueid, year) %>%
  summarise(audit = mean(audit), corrupt = mean(spent_unauthorized_sub), PRI_inc = mean(PRI_inc), PAN_inc = mean(PAN_inc), PRD_inc = mean(PRD_inc), year = mean(year), state = mean(state) )

asum06 <- subset(asum, year == 2006 | year == 2007)

audit_map <- merge(mex_sf, asum06, by.x = "CVE_CONCA", by.y = "uniqueid", all.x = T)

audit_map$audit <- as.factor(audit_map$audit)

plot <- ggplot(audit_map) +
  geom_sf(color = "black", aes(geometry = geometry, fill = audit)) +
  theme_void()
plot


ag <- subset(asum, state == 1)
