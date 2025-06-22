library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)

mun_am_stations <- read.csv("~/mexico_mun/raw/mun_am_stations.csv")
mun_am_stations$mun_id <-  paste0(sprintf("%02d", mun_am_stations$CVE_ENT),
                                  sprintf("%03d", mun_am_stations$CVE_MUN))

mun_am <- subset(mun_am_stations, select = c(mun_id, Distintivo))

mun_fm_stations <- read.csv("~/mexico_mun/raw/mun_fm_stations.csv")
mun_fm_stations$mun_id <-  paste0(sprintf("%02d", mun_fm_stations$CVE_ENT),
                                  sprintf("%03d", mun_fm_stations$CVE_MUN))

mun_fm <- subset(mun_fm_stations, select = c(mun_id, Distintivo))

test <- rbind(mun_am, mun_fm)
test$values <- 1

test_wide <- test %>%
  pivot_wider(names_from = Distintivo, values_from = values)

test_wide[is.na(test_wide)] <- 0

library(vegan)  # For the vegdist function

# Calculate the Jaccard similarity matrix
jaccard_sim <- 1 - as.matrix(vegdist(test_wide[,-1], method = "jaccard", na.rm = T))

row.names(jaccard_sim) <- test_wide$mun_id

# Adding "md" in front of each element
columns <- paste0("js_", test_wide$mun_id)
colnames(jaccard_sim) <- columns

js_mat <- as.data.frame(jaccard_sim)
js_mat$mun_id <- rownames(js_mat)

# Pivot longer 
js_mat_long <- js_mat %>% 
  pivot_longer( cols = -mun_id, names_to = "ref_mun_id", values_to = "js" )

js_mat_long$ref_mun_id <- gsub("js_", "", js_mat_long$ref_mun_id)

js <- subset(js_mat_long, ref_mun_id != mun_id)

save(js, file = "~/mexico_mun/data/jaccard_similarity_AM_FM.Rdata")
