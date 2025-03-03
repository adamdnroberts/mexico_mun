library(tidyverse)
library(dplyr)
library(sf)
library(sp)

#get neighboring/adjacent municipalities list
mex_sf <- read_sf("~/mexico_mun/raw/mun1995shp/Municipios_1995.shp")  # Read the shapefile
mex_sf$mun_id <- paste0(mex_sf$CVE_ENT, mex_sf$CVE_MUN)  # Create a unique ID for each municipality

mex_sf <- mex_sf %>% arrange(mun_id)

centroids <- st_centroid(mex_sf$geometry)
coords_sf <- st_coordinates(centroids)
distance_matrix <- spDists(coords_sf)

row.names(distance_matrix) <- mex_sf$mun_id

columns <- paste0("md_", mex_sf$mun_id)
colnames(distance_matrix) <- columns

cd_mat <- as.data.frame(distance_matrix)
cd_mat$mun_id <- mex_sf$mun_id

# Pivot longer 
cd_long <- cd_mat %>% 
  pivot_longer( cols = -mun_id, names_to = "ref_mun_id", values_to = "cd" )

cd_long$ref_mun_id <- gsub("md_", "", cd_long$ref_mun_id)

cd <- subset(cd_long, cd > 0)

save(cd, file = "C:/Users/adamd/Documents/mexico_mun/data/centroid_dist.Rdata")
