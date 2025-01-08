#SHP FILES!!!! https://www.inegi.org.mx/temas/mg/default.html#Descargas
#https://r-graph-gallery.com/168-load-a-shape-file-into-r.html

library(sf)
library(ggplot2)
library(dplyr)
library(rdd)
library(rddtools) #for balance of covariates testing

#get neighboring/adjacent municipalities list
mex_sf <- read_sf("~/mexico_RD/mun1995shp/Municipios_1995.shp")  # Read the shapefile
mex_sf$mun_id <- paste0(mex_sf$CVE_ENT, mex_sf$CVE_MUN)  # Create a unique ID for each municipality

adj_list <- st_intersects(mex_sf, mex_sf, sparse = T)  # Find intersecting (neighboring) municipalities
full_neighbors <- as.data.frame(adj_list)  # Convert the adjacency list to a dataframe

neighbors <- subset(full_neighbors, row.id != col.id)  # Remove self-neighbor pairs (where row.id == col.id)

ids <- as.data.frame(cbind(mex_sf$OID, mex_sf$mun_id))  # Combine OID and unique municipality ID into a dataframe
colnames(ids) <- c("df_id", "mun_id")  # Rename columns for clarity

merge_neighbors <- merge(ids, neighbors, by.x = "df_id", by.y = "row.id")  # Merge IDs with neighbors by row ID
merge_neighbors2 <- merge(ids, merge_neighbors, by.x = "df_id", by.y = "col.id")  # Merge the resulting dataframe with IDs by column ID

#load elections data
load("~/mexico_RD/full_dataset_mexelec.Rdata")

df <- subset(big_df, year>=1994 & year <= 1999 & estado!="Tlaxcala" & (p1_name == "PRI" | p2_name == "PRI") & (p1_name == "PAN" | p2_name == "PAN"))
df$mun_id <- gsub(" ", "", df$Municipio)

length(unique(df$mun_id))

#figure out missing municipalities
shape_muns <- unique(merge_neighbors2$mun_id.x)
elec_muns <- unique(df$mun_id)

missing_muns1 <- setdiff(shape_muns,elec_muns)
missing_muns2 <- setdiff(elec_muns,shape_muns)

big_df$mun_id <- gsub(" ", "", big_df$Municipio)
full_elec_muns <- unique(big_df$mun_id)
missing_muns3 <- setdiff(full_elec_muns,shape_muns)
missing_muns4 <- setdiff(shape_muns,full_elec_muns)


#why missing from shape file?
test <- subset(df, mun_id %in% missing_muns2)
summary(test$PAN_pct) #all NAs, so we're good

#why missing from CIDAC?
mex_sf$missing <- as.factor(ifelse(mex_sf$mun_id %in% missing_muns1,1,0))

ggplot(mex_sf) +
  geom_sf(color = "black", aes(geometry = geometry, fill=missing)) +
  theme_void()



# unique_pairs <- df %>%
#   distinct(year, mun_id)

#create smaller datasets for merge
neighbors_PAN <- subset(df, select = c(year, mun_id, next_PAN_pct, PAN_pct, estado))
main_mun_PAN <- subset(df, select = c(year, mun_id, PAN_pct, estado))

#merge datasets using adjacent municipalities index
neighbors2 <- merge(merge_neighbors2,neighbors_PAN, by.x = c("mun_id.y"), by.y = c("mun_id"))
neighbors2 <- neighbors2 %>% rename(vec_id = mun_id.y, vec_PAN_pct = PAN_pct)

df_rdd <- merge(main_mun_PAN,neighbors2, by.x = c("mun_id"), by.y = c("mun_id.x"))
df_rdd <- df_rdd %>% 
  rename(main_year = year.x, vec_year = year.y) %>% 
  select(-df_id, -df_id.y)

#average vecino, adjacent neighbors
adj <- df_rdd %>% 
  group_by(mun_id) %>%
  summarise(PAN_pct = first(PAN_pct), vec_npp = mean(next_PAN_pct, na.rm = T), main_year = first(main_year), main_estado = first(estado.x), vec_pp = mean(vec_PAN_pct, na.rm = T))
  
DCdensity(adj$PAN_pct, cutpoint = 0.5)
abline(v = 0.5, col = "red", lwd = 2)
title(x = "PAN vote share")

rd1 <- RDestimate(vec_npp ~ PAN_pct, cutpoint = 0.5, data = adj)
summary(rd1)

lm <- lm(vec_npp ~ PAN_pct + vec_pp + main_estado + main_year, data = adj)
summary(lm)

#probably good that this doesn't show an effect?
ggplot(adj, aes(x = PAN_pct, y = vec_npp)) +
  geom_point(alpha = 0.5) +
  #geom_smooth() +
  labs(title = "Regression Discontinuity Design",
       x = "PAN vote share, t",
       y = "Neighbors Average PAN vote share, t+1") +
  theme_minimal()

#closest election only, for those with zero or 1 PAN wins, effect of extensive margin

# Filter groups with at most one value of PAN_pct > 0.5, then keep only largest value
ext_margin <- df_rdd %>%
  group_by(vec_id) %>%
  filter(sum(PAN_pct > 0.5) <= 1) %>%
  slice(which.max(PAN_pct)) %>%
  ungroup()

DCdensity(ext_margin$PAN_pct, cutpoint = 0.5)
title(x = "PAN vote share")

rd1_ext <- RDestimate(next_PAN_pct ~ PAN_pct, cutpoint = 0.5, data = ext_margin)
summary(rd1_ext)

plot(rd1_ext, gran = 400, range = c(0.45,0.55))
abline(v = 0.5, col = "black", lwd = 2, lty = "solid")
title(x = "neighbor PAN vote share, t", y = "PAN vote share, t+1")

#closest election only, effect of having an additional PAN win
df_only_closest <- df_rdd %>%
  group_by(vec_id) %>%
  slice(which.min(abs(PAN_pct - 0.5)))

DCdensity(df_only_closest$PAN_pct, cutpoint = 0.5)
title(x = "PAN vote share")

rd1 <- RDestimate(next_PAN_pct ~ PAN_pct, cutpoint = 0.5, data = df_only_closest)
summary(rd1)

plot(rd1, gran = 20, range = c(0.40,0.6)) #this plot does not show the effect estimated from RDestimate
abline(v = 0.5, col = "grey", lwd = 2, lty = "dashed")
title(x = "neighbor PAN vote share, t", y = "PAN vote share, t+1")

rd2 <- RDestimate(next_PAN_pct ~ PAN_pct | estado.x + estado.y, cutpoint = 0.5, data = df_only_closest) #RDestimate does something weird when you add covariates
summary(rd2)
