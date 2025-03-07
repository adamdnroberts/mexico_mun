library(sf)
library(dplyr)
library(ggplot2)
library(fixest)

audits <- read.csv("~/mexico_mun/raw/ueaa046_replication_package/audits_sample.csv")

audits$uniqueid <- sprintf("%05d", audits$uniqueid)

mex_sf <- read_sf("~/mexico_mun/raw/mun2005shp/Municipios_2005.shp")  # Read the shapefile

asum <- audits %>%
  group_by(uniqueid, year) %>%
  summarise(audit = mean(audit), corrupt = mean(spent_unauthorized_sub), PRI_inc = mean(PRI_inc), PAN_inc = mean(PAN_inc), PRD_inc = mean(PRD_inc), year = mean(year) )

# test <- merge(mex_sf, a67_sum, by.x = "CVE_CONCA", by.y = "uniqueid", all.x = T)
# 
# test$audit <- as.factor(test$audit)
# 
# plot <- ggplot(test) +
#   geom_sf(color = "black", aes(geometry = geometry, fill = audit)) +
#   theme_void()
# plot

#create adjacent dataset
adj_list <- st_intersects(mex_sf, mex_sf, sparse = T)  # Find intersecting (neighboring) municipalities
full_neighbors <- as.data.frame(adj_list)  # Convert the adjacency list to a dataframe

neighbors <- subset(full_neighbors, row.id != col.id)  # Remove self-neighbor pairs (where row.id == col.id)

ids <- as.data.frame(cbind(mex_sf$OID, mex_sf$CVE_CONCA))  # Combine OID and unique municipality ID into a dataframe
colnames(ids) <- c("df_id", "mun_id")  # Rename columns for clarity

merge_neighbors <- merge(ids, neighbors, by.x = "df_id", by.y = "row.id")  # Merge IDs with neighbors by row ID
merge_neighbors2 <- merge(ids, merge_neighbors, by.x = "df_id", by.y = "col.id")  # Merge the resulting dataframe with IDs by column ID

load("~/mexico_RD/full_dataset_mexelec.Rdata")

# Find coalitions that contain parties
prd_columns <- grep("PRD", names(big_df), value = TRUE)
pan_columns <- grep("PAN", names(big_df), value = TRUE)
pri_columns <- grep("PRI", names(big_df), value = TRUE)

# Sum the values of these columns row-wise
big_df$PRD_sum <- rowSums(big_df[, prd_columns], na.rm = TRUE)
big_df$PAN_sum <- rowSums(big_df[, pan_columns], na.rm = TRUE)
big_df$PRI_sum <- rowSums(big_df[, pri_columns], na.rm = TRUE)

big_df$PRD_pct <- big_df$PRD_sum/big_df$TOTAL
big_df$PAN_pct <- big_df$PAN_sum/big_df$TOTAL
big_df$PRI_pct <- big_df$PRI_sum/big_df$TOTAL

big_df <- big_df %>%
  arrange(year) %>%
  mutate(PRI_pct_lag = lag(PRI_pct, n = 1), PAN_pct_lag = lag(PAN_pct, n = 1), PRD_pct_lag = lag(PRD_pct, n = 1))

big_df$PRI_change <- big_df$PRI_pct_lag - big_df$PRI_pct
big_df$PAN_change <- big_df$PAN_pct_lag - big_df$PAN_pct
big_df$PRD_change <- big_df$PRD_pct_lag - big_df$PRD_pct

big_df_merge <- subset(big_df, select = c(mun_id,year,PRI_change,PAN_change,PRD_change))

unique_pairs <- expand.grid(audit_id = unique(asum$uniqueid), elec_id = unique(big_df$mun_id))
# Extract the first two characters of audit_id and elec_id
unique_pairs$audit_id_prefix <- substr(unique_pairs$audit_id, 1, 2)
unique_pairs$elec_id_prefix <- substr(unique_pairs$elec_id, 1, 2)

# Subset the data frame to keep only pairs with matching prefixes
filtered_pairs <- subset(unique_pairs, audit_id_prefix == elec_id_prefix)

filtered_pairs <- filtered_pairs[, !(names(filtered_pairs) %in% c("audit_id_prefix", "elec_id_prefix"))]

# Create a list of years
years <- 2006:2012

# Initialize an empty list to store the data frames
pairs_list <- list()

# Loop through each year and create a corresponding data frame
for (year in years) {
  pairs <- filtered_pairs
  pairs$year <- year
  pairs_list[[as.character(year)]] <- pairs
}

# Combine all the data frames into one
expanded_pairs <- do.call(rbind, pairs_list)

new <- merge(expanded_pairs, merge_neighbors2, by.x = c("audit_id","elec_id"), by.y = c("mun_id.x","mun_id.y"), all.x = T)

test <- merge(new, asum, by.x = c("audit_id","year"), by.y = c("uniqueid","year"), all.y = T)
test2 <- merge(test, big_df_merge, by.x = c("elec_id","year"), by.y = c("mun_id","year"))
test2$corruptm <- ifelse(test2$corrupt > summary(a67_sum$corrupt)[3],1,0)
test2$corrupt4 <- ifelse(test2$corrupt >= summary(a67_sum$corrupt)[5],1,0)

test2$adj <- ifelse(!is.na(test2$df_id),1,0)

test2$pvs <- ifelse(test2$PRI_inc ==1, test2$PRI_change, ifelse(test2$PAN_inc==1,test2$PAN_change,test2$PRI_change))
test2$pvs[is.infinite(test2$pvs)] <- NA

m <- feols(pvs ~ corruptm + adj + audit + corruptm*adj + corruptm*audit + adj*audit + corruptm*adj*audit | audit_id + elec_id, data = test2)
summary(m)

test2$a_e_id <- paste0(test2$audit_id, test2$elec_id)
test2$audit_state <- substr(test2$audit_id, 1, 2)
test2$elec_state <- substr(test2$elec_id, 1, 2)

#above median corruption
m0m <- feols(pvs ~ corruptm + adj + audit + corruptm*adj + corruptm*audit + adj*audit + corruptm*adj*audit, data = test2)
m1m <- feols(pvs ~ corruptm*adj*audit | audit_id + elec_id, data = test2)
m2m <- feols(pvs ~ corruptm*adj*audit | a_e_id, data = test2)
m3m <- feols(pvs ~ corruptm*adj*audit | a_e_id + year, data = test2)

etable(m0m,m1m,m2m,m3m)

#top quartile corruption
m0f <- feols(pvs ~ corrupt4 + adj + audit + corrupt4*adj + corrupt4*audit + adj*audit + corrupt4*adj*audit, data = test2)
m1f <- feols(pvs ~ corrupt4*adj*audit | audit_id + elec_id, data = test2)
m1.2f <- feols(pvs ~ corrupt4*adj*audit | elec_id + audit_id, data = test2)
m2f <- feols(pvs ~ corrupt4*adj*audit | a_e_id, data = test2)
m3f <- feols(pvs ~ corrupt4*adj*audit | a_e_id + year, data = test2)

etable(m0f,m1f,m1.2f,m2f,m3f)
