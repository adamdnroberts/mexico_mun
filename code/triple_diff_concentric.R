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
neighbors$mun_id.x <- mex_sf$CVE_CONCA[neighbors$row.id]
neighbors$mun_id.y <- mex_sf$CVE_CONCA[neighbors$col.id]

# ids <- as.data.frame(cbind(mex_sf$OID, mex_sf$CVE_CONCA))  # Combine OID and unique municipality ID into a dataframe
# colnames(ids) <- c("df_id", "mun_id")  # Rename columns for clarity

neighbors_merge  <- subset(neighbors, select = c(mun_id.x,mun_id.y))

hello <- merge(neighbors_merge, neighbors_merge, by.x = "mun_id.y", by.y = "mun_id.x")  # Merge the resulting dataframe with IDs by column ID
hello$adjadj <- 1
hello$mun_id.y <- NULL
hello <- hello %>%
  rename(mun_id.y = mun_id.y.y)

neighbors_merge$adjadj <- 0

hello2 <- rbind(neighbors_merge, hello)

hello2 <- hello2 %>%
  group_by(mun_id.x, mun_id.y) %>%
  mutate(adjadj = ifelse(n() > 1, 0, adjadj)) %>%
  ungroup()

hello2 <- hello2 %>%
  distinct(mun_id.x, mun_id.y, adjadj, .keep_all = TRUE)

# Create a list of years
years <- 2006:2012

# Initialize an empty list to store the data frames
pairs_list <- list()

# Loop through each year and create a corresponding data frame
for (year in years) {
  pairs <- hello2
  pairs$year <- year
  pairs_list[[as.character(year)]] <- pairs
}

# Combine all the data frames into one
expanded_pairs <- do.call(rbind, pairs_list)

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

test <- merge(expanded_pairs, asum, by.x = c("mun_id.x","year"), by.y = c("uniqueid","year"), all.y = T)
test <- test %>%
  rename(audit_id = mun_id.x, elec_id = mun_id.y)

test2 <- merge(test, big_df_merge, by.x = c("elec_id","year"), by.y = c("mun_id","year"))
test2$corruptm <- ifelse(test2$corrupt > summary(a67_sum$corrupt)[3],1,0)
test2$corrupt4 <- ifelse(test2$corrupt >= summary(a67_sum$corrupt)[5],1,0)

test2$adj <- ifelse(test2$adjadj == 0,1,0)

test2$pvs <- ifelse(test2$PRI_inc ==1, test2$PRI_change, ifelse(test2$PAN_inc==1,test2$PAN_change,test2$PRI_change))
test2$pvs[is.infinite(test2$pvs)] <- NA

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
m2f <- feols(pvs ~ corrupt4*adj*audit | a_e_id, data = test2)
m3f <- feols(pvs ~ corrupt4*adj*audit | a_e_id + year, data = test2)

etable(m0f,m1f,m2f,m3f)