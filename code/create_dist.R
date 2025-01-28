library(dplyr)
library(data.table)
library(geosphere)

load("~/mexico_RD/data/mun_ll.Rdata")

# Pre-allocate a matrix
n <- nrow(mun_ll)
temp_matrix <- matrix(NA, n^2, 3)
counter <- 1

# Start time
start_time <- Sys.time()
for (i in 1:n) {
  if (i %% 125 == 0) { print(round(i/2427,2)) }
  
  for (j in 1:n) {
    
    if (i == j) {next}
    if (is.na(mun_ll$LAT_DECIMAL[i]) | is.na(mun_ll$LON_DECIMAL[i]) | is.na(mun_ll$LAT_DECIMAL[j]) | is.na(mun_ll$LON_DECIMAL[j])) { next }
    d <- sqrt((mun_ll$LAT_DECIMAL[j] - mun_ll$LAT_DECIMAL[i])^2 + (mun_ll$LON_DECIMAL[j] - mun_ll$LON_DECIMAL[i])^2)
    temp_matrix[counter, ] <- c(mun_ll$mun_full[i], mun_ll$mun_full[j], d)
  
    counter <- counter + 1
  }
} 
stop_time <- Sys.time()
print(stop_time - start_time)

temp_matrix <- temp_matrix[rowSums(is.na(temp_matrix)) != ncol(temp_matrix), ]

# Convert to dataframe
dist_df <- as.data.table(temp_matrix, stringsAsFactors = FALSE)
colnames(dist_df) <- c("mun", "neighbor", "d")

#convert d to numeric
dist_df$d <- as.numeric(dist_df$d)

save(dist_df, file = "~/mexico_RD/pairwise_distances.Rdata")

summary(dist_df$d)

test <- subset(dist_df, d <= 1)
length(unique(test$mun))

min_ds <- dist_df %>% group_by(mun) %>% summarize(min = min(d))

# keep only closest 5 neighbors
vec5 <- dist_df %>%
  group_by(mun) %>%
  slice_min(order_by = d, n = 5)

save(vec5, file = "~/mexico_RD/vec5.Rdata")

# keep only those within 1.5 euclidean coordinates

near <- subset(dist_df, d < 1.5)

save(near, file = "~/mexico_RD/near.Rdata")

## REDO DISTANCE USING KM ##
n <- nrow(mun_ll)
temp_matrix <- matrix(NA, n^2, 3)
counter <- 1

# Start time
start_time <- Sys.time()
for (i in 1:n) {
  if (i %% 125 == 0) { print(round(i/2427,2)) }

  for (j in 1:n) {
    
    if (i == j) {next}
    if (is.na(mun_ll$LAT_DECIMAL[i]) | is.na(mun_ll$LON_DECIMAL[i]) | is.na(mun_ll$LAT_DECIMAL[j]) | is.na(mun_ll$LON_DECIMAL[j])) { next }
    dH <- distHaversine(c(mun_ll$LON_DECIMAL[i],mun_ll$LAT_DECIMAL[i]), c(mun_ll$LON_DECIMAL[j],mun_ll$LAT_DECIMAL[j]))/1000
    temp_matrix[counter, ] <- c(mun_ll$mun_full[i], mun_ll$mun_full[j], dH)
    
    counter <- counter + 1
  }
} 
stop_time <- Sys.time()
print(stop_time - start_time)

temp_matrix <- temp_matrix[rowSums(is.na(temp_matrix)) != ncol(temp_matrix), ]

# Convert to dataframe
dH_df <- as.data.table(temp_matrix, stringsAsFactors = FALSE)
colnames(dH_df) <- c("mun", "neighbor", "dH")

dH_df$dH <- as.numeric(dH_df$dH)

hist(dH_df$dH)
summary(dH_df$dH)

save(dH_df, file = "~/mexico_RD/pairwise_distances_km.Rdata")
