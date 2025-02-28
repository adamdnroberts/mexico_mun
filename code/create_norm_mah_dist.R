load("~/mexico_mun/data/mah_dist_full.Rdata")

mah_dist <- subset(mah_dist_full, select = c(mun_id, prev_PAN_pct, pop, 
                                             income,
                                             transfers_pct, 
                                             LAT_DECIMAL, LON_DECIMAL,
                                             #depMR_PAN_pct, 
                                             #depPR_PAN_pct, 
                                             #senate_PAN_pct, 
                                             pres_PAN_pct, 
                                             pop_rural,
                                             gov
))

#options(scipen = 999)
mah_dist <- na.omit(mah_dist)
S <- var(mah_dist[, 2:ncol(mah_dist)])

mat <- as.matrix(mah_dist[, 2:ncol(mah_dist)])
qr(mat)$rank # no linear dependence

# Calculating Mahalanobis distance
n <- nrow(mat)
temp_matrix <- matrix(NA, n, n)

# Start time
start_time <- Sys.time()
for (i in 1:n) {
  temp_matrix[, i] <- mahalanobis(mat, mat[i, ], cov = S, tol = 1e-50)
}
stop_time <- Sys.time()
print(stop_time - start_time)

row.names(temp_matrix) <- mah_dist$mun_id

# Adding "md" in front of each element
columns <- paste0("md_", mah_dist$mun_id)
colnames(temp_matrix) <- columns

md_mat <- as.data.frame(temp_matrix)

#normalizing weights
md_raw_wt <- 1/md_mat
md_raw_wt[md_raw_wt == Inf | md_raw_wt == -Inf] <- 0

md_norm <- t(apply(md_raw_wt, 1, function(x) x / sum(x)))

maxes <- apply(as.data.frame(md_norm), 1, max)

max_value <- max(md_norm) # Find the maximum value in the entire matrix 
row_index <- which(apply(md_norm, 1, function(x) max_value %in% x))
#row.names(md_norm)[152]

md_norm <- as.data.frame(md_norm)
md_norm$mun_id <- rownames(md_norm)

# Pivot longer 
md_norm_long <- md_norm %>% 
  pivot_longer( cols = -mun_id, names_to = "ref_mun_id", values_to = "mah_d" )

md_norm_long$ref_mun_id <- gsub("md_", "", md_norm_long$ref_mun_id)

md_norm_final <- subset(md_norm_long, mah_d > 0)

save(md_norm_final, file = "~/mexico_mun/data/md_normalized.Rdata")
