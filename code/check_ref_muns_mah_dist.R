load("~/mexico_mun/data/rdd_mah_dist.Rdata")

mah_dist <- subset(mah_dist_full, select = c(mun_id, prev_PAN_pct, pop, income,
                                             transfers_pct, 
                                             LAT_DECIMAL, LON_DECIMAL, 
                                             depMR_PAN_pct, 
                                             depPR_PAN_pct, 
                                             senate_PAN_pct, 
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

source("~/mexico_mun/code/process_elec_df.R")

#function
just_muns <- function(municipio, full = FALSE, full_country = FALSE) {
  if (full_country == F & full == T) {
    estado <- substr(municipio, 1, 2)
    adj_plot <- subset(mex_sf, CVE_ENT == estado)
    adj_list <- st_intersects(adj_plot, adj_plot, sparse = T)
  }
  else {adj_plot <- mex_sf}
  
  adj_plot$neighbors <- NA
  mun_vec <- md_mat[,mah_dist$mun_id == municipio]
  muns <- mah_dist$mun_id[mun_vec <= sort(mun_vec)[6]]
  adj_plot$neighbors[adj_plot$mun_id %in% muns] <- "neighbor"
  adj_plot$neighbors[adj_plot$mun_id == municipio] <- "municipality"
  
  print(muns)
}

#test muns
just_muns(municipio = "24047")
just_muns(municipio = "19009")



#Big cities
just_muns(municipio = "21114") #Puebla
just_muns(municipio = "19039") #Monterrey
just_muns(municipio = "14039") #Guadalajara