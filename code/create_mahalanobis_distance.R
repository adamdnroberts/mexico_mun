library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)

load("~/mexico_mun/data/full_dataset_mexbudget.Rdata")
load("~/mexico_mun/data/full_dataset_mexelec.Rdata")

mex_sf <- read_sf("~/mexico_mun/raw/mun1995shp/Municipios_1995.shp")

elec_full <- subset(big_df, year >=1995 & year <= 1997)
elec <- subset(elec_full, select = c(mun_id, prev_PAN_pct))

# Keep only the earliest row for each mun_id 
# elec_earliest <- elec %>% group_by(mun_id) %>% slice_min(order_by = year, with_ties = FALSE) %>% ungroup()
# no states with multiple elections in this period, so no need for this

load("~/mexico_mun/data/mexpop.Rdata")
pop_full <- subset(mun_cen_final, year == 1995)
pop <- subset(pop_full, select = c(mun_id, pop))
pop <- pop %>% mutate(pop = gsub(",", "", pop))
pop$pop <- as.numeric(pop$pop)

subset(pop_full, mun_id == "02001")


bud_all95 <- subset(wide_budget_df, ANIO == 1995)

budget <- subset(bud_all95, select = c(mun_id, Ingresos.Tema.Total.de.ingresos, Ingresos.Capítulo.Participaciones.federales, Egresos.Capítulo.Deuda.pública,Ingresos.Concepto.Impuesto.sobre.los.Ingresos))
colnames(budget) <- c("mun_id","income","transfers","debt","taxes")

budget <- budget[, -c(4, 5)]

# Calculate the area
mex_sf$area <- st_area(mex_sf)
#create municipio ID
mex_sf$mun_id <- paste0(mex_sf$CVE_ENT,mex_sf$CVE_MUN)
mex_sf <- mex_sf %>% mutate(mun_id = gsub(" ", "", mun_id))

#size <- as.data.frame(subset(mex_sf, select = c(mun_id,area)))
size <- as.data.frame(subset(mex_sf, select = c(mun_id,COUNT)))
#size$area <- as.numeric(size$area)
size$geometry <- NULL

load("~/mexico_mun/data/mun_ll.Rdata")
dist <- subset(mun_ll, select = c(mun_full, LAT_DECIMAL, LON_DECIMAL))
dist <- dist %>% rename(mun_id = mun_full)

load("~/mexico_mun/data/federal_elections_w_key.Rdata")
load("~/mexico_mun/data/rural_mun_pop.Rdata")

#THE MERGE#

merge1 <- merge(elec, pop, by = "mun_id")
merge2 <- merge(merge1, budget, by = "mun_id")
merge3 <- merge(merge2, size, by = "mun_id")
merge4 <- merge(merge3, dist, by = "mun_id")
merge5 <- merge(merge4, fed_elec_w_key, by = "mun_id")
mah_dist_full <- merge(merge5, rural, by = "mun_id")

PAN_governors <- sprintf("%02d", c(2,8,11,14))
mah_dist_full$gov <- ifelse(mah_dist_full$estado %in% PAN_governors, 1, 0)
mah_dist_full$transfers_pct <- mah_dist_full$transfers/mah_dist_full$income

save(mah_dist_full, file = "~/mexico_mun/data/mah_dist_full.Rdata")


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

#save(md_mat, file = "~/mexico_mun/data/md_not_normalized.Rdata")

# #data viz
# create_plot_adj <- function(municipio, full = FALSE, full_country = FALSE) {
#   if (full_country == F & full == T) {
#     estado <- substr(municipio, 1, 2)
#     adj_plot <- subset(mex_sf, CVE_ENT == estado)
#     adj_list <- st_intersects(adj_plot, adj_plot, sparse = T)
#   }
#   else {adj_plot <- mex_sf}
#   
#   adj_plot$neighbors <- NA
#   mun_vec <- md_mat[,mah_dist$mun_id == municipio]
#   muns <- mah_dist$mun_id[mun_vec <= sort(mun_vec)[6]]
#   adj_plot$neighbors[adj_plot$mun_id %in% muns] <- "neighbor"
#   adj_plot$neighbors[adj_plot$mun_id == municipio] <- "municipality"
#   
#   print(muns)
#   
#   # Find the bounding box for the "municipio"
#   bbox <- adj_plot %>%
#     filter(!is.na(neighbors)) %>%
#     st_bbox()
#   
#   if (full == FALSE) {
#     plot <- ggplot(adj_plot) +
#       geom_sf(color = "black", aes(geometry = geometry, fill = neighbors)) +
#       scale_fill_manual(values = c("neighbor" = "blue", "municipality" = "red"), na.value = "white") +
#       coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"])) +
#       theme_void()
#   }
#   else {
#     plot <- ggplot(adj_plot) +
#       geom_sf(color = "black", aes(geometry = geometry, fill = neighbors)) +
#       theme_void() +
#       theme(legend.position = "none")
#   }
#   return(plot)
# }
# 
# #Densely populated
# #create_plot_adj(municipio = "09007") #CDMX don't have in dataset
# create_plot_adj(municipio = "21114") #Puebla
# plot <- create_plot_adj(municipio = "19039") #Monterrey
# create_plot_adj(municipio = "14039") #Guadalajara
# 
# 
# #
# #median populations
# create_plot_adj(municipio = "08021")
# 
# #sparsely populated
# create_plot_adj(municipio = "14011") #potentially problematic? Atengo, Jalisco
# 
# #check distances for sparsely populated
# refs <- c("08002", "08016", "08021", "08045", "08054", "08062")


# geo_dist <- function(municipio) {
#   df <- subset(dist_df, mun == municipio)
#   
#   mun_vec <- md_mat[,mah_dist$mun_id == municipio]
#   muns <- mah_dist$mun_id[mun_vec <= sort(mun_vec)[6]]
#   
#   
#   for (i in 1:length(muns)) {
#     ref_d <- round(df$d[df$neighbor == muns[i]],2)
#     print(paste("Distance from",municipio,"to",muns[i],"is",ref_d))
#   }
# }
# 
# geo_dist("08021")
# geo_dist("14011")

#try not normalized data


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
