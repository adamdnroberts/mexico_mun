library(dplyr)
library(sf)
library(ggplot2)

load("~/mexico_RD/full_dataset_mexbudget.Rdata")
load("~/mexico_RD/full_dataset_mexelec.Rdata")

mex_sf <- read_sf("~/mexico_RD/mun1995shp/Municipios_1995.shp")

elec_full <- subset(big_df, year >=1995 & year <= 1997)
elec <- subset(elec_full, select = c(mun_id, prev_PAN_pct))

# Keep only the earliest row for each mun_id 
#elec_earliest <- elec %>% group_by(mun_id) %>% slice_min(order_by = year, with_ties = FALSE) %>% ungroup()
#no states with multiple elections in this period, so no need for this

load("~/mexico_RD/mexpop.Rdata")
pop_full <- subset(mun_cen_final, year == 1995)
pop <- subset(pop_full, select = c(mun_id, pop))
pop <- pop %>% mutate(pop = gsub(",", "", pop))
pop$pop <- as.numeric(pop$pop)

subset(pop_full, mun_id == "02001")


bud_all95 <- subset(wide_budget_df, ANIO == 1995)

budget <- subset(bud_all95, select = c(mun_id, Ingresos.Tema.Total.de.ingresos))
colnames(budget)[2] <- "income"

# Calculate the area
mex_sf$area <- st_area(mex_sf)
#create municipio ID
mex_sf$mun_id <- paste0(mex_sf$CVE_ENT,mex_sf$CVE_MUN)
mex_sf <- mex_sf %>% mutate(mun_id = gsub(" ", "", mun_id))

#size <- as.data.frame(subset(mex_sf, select = c(mun_id,area)))
size <- as.data.frame(subset(mex_sf, select = c(mun_id,COUNT)))
#size$area <- as.numeric(size$area)
size$geometry <- NULL

load("~/mexico_RD/mun_ll.Rdata")
dist <- subset(mun_ll, select = c(mun_full, LAT_DECIMAL, LON_DECIMAL))
dist <- dist %>% rename(mun_id = mun_full)

#THE MERGE#

merge1 <- merge(elec, pop, by = "mun_id")
merge2 <- merge(merge1, budget, by = "mun_id")
#merge3 <- merge(merge2, size, by = "mun_id")
mah_dist <- merge(merge2,dist, by = "mun_id")

#options(scipen = 999)
mah_dist <- na.omit(mah_dist)
S <- var(mah_dist[,2:6])

mat <- as.matrix(mah_dist[,2:6])
qr(mat)$rank #no linear dependence

#calculating distance

# Pre-allocate a matrix
n <- length(mat[,1])
temp_matrix <- matrix(NA, length(mat[,1]), n)
counter <- 1

# Start time
start_time <- Sys.time()
for (i in 1:n) {
  temp_matrix[,counter] <- mahalanobis(mat, as.matrix(mah_dist[i,2:6]), cov = S, tol=1e-50)
    counter <- counter + 1
} 
stop_time <- Sys.time()
print(stop_time - start_time)

row.names(temp_matrix) <- mah_dist$mun_id

# Adding "md" in front of each element
columns <- sapply(mah_dist$mun_id, function(x) paste0("md_", x))

colnames(temp_matrix) <- columns

md_mat <- as.data.frame(temp_matrix)

#data viz
create_plot_adj <- function(municipio, full = FALSE, full_country = FALSE) {
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
  
  # Find the bounding box for the "municipio"
  bbox <- adj_plot %>%
    filter(!is.na(neighbors)) %>%
    st_bbox()
  
  if (full == FALSE) {
    plot <- ggplot(adj_plot) +
      geom_sf(color = "black", aes(geometry = geometry, fill = neighbors)) +
      scale_fill_manual(values = c("neighbor" = "blue", "municipality" = "red"), na.value = "white") +
      coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"])) +
      theme_void()
  }
  else {
    plot <- ggplot(adj_plot) +
      geom_sf(color = "black", aes(geometry = geometry, fill = neighbors)) +
      theme_void() +
      theme(legend.position = "none")
  }
  return(plot)
}

#Densely populated
#create_plot_adj(municipio = "09007") #CDMX don't have in dataset
create_plot_adj(municipio = "21114") #Puebla
create_plot_adj(municipio = "19039") #Monterrey
create_plot_adj(municipio = "14039") #Guadalajara

#median populations
create_plot_adj(municipio = "08021")

#sparsely populated
create_plot_adj(municipio = "14011") #potentially problematic? Atengo, Jalisco

#check distances for sparsely populated
refs <- c("08002", "08016", "08021", "08045", "08054", "08062")



geo_dist <- function(municipio) {
  df <- subset(dist_df, mun == municipio)
  
  mun_vec <- md_mat[,mah_dist$mun_id == municipio]
  muns <- mah_dist$mun_id[mun_vec <= sort(mun_vec)[6]]
  
  
  for (i in 1:length(muns)) {
    ref_d <- round(df$d[df$neighbor == muns[i]],2)
    print(paste("Distance from",municipio,"to",muns[i],"is",ref_d))
  }
}

geo_dist("08021")
geo_dist("14011")

#normalizing weights
md_raw_wt <- 1/md_mat
md_raw_wt[md_raw_wt == Inf | md_raw_wt == -Inf] <- 0

md_norm <- t(apply(md_raw_wt, 1, function(x) x / sum(x)))

maxes <- apply(as.data.frame(md_norm), 1, max)
summary(maxes)

max_value <- max(md_norm) # Find the maximum value in the entire matrix 
row_index <- which(apply(md_norm, 1, function(x) max_value %in% x))
row.names(md_norm)[152]
