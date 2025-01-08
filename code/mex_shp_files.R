#SHP FILES!!!! https://www.inegi.org.mx/temas/mg/default.html#Descargas
#https://r-graph-gallery.com/168-load-a-shape-file-into-r.html

library(sf)
library(ggplot2)
library(dplyr)

mex_sf <- read_sf("~/mexico_RD/mun1995shp/Municipios_1995.shp")
mex_sf$mun_full <- paste0(mex_sf$CVE_ENT,mex_sf$CVE_MUN)

adj_matrix <- st_intersects(mex_sf, mex_sf, sparse = F)
adj_list <- st_intersects(mex_sf, mex_sf, sparse = T)

#I think count is number of cities/localities in municipio?

create_plot_adj <- function(municipio, estado = 19, full = FALSE, full_country = FALSE) {
  if (full_country == F) {
  adj_plot <- subset(mex_sf, CVE_ENT == estado)
  adj_list <- st_intersects(adj_plot, adj_plot, sparse = T)
  }
  else {adj_plot <- mex_sf}
  adj_plot$neighbors <- NA
  adj_plot$neighbors[adj_list[[municipio]]] <- "neighbor"
  adj_plot$neighbors[municipio] <- "municipality"
  
  # Find the bounding box for the "municipio"
  bbox <- adj_plot %>%
    filter(neighbors == "neighbor") %>%
    st_bbox()
  
  if (full == FALSE) {
  plot <- ggplot(adj_plot) +
    geom_sf(color = "black", aes(geometry = geometry, fill = neighbors)) +
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

# create_plot_adj(municipio = 19) #San Pedro
# create_plot_adj(municipio = 39) #Monterrey
# create_plot_adj(municipio = 44) #Sabinas Hidalgo
#create_plot_adj(municipio = 26, full = F) #Guadalupe
create_plot_adj(municipio = 1, estado = "01", full = F, full_country = T) #should be 9 neighbors


#Municipio de Acatlan is non-contiguous (Puebla)
#test$three <- ifelse(test$CVE_MUN == "003",1,0)

load("~/mexico_RD/vec5.Rdata")

#function here
create_plot_ll <- function(mun, full = FALSE) {
  vc <- vec5$neighbor[vec5$mun==mun]
  vc <- c(vc, mun)
  
  for_plot <- mex_sf[mex_sf$CVE_ENT == substr(mun, 1, 2),]
  
  for_plot$neighbors <- NA
  for_plot$neighbors[for_plot$mun_full %in% vc] <- "neighbor"
  for_plot$neighbors[for_plot$mun_full == mun] <- "municipality"
  
  bbox <- for_plot %>%
    filter(neighbors == "neighbor") %>%
    st_bbox()
  
  if (full == FALSE) {
  plot <- ggplot(for_plot) +
    geom_sf(color = "black", aes(geometry = geometry, fill = neighbors)) +
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"])) +
    theme_void()
  }
  else {
    plot <- ggplot(for_plot) +
      geom_sf(color = "black", aes(geometry = geometry, fill = neighbors)) +
      theme_void() +
      theme(legend.position = "none")
  }
  
  return(plot)
}

create_plot_ll("19026", full = T)

library(ggplot2)
library(dplyr)

# Find the bounding box for the "municipio"
bbox <- for_plot %>%
  filter(neighbors == "neighbor") %>%
  st_bbox()

# Create the plot with zoom
plot <- ggplot(for_plot) +
  geom_sf(color = "black", aes(geometry = geometry, fill = neighbors)) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"])) +
  theme_void()

# Print the plot
print(plot)

  
  


