#SHP FILES!!!! https://www.inegi.org.mx/temas/mg/default.html#Descargas
#https://r-graph-gallery.com/168-load-a-shape-file-into-r.html

library(sf)
library(ggplot2)
library(dplyr)
library(rdd)
library(rddtools) #for balance of covariates testing

#get neighboring/adjacent municipalities list
mex_sf <- read_sf("~/mexico_mun/raw/mun1995shp/Municipios_1995.shp")  # Read the shapefile
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

big_df$mun_id <- gsub(" ", "", big_df$Municipio)

df <- subset(big_df, year>= 1995 & year <= 1997 
             #& estado!="Tlaxcala" 
             & (p1_name == "PRI" | p2_name == "PRI") & (p1_name == "PAN" | p2_name == "PAN")
)
#df$mun_id <- gsub(" ", "", df$Municipio)
df$PRD_pct <- df$PRD / (df$p1 + df$p2) # PRD percentage compared to top two
df$PRD_treat <- ifelse(df$PRD_pct > 0.5, 1, 0)

df_ref <- subset(big_df, year>= 1995 & year <= 1997 
                 #& estado!="Tlaxcala" 
                 #& (p1_name == "PRI" | p2_name == "PRI") & (p1_name == "PAN" | p2_name == "PAN")
)

#create smaller datasets for merge
ref_PAN <- subset(df_ref, select = c(year, mun_id, next_PAN_pct, PAN_pct, estado))
main_mun_PAN <- subset(df, select = c(year, mun_id, PAN_pct, estado))

#merge datasets using adjacent municipalities index
ref2 <- merge(merge_neighbors2,ref_PAN, by.x = "mun_id.y", by.y = "mun_id")
ref2 <- ref2 %>% rename(ref_PAN_pct = PAN_pct, ref_next_PAN_pct = next_PAN_pct, ref_estado = estado, ref_year = year, ref_mun_id = mun_id.y, mun_id = mun_id.x)
ref2$ref_PAN_wins <- ifelse(ref2$ref_PAN_pct > 0, 1, 0)

df_rdd <- merge(main_mun_PAN,ref2, by = "mun_id")
df_rdd <- df_rdd %>% 
  rename(main_year = year, main_estado = estado)

df_rdd$change <- df_rdd$ref_next_PAN_pct - df_rdd$ref_PAN_pct
df_rdd$mun_id_factor <- as.factor(df_rdd$mun_id)
df_rdd$main_year_factor <- as.factor(df_rdd$main_year)
df_rdd$main_estado_factor <- as.factor(df_rdd$main_estado)
df_rdd$ref_year_factor <- as.factor(df_rdd$ref_year)
df_rdd$ref_estado_factor <- as.factor(df_rdd$ref_estado)

md_rdr <- rdrobust(y = df_rdd$change, x = df_rdd$PAN_pct, covs = cbind(df_rdd$mun_id_factor, df_rdd$main_year_factor, df_rdd$main_estado_factor, df_rdd$ref_year_factor, df_rdd$ref_estado_factor), p = 1)
summary(md_rdr)

df_rdd$weight <- 1

df_rdd_avg <- df_rdd %>%
  group_by(mun_id) %>%
  summarise(
  PAN_pct = mean(PAN_pct, na.rm = TRUE),
  weighted_avg_npp = sum(ref_next_PAN_pct * weight) / sum(weight),
  weighted_avg_pp = sum(ref_PAN_pct * weight) / sum(weight),
  main_estado = as.factor(first(main_estado)),
  main_year = as.factor(first(main_year)),
  )
  
df_rdd_avg <- df_rdd_avg %>%
    mutate(change_pp_wt = weighted_avg_npp - weighted_avg_pp)
  
md_rdr <- rdrobust(y = df_rdd_avg$change_pp_wt, x = df_rdd_avg$PAN_pct, p = 1)
summary(md_rdr)

# Define the function
format_model_table <- function(model) {
  
  # Extract the model name from the model object
  model_name <- deparse(substitute(model))
  
  # Get the second character from the model name
  second_char <- substr(model_name, 2, 2)
  
  # Extract the necessary values from the model object
  coef <- round(model$coef[3],3)
  se <- round(model$se[3], 3)
  N_left <- model$N[1]
  N_right <- model$N[2]
  eff_N_left <- model$N_h[1]
  eff_N_right <- model$N_h[2]
  
  # Create a data frame with the specified values
  model_table <- data.frame(
    #No.Refs = substr(model_name, 2, 2),
    #No.PAN.refs = substr(model_name, 4, 4),
    Coef. = coef,
    SE = se,
    `N.left` = N_left,
    `N.Right` = N_right,
    `Eff.N.Left` = eff_N_left,
    `Eff.N.Right` = eff_N_right
  )
  
  # Return the formatted table
  return(model_table)
}

# Call the function with the model object
model_table <- rbind(format_model_table(md_rdr))
print(model_table)

png(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/Third Year Paper Results Outline/images/rdplot_bordering.png", width = 6, height = 4, units = "in", res = 300)
rdplot(y = df_rdd_avg$change_pp_wt, x = df_rdd_avg$PAN_pct)
dev.off()
