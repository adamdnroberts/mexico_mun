library(ggplot2)
library(dplyr)
library(viridis)
library(rdrobust)

load("~/mexico_mun/data/rdd_PRD_subset.Rdata")

#With controls
# Pre-allocate the result matrix
descriptive_statistics <- matrix(NA, nrow = 5, ncol = 6)

df_rdd_PRD <- subset(rdd_PRD_subset, ref_PRD_wins == 0 & main_estado == ref_estado)

# Vector of n values
n_values <- 1:5

# Loop through n values
for (n in n_values) {
  print(n)
  df_n <- df_rdd_PRD %>%
    group_by(mun_id) %>%
    slice(n)
  
  ds <- summary(df_n$dH)

  descriptive_statistics[n, ] <- c(n, round(ds[1],2), round(ds[3],2), round(ds[6],2), round(ds[4],2), round(sd(df_n$dH),2)) 
}

print(descriptive_statistics)
