library(ggplot2)
library(dplyr)
library(rdd)
library(viridis)
library(rdrobust)

<<<<<<< HEAD
load("~/mexico_mun/data/rdd_distance.Rdata")
=======
load("~/mexico_RD/data/pairwise_distances.Rdata")

load("~/mexico_RD/data/full_dataset_mexelec.Rdata")
big_df$mun_id <- gsub(" ", "", big_df$Municipio)

df <- subset(big_df, year>= 1995 & year <= 1997 
             #& estado!="Tlaxcala" 
             & (p1_name == "PRI" | p2_name == "PRI") & (p1_name == "PAN" | p2_name == "PAN")
)
#df$mun_id <- gsub(" ", "", df$Municipio)
df$PRD_pct <- df$PRD / (df$p1 + df$p2) # PRD percentage compared to top two
df$PRD_treat <- ifelse(df$PRD_pct > 0.5, 1, 0)

DCdensity(df$PAN_pct[df$year <= 1997], cutpoint = 0.5)

df_ref <- subset(big_df, year>= 1995 & year <= 1997 
                 #& estado!="Tlaxcala" 
                 #& (p1_name == "PRI" | p2_name == "PRI") & (p1_name == "PAN" | p2_name == "PAN")
)

#create smaller datasets for merge
ref_PAN <- subset(df_ref, select = c(year, mun_id, next_PAN_pct, PAN_pct, estado))
main_mun_PAN <- subset(df, select = c(year, mun_id, PAN_pct, estado))
DCdensity(ref_PAN$PAN_pct, cutpoint = 0.5)

#merge datasets using adjacent municipalities index
ref2 <- merge(dist_df,ref_PAN, by.x = c("neighbor"), by.y = c("mun_id"))
ref2 <- ref2 %>% rename(ref_PAN_pct = PAN_pct)

df_rdd <- merge(main_mun_PAN,ref2, by.x = c("mun_id"), by.y = c("mun"))
df_rdd <- df_rdd %>% 
  rename(main_year = year.x, ref_year = year.y, main_estado = estado.x, ref_estado = estado.y)

df_rdd$weight <- 1/df_rdd$d

save(df_rdd, file = "~/mexico_RD/data/rdd_near.Rdata")


# First, sort by mun_id and then by d
df_rdd_sorted <- df_rdd %>%
  arrange(mun_id, dH)

# Pre-allocate the result matrix
robust_est <- matrix(NA, nrow = 100, ncol = 6)

# Vector of n values
n_values <- 1:100

# Loop through n values
for (n in n_values) {
  print(n)
  df_n <- df_rdd_sorted %>%
    group_by(mun_id) %>%
    slice_head(n = n) %>%
    summarise(
      PAN_pct = mean(PAN_pct, na.rm = TRUE),
      weighted_avg_npp = sum(next_PAN_pct * weight) / sum(weight),
      weighted_avg_pp = sum(ref_PAN_pct * weight) / sum(weight)
    )
  
  df_n <- df_n %>%
    mutate(change_pp_wt = weighted_avg_npp - weighted_avg_pp)
  
  #md_n <- RDestimate(change_pp_wt ~ PAN_pct, cutpoint = 0.5, data = df_n)
  md_rdr <- rdrobust(y = df_n$change_pp_wt, x = df_n$PAN_pct, c = 0.5, p = 1, bwselect = "mserd")
  
  
  #robust_est[n, ] <- c(md_n$est[1], md_n$ci[1, 1], md_n$ci[1, 2], n)
  robust_est[n, ] <- c(md_rdr$coef[3], md_rdr$ci[3, 1], md_rdr$ci[3, 2], md_rdr$coef[3] - md_rdr$se[3]*1.65,  md_rdr$coef[3] + md_rdr$se[3]*1.65, n)
}

# Create a data frame for the plot
plot_data <- as.data.frame(robust_est)
colnames(plot_data) <- c("est", "ci_lower","ci_upper","ci90low", "ci90high", "n")

p <- ggplot(plot_data, aes(x = n, y = est)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(x = "Number of References in Average", y = "Estimate", title = "RD Estimates by number of references included") +
  theme_minimal() 

print(p)


#COLLECT ALL THREE BANDWIDTHS WITH rdd

# Pre-allocate the result matrix
bw_estimates <- matrix(NA, nrow = 100, ncol = 4)
bw_half_estimates <- matrix(NA, nrow = 100, ncol = 4)
bw_dbl_estimates <- matrix(NA, nrow = 100, ncol = 4)


# Vector of n values
n_values <- 1:100

# Loop through n values
for (n in n_values) {
  df_n <- df_rdd_sorted %>%
    group_by(mun_id) %>%
    slice_head(n = n) %>%
    summarise(
      PAN_pct = mean(PAN_pct, na.rm = TRUE),
      weighted_avg_npp = sum(next_PAN_pct * weight) / sum(weight),
      weighted_avg_pp = sum(ref_PAN_pct * weight) / sum(weight)
    )
  
  df_n <- df_n %>%
    mutate(change_pp_wt = weighted_avg_npp - weighted_avg_pp)
  
  #md_n <- RDestimate(change_pp_wt ~ PAN_pct, cutpoint = 0.5, data = df_n)
  md_rdr <- rdrobust(y = df_n$change_pp_wt, x = df_n$PAN_pct, c = 0.5, p = 1, bwselect = "mserd")
  
  #bw_estimates[n, ] <- c(md_n$est[1], md_n$ci[1, 1], md_n$ci[1, 2], n)
  bw_estimates[n, ] <- c(md_rdr$coef[1], md_rdr$ci[1, 1], md_rdr$ci[1, 2], n)
}


# Create a data frame for the plot
bw_df <- as.data.frame(bw_estimates)
colnames(bw_df) <- c("est", "ci_lower","ci_upper","n")

bw_df_half <- as.data.frame(bw_half_estimates)
colnames(bw_df_half) <- c("est", "ci_lower","ci_upper","n")

bw_df_dbl <- as.data.frame(bw_dbl_estimates)
colnames(bw_df_dbl) <- c("est", "ci_lower","ci_upper","n")

# Create the plot
p <- ggplot(bw_df, aes(x = n, y = est)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(x = "Number of References in Average", y = "Estimate", title = "RD Estimates by number of references included") +
  theme_minimal() 

# Display the plot
print(p)


#COLLECT ALL THREE ESTIMATES

# Pre-allocate the result matrix
bw_estimates <- matrix(NA, nrow = 100, ncol = 4)
bw_half_estimates <- matrix(NA, nrow = 100, ncol = 4)
bw_dbl_estimates <- matrix(NA, nrow = 100, ncol = 4)


# Vector of n values
n_values <- 1:100

# Loop through n values
for (n in n_values) {
  df_n <- df_rdd_sorted %>%
    group_by(mun_id) %>%
    slice_head(n = n) %>%
    summarise(
      PAN_pct = mean(PAN_pct, na.rm = TRUE),
      weighted_avg_npp = sum(next_PAN_pct * weight) / sum(weight),
      weighted_avg_pp = sum(ref_PAN_pct * weight) / sum(weight)
    )
  
  df_n <- df_n %>%
    mutate(change_pp_wt = weighted_avg_npp - weighted_avg_pp)
  
  md_n <- RDestimate(change_pp_wt ~ PAN_pct, cutpoint = 0.5, data = df_n)
  
  bw_estimates[n, ] <- c(md_n$est[1], md_n$ci[1, 1], md_n$ci[1, 2], n)
  bw_half_estimates[n, ] <- c(md_n$est[2], md_n$ci[2, 1], md_n$ci[2, 2], n)
  bw_dbl_estimates[n, ] <- c(md_n$est[3], md_n$ci[3, 1], md_n$ci[3, 2], n)
}


# Create a data frame for the plot
bw_df <- as.data.frame(bw_estimates)
colnames(bw_df) <- c("est", "ci_lower","ci_upper","n")

bw_df_half <- as.data.frame(bw_half_estimates)
colnames(bw_df_half) <- c("est", "ci_lower","ci_upper","n")

bw_df_dbl <- as.data.frame(bw_dbl_estimates)
colnames(bw_df_dbl) <- c("est", "ci_lower","ci_upper","n")

# Create the plot
p <- ggplot(bw_df, aes(x = n, y = est)) +
  geom_point() +
  #geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(x = "Number of References in Average", y = "Estimate", title = "RD Estimates by number of references included") +
  theme_minimal() 

# Display the plot
print(p)

# Combine the data frames into one with an additional column to distinguish them
bw_df$bw <- 1
bw_df_half$bw <- 2
bw_df_dbl$bw <- 3
combined_df <- bind_rows(bw_df, bw_df_half, bw_df_dbl)

# Convert bw to a factor variable with specified labels
combined_df$bw <- factor(combined_df$bw, levels = c(1, 2, 3), labels = c("I-K", "Half", "Double"))

# Create the plot
compare_bw <- ggplot(data = combined_df, aes(x = n, y = est, color = bw)) +
  geom_point() +
  #geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.3, width = 0.2) +
  scale_color_viridis_d() +
  labs(x = "Number of References in Average", y = "Estimate", title = "RD Estimates by number of references included") +
  theme_minimal()

print(compare_bw)

## RDROBUST, ALL THRE ESTIMATES ##
# First, sort by mun_id and then by d
df_rdd_sorted <- df_rdd %>%
  arrange(mun_id, d)

# Pre-allocate the result matrix
bw1 <- matrix(NA, nrow = 100, ncol = 4)
bw2 <- matrix(NA, nrow = 100, ncol = 4)
bw3 <- matrix(NA, nrow = 100, ncol = 4)

# Vector of n values
n_values <- 1:100

# Loop through n values
for (n in n_values) {
  df_n <- df_rdd_sorted %>%
    group_by(mun_id) %>%
    slice_head(n = n) %>%
    summarise(
      PAN_pct = mean(PAN_pct, na.rm = TRUE),
      weighted_avg_npp = sum(next_PAN_pct * weight) / sum(weight),
      weighted_avg_pp = sum(ref_PAN_pct * weight) / sum(weight)
    )
  
  df_n <- df_n %>%
    mutate(change_pp_wt = weighted_avg_npp - weighted_avg_pp)
  
  md_rdr <- rdrobust(y = df_n$change_pp_wt, x = df_n$PAN_pct, c = 0.5, bwselect = "cerrd")
  
  bw1[n, ] <- c(md_rdr$coef[1], md_rdr$ci[1, 1], md_rdr$ci[1, 2], n)
  bw2[n, ] <- c(md_rdr$coef[2], md_rdr$ci[2, 1], md_rdr$ci[2, 2], n)
  bw3[n, ] <- c(md_rdr$coef[3], md_rdr$ci[3, 1], md_rdr$ci[3, 2], n)
}


# Create a data frame for the plot
bw1 <- as.data.frame(bw1)
colnames(bw1) <- c("est", "ci_lower","ci_upper","n")

bw2 <- as.data.frame(bw2)
colnames(bw2) <- c("est", "ci_lower","ci_upper","n")

bw3 <- as.data.frame(bw3)
colnames(bw3) <- c("est", "ci_lower","ci_upper","n")

# Combine the data frames into one with an additional column to distinguish them
bw1$ci_type <- 1
bw2$ci_type <- 2
bw3$ci_type <- 3
combined_df <- bind_rows(bw1, bw2, bw3)

# Convert bw to a factor variable with specified labels
combined_df$ci_type <- factor(combined_df$ci_type, levels = c(1, 2, 3), labels = c("Conventional", "Bias Corrected", "Robust"))

# Create the plot
ggplot(data = combined_df, aes(x = n, y = est, color = ci_type)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.3, width = 0.2) +
  scale_color_viridis_d(direction = -1) +
  labs(x = "Number of References in Average", y = "Estimate", title = "RD Estimates by number of references included") 
