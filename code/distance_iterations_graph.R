library(ggplot2)
library(dplyr)
library(rdd)
library(viridis)
library(rdrobust)
library(RDHonest)

load("~/mexico_mun/data/rdd_near.Rdata")

# First, sort by mun_id and then by d
df_rdd_sorted <- df_rdd %>%
  arrange(mun_id, d)

# Pre-allocate the result matrix
robust_est <- matrix(NA, nrow = 100, ncol = 6)

# Vector of n values
n_values <- 1:10

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
  labs(x = "Number of References in Weighted Average", y = "RD Estimate", title = "RD Estimates by number of references included") +
  theme_minimal() 

print(p)

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/3YP_Presentation_2_17_25/images/num_refs_10.png", plot = p, width = 6, height = 4)

#RD Plot for only 1 reference
df_1 <- df_rdd_sorted %>%
  group_by(mun_id) %>%
  slice_head(n = 1) %>%
  summarise(
    PAN_pct = mean(PAN_pct, na.rm = TRUE),
    weighted_avg_npp = sum(next_PAN_pct * weight) / sum(weight),
    weighted_avg_pp = sum(ref_PAN_pct * weight) / sum(weight)
  )

df_1 <- df_1 %>%
  mutate(change_pp = next_PAN_pct - ref_PAN_pct)

#md_n <- RDestimate(change_pp_wt ~ PAN_pct, cutpoint = 0.5, data = df_n)
one_ref <- rdrobust(y = df_1$change_pp, x = df_1$PAN_pct, c = 0.5, p = 1, bwselect = "mserd")
summary(one_ref)

png(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/3YP_Presentation_2_17_25/images/rdplot_nearest.png", width = 6, height = 4, units = "in", res = 300)
rdplot(y = df_1$change_pp, x = df_1$PAN_pct, c = 0.5, title = "RD for nearest municipality", x.label = "PAN Vote Share, t", y.label = "Nearest Municipalitiy PAN vote share, t+1")
dev.off()

##EXAMINE SUPPLY SIDE

#are they just dropping out in nearby places?
summary(df_1$PAN_runs_next)
pan_runs <- rdrobust(y = df_1$PAN_runs_next, x = df_1$PAN_pct, c = 0.5, p = 1, bwselect = "mserd")
summary(pan_runs)

rdplot(y = df_1$PAN_runs_next, x = df_1$PAN_pct, c = 0.5)

#drop places where PAN doesn't run (PAN Always Runs)
PAR <- subset(df_1, PAN_runs_next == 1)

PAR_m <- rdrobust(y = PAR$change_pp, x = PAR$PAN_pct, c = 0.5, p = 1, bwselect = "mserd")
summary(PAR_m)

rdplot(y = PAR$change_pp, x = PAR$PAN_pct, c = 0.5, title = "RD for nearest municipality", x.label = "PAN Vote Share, t", y.label = "Nearest Municipalitiy PAN vote share, t+1")



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
