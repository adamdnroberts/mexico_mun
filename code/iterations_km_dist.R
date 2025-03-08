library(ggplot2)
library(dplyr)
library(viridis)
library(rdrobust)

load("~/mexico_mun/data/rdd_distance.Rdata")

## DISTANCES IN KM ##

# Pre-allocate the result matrix
robust_est <- matrix(NA, nrow = 100, ncol = 6)

# Vector of n values
km_values <- seq(25, 150, 25)

# Loop through n values
start_time <- Sys.time()
i <- 1
for (km in km_values) {
  df_km <- df_rdd %>%
    group_by(mun_id) %>%
    filter(dH <= km) %>%
    summarise(
      PAN_pct = mean(PAN_pct, na.rm = TRUE),
      weighted_avg_npp = sum(ref_next_PAN_pct * weight) / sum(weight),
      weighted_avg_pp = sum(ref_PAN_pct * weight) / sum(weight)
    )
  
  df_km <- df_km %>%
    mutate(change_pp_wt = weighted_avg_npp - weighted_avg_pp)
  
  md_rdr <- rdrobust(y = df_km$change_pp_wt, x = df_km$PAN_pct, p = 1)
  
  robust_est[i, ] <- c(md_rdr$coef[3], md_rdr$ci[3, 1], md_rdr$ci[3, 2], md_rdr$coef[3] - md_rdr$se[3]*1.65,  md_rdr$coef[3] + md_rdr$se[3]*1.65, km)
  
  i = i+1
}
stop_time <- Sys.time()
print(stop_time - start_time)


# Create a data frame for the plot, 50 km = 96% of muns in df
robust_est <- as.data.frame(robust_est)
colnames(robust_est) <- c("est", "ci_lower","ci_upper","ci90low", "ci90high", "km")

km_mun <- ggplot(data = robust_est, aes(x = km, y = est)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper)) +
  geom_errorbar(aes(ymin = ci90low, ymax = ci90high), alpha = 0.9, color = "gray") +
  geom_hline(yintercept = 0, color = "red", alpha = 0.3) +
  labs(x = "KM limit", y = "Estimate", title = "RD Robust Estimates, municipal seat distance bins") +
  theme_minimal()

print(km_mun)

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/Third Year Paper Results Outline/images/ms_dist_km.png", plot = km_mun, width = 6, height = 4)

##NOW USE BINNED DISTANCES

# Pre-allocate the result matrix
robust_est <- matrix(NA, nrow = 100, ncol = 6)

km_percentiles <- quantile(df_rdd$dH, probs = seq(0.01, 0.1, 0.01), na.rm = TRUE)

# Loop through n values
start_time <- Sys.time()
i <- 1
for (kmp in km_percentiles) {
  df_km <- df_rdd %>%
    group_by(mun_id) %>%
    filter(dH <= kmp) %>%
    summarise(
      PAN_pct = mean(PAN_pct, na.rm = TRUE),
      weighted_avg_npp = sum(ref_next_PAN_pct * weight) / sum(weight),
      weighted_avg_pp = sum(ref_PAN_pct * weight) / sum(weight),
      main_estado = as.factor(first(main_estado)),
      main_year = as.factor(first(main_year)),
    )
  
  df_km <- df_km %>%
    mutate(change_pp_wt = weighted_avg_npp - weighted_avg_pp)
  
  md_rdr <- rdrobust(y = df_km$change_pp_wt, x = df_km$PAN_pct, p = 1)
  
  robust_est[i, ] <- c(md_rdr$coef[3], md_rdr$ci[3, 1], md_rdr$ci[3, 2], md_rdr$coef[3] - md_rdr$se[3]*1.65,  md_rdr$coef[3] + md_rdr$se[3]*1.65, i)
  
  i = i+1
}
stop_time <- Sys.time()
print(stop_time - start_time)


# Create a data frame for the plot, 50 km = 96% of muns in df
robust_est <- as.data.frame(robust_est)
colnames(robust_est) <- c("est", "ci_lower","ci_upper","ci90low", "ci90high", "percentile")
robust_est$percentile <- as.factor(robust_est$percentile)

percentile <- ggplot(data = robust_est, aes(x = percentile, y = est)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper)) +
  #geom_errorbar(aes(ymin = ci90low, ymax = ci90high), alpha = 0.9, color = "gray") +
  geom_hline(yintercept = 0, color = "red", alpha = 0.3) + #, linetype = "3") + 
  labs(x = "Percentile KM Distance", y = "Estimate", 
       title = "RD Robust Estimates") +
  theme_minimal()

print(percentile)

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/Third Year Paper Results Outline/images/mskm_dist_percentiles.png", plot = percentile, width = 6, height = 4)

