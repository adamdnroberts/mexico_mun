library(ggplot2)
library(dplyr)
library(rdd)
library(viridis)
library(rdrobust)

## DISTANCES IN KM ##

# Pre-allocate the result matrix
robust_est <- matrix(NA, nrow = 100, ncol = 6)

# Vector of n values
km_values <- seq(5, 50, 5)

# Loop through n values
start_time <- Sys.time()
i <- 1
for (km in km_values) {
  df_km <- df_rdd %>%
    group_by(mun_id) %>%
    filter(dH <= km) %>%
    summarise(
      PAN_pct = mean(PAN_pct, na.rm = TRUE),
      weighted_avg_npp = sum(next_PAN_pct * weight) / sum(weight),
      weighted_avg_pp = sum(ref_PAN_pct * weight) / sum(weight)
    )
  
  df_km <- df_km %>%
    mutate(change_pp_wt = weighted_avg_npp - weighted_avg_pp)
  
  md_rdr <- rdrobust(y = df_km$change_pp_wt, x = df_km$PAN_pct, c = 0.5, p = 1)
  
  robust_est[i, ] <- c(md_rdr$coef[3], md_rdr$ci[3, 1], md_rdr$ci[3, 2], md_rdr$coef[3] - md_rdr$se[3]*1.65,  md_rdr$coef[3] + md_rdr$se[3]*1.65, km)
  
  i = i+1
}
stop_time <- Sys.time()
print(stop_time - start_time)


# Create a data frame for the plot, 50 km = 96% of muns in df
robust_est <- as.data.frame(robust_est)
colnames(robust_est) <- c("est", "ci_lower","ci_upper","ci90low", "ci90high", "km")

ggplot(data = robust_est, aes(x = km, y = est)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.3, color = "darkred") +
  geom_errorbar(aes(ymin = ci90low, ymax = ci90high), alpha = 0.9, color = "gray") +
  #geom_hline(yintercept = 0, color = "darkgray") + 
  labs(x = "KM limit", y = "Estimate", title = "RD Robust Estimates, weighted average by km distance") +
  theme_minimal()

## ADD CONTROLS ##

# Pre-allocate the result matrix
robust_est <- matrix(NA, nrow = 100, ncol = 6)

# Vector of n values
km_values <- seq(5, 50, 5)

# Loop through n values
start_time <- Sys.time()
i <- 1
for (km in km_values) {
  df_km <- df_rdd %>%
    group_by(mun_id) %>%
    filter(dH <= km) %>%
    summarise(
      PAN_pct = mean(PAN_pct, na.rm = TRUE),
      weighted_avg_npp = sum(next_PAN_pct * weight) / sum(weight),
      weighted_avg_pp = sum(ref_PAN_pct * weight) / sum(weight)
      )
  
  df_km <- df_km %>%
    mutate(change_pp_wt = weighted_avg_npp - weighted_avg_pp)
  
  md_rdr <- rdrobust(y = df_km$change_pp_wt, x = df_km$PAN_pct, c = 0.5, p = 1)
  
  robust_est[i, ] <- c(md_rdr$coef[3], md_rdr$ci[3, 1], md_rdr$ci[3, 2], md_rdr$coef[3] - md_rdr$se[3]*1.65,  md_rdr$coef[3] + md_rdr$se[3]*1.65, km)
  
  i = i+1
}
stop_time <- Sys.time()
print(stop_time - start_time)


# Create a data frame for the plot, 50 km = 96% of muns in df
robust_est <- as.data.frame(robust_est)
colnames(robust_est) <- c("est", "ci_lower","ci_upper","ci90low", "ci90high", "km")

ggplot(data = robust_est, aes(x = km, y = est)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.3, color = "darkred") +
  geom_errorbar(aes(ymin = ci90low, ymax = ci90high), alpha = 0.9, color = "gray") +
  #geom_hline(yintercept = 0, color = "darkgray") + 
  labs(x = "KM limit", y = "Estimate", title = "RD Robust Estimates, weighted average by km distance") +
  theme_minimal()

rdplot(y = df_km$change_pp_wt, x = df_km$PAN_pct, c = 0.5)
