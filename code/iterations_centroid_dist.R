library(ggplot2)
library(dplyr)
library(viridis)
library(rdrobust)

load("~/mexico_mun/data/centroid_dist.Rdata")

# First, sort by mun_id and then by d
df_rdd_sorted <- df_rdd_cd %>%
  arrange(mun_id, cd)

df_rdd_sorted <- subset(df_rdd_sorted, ref_PAN_wins == 0)
df_rdd_sorted$weight <- 1/df_rdd_sorted$cd

# Pre-allocate the result matrix
robust_est <- matrix(NA, nrow = 100, ncol = 7)
ws_est <- matrix(NA, nrow = 100, ncol = 7)

# Vector of n values
n_values <- 1:10

# Loop through n values
for (n in n_values) {
  print(n)
  df_cdp <- df_rdd_sorted %>%
    group_by(mun_id) %>%
    slice_head(n = n) %>%
    summarise(
      PAN_pct = mean(PAN_pct, na.rm = TRUE),
      weighted_avg_npp = sum(ref_next_PAN_pct * weight) / sum(weight),
      weighted_avg_pp = sum(ref_PAN_pct * weight) / sum(weight),
      main_estado = as.factor(first(main_estado)),
      main_year = as.factor(first(main_year)),
      mean_cd = mean(cd),
      weighted_mean_cd = sum(cd * weight) / sum(weight),
      
    )
  
  df_within_state <- df_rdd_sorted %>%
    group_by(mun_id) %>%
    filter(main_estado == ref_estado) %>%
    slice_head(n = n) %>%
    summarise(
      PAN_pct = mean(PAN_pct, na.rm = TRUE),
      weighted_avg_npp = sum(ref_next_PAN_pct * weight) / sum(weight),
      weighted_avg_pp = sum(ref_PAN_pct * weight) / sum(weight),
      main_estado = as.factor(first(main_estado)),
      main_year = as.factor(first(main_year)),
      mean_cd = mean(cd),
      weighted_mean_cd = sum(cd * weight) / sum(weight),
      
    )
  
  df_cdp <- df_cdp %>%
    mutate(change_pp_wt = weighted_avg_npp - weighted_avg_pp)
  
  df_within_state <- df_within_state %>%
    mutate(change_pp_wt = weighted_avg_npp - weighted_avg_pp)
  
  md_rdr <- rdrobust(y = df_cdp$change_pp_wt, x = df_cdp$PAN_pct, covs = cbind(df_cdp$mean_cd,df_cdp$main_estado,df_cdp$main_year), p = 1, bwselect = "mserd")
  ws <- rdrobust(y = df_within_state$change_pp_wt, x = df_within_state$PAN_pct, covs = cbind(df_within_state$weighted_mean_cd,df_within_state$main_estado,df_within_state$main_year), p = 1, bwselect = "mserd")
  
  
  robust_est[n, ] <- c(md_rdr$coef[3], md_rdr$ci[3, 1], md_rdr$ci[3, 2], md_rdr$coef[3] - md_rdr$se[3]*1.65,  md_rdr$coef[3] + md_rdr$se[3]*1.65,n,1) 
  ws_est[n,] <- c(ws$coef[3], ws$ci[3, 1], ws$ci[3, 2], ws$coef[3] - ws$se[3]*1.65,  ws$coef[3] + ws$se[3]*1.65, n,2)
}

# Create a data frame for the plot
plot_data <- as.data.frame(rbind(robust_est,ws_est))
plot_data <- na.omit(plot_data)
colnames(plot_data) <- c("est", "ci_lower","ci_upper","ci90low", "ci90high", "n", "ws")
plot_data$ws <- factor(plot_data$ws, levels = c(1,2), labels = c("All muns","Within State"))

plot_data$n <- as.factor(plot_data$n)

#no meaningful difference between the two distance controls, so just use average
p <- ggplot(plot_data, aes(x = n, y = est, color = ws)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, position = position_dodge(width = -0.5)) +
  geom_point(position = position_dodge(width = -0.5)) +
  labs(x = "Number of References in Weighted Average", y = "RD Estimate", title = "RD Estimates by number of references included") +
  theme_minimal() +
  scale_color_grey()

print(p)

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/Third Year Paper Results Outline/images/centroid_dist_10_refs.png", plot = p, width = 6, height = 4)

##NOW USE BINNED DISTANCES

# Pre-allocate the result matrix
robust_est <- matrix(NA, nrow = 100, ncol = 6)

cd_percentiles <- quantile(df_rdd_sorted$cd, probs = seq(0.01, 0.1, 0.01), na.rm = TRUE)

df_rdd_cd$weight <- 1/df_rdd_cd$cd

# Loop through n values
start_time <- Sys.time()
i <- 1
for (cdp in cd_percentiles) {
  df_cdp <- df_rdd_cd %>%
    group_by(mun_id) %>%
    filter(cd <= cdp) %>%
    summarise(
      PAN_pct = mean(PAN_pct, na.rm = TRUE),
      weighted_avg_npp = sum(ref_next_PAN_pct * weight) / sum(weight),
      weighted_avg_pp = sum(ref_PAN_pct * weight) / sum(weight),
      main_estado = as.factor(first(main_estado)),
      main_year = as.factor(first(main_year)),
    )
  
  df_cdp <- df_cdp %>%
    mutate(change_pp_wt = weighted_avg_npp - weighted_avg_pp)
  
  md_rdr <- rdrobust(y = df_cdp$change_pp_wt, x = df_cdp$PAN_pct, covs = cbind(df_cdp$main_estado,df_cdp$main_year), p = 1)
  
  robust_est[i, ] <- c(md_rdr$coef[3], md_rdr$ci[3, 1], md_rdr$ci[3, 2], md_rdr$coef[3] - md_rdr$se[3]*1.65,  md_rdr$coef[3] + md_rdr$se[3]*1.65, i)
  
  i = i+1
}
stop_time <- Sys.time()
print(stop_time - start_time)


# Create a data frame for the plot, 50 km = 96% of muns in df
robust_est <- as.data.frame(robust_est)
colnames(robust_est) <- c("est", "ci_lower","ci_upper","ci90low", "ci90high", "percentile")

percentile <- ggplot(data = robust_est, aes(x = percentile, y = est)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper)) +
  #geom_errorbar(aes(ymin = ci90low, ymax = ci90high), alpha = 0.9, color = "gray") +
  geom_hline(yintercept = 0, color = "red", alpha = 0.3) + #, linetype = "3") + 
  labs(x = "Percentile Centroid Distance", y = "Estimate", title = "RD Robust Estimates, weighted average by centroid distance") +
  theme_minimal()

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/Third Year Paper Results Outline/images/centroid_dist_percentiles.png", plot = percentile, width = 6, height = 4)


