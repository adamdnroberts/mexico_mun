library(ggplot2)
library(dplyr)
library(viridis)
library(rdrobust)

load("~/mexico_mun/data/rdd_PAN_subset.Rdata")

#With controls
# Pre-allocate the result matrix
robust_est_w_controls <- matrix(NA, nrow = 5, ncol = 6)

df_rdd_PAN <- subset(rdd_PAN_subset, ref_PAN_wins == 0 & main_estado == ref_estado)

# Vector of n values
n_values <- 1:5

# Loop through n values
for (n in n_values) {
  print(n)
  df_n <- rdd_PAN_subset %>%
    group_by(mun_id) %>%
    slice_head(n = n) %>%
    summarise(
      PAN_margin = mean(PAN_margin, na.rm = TRUE),
      weighted_avg_npp = sum(ref_next_PAN_pct * weight) / sum(weight),
      weighted_avg_pp = sum(ref_PAN_pct * weight) / sum(weight),
      main_year = first(main_year),
      main_estado = first(main_estado),
      avg_dH = mean(dH)
    )
  
  df_n <- df_n %>%
    mutate(change_pp_wt = weighted_avg_npp - weighted_avg_pp)
  
  df_n$main_estado <- as.factor(df_n$main_estado)
  df_n$main_year <- as.factor(df_n$main_year)
  
  m90 <- rdrobust(y = df_n$change_pp_wt, x = df_n$PAN_margin, p = 1, 
                  covs = cbind(df_n$main_year, df_n$main_estado,df_n$avg_dH), 
                  bwselect = "cerrd", level= 90)
  m95 <- rdrobust(y = df_n$change_pp_wt, x = df_n$PAN_margin, p = 1, 
                  covs = cbind(df_n$main_year, df_n$main_estado,df_n$avg_dH), 
                  bwselect = "cerrd", level= 95)
  
  robust_est_w_controls[n, ] <- c(m90$coef[3], m90$ci[3, 1], m90$ci[3, 2], m95$ci[3, 1],  m95$ci[3, 2], n) 
}

# Create a data frame for the plot
plot_data <- as.data.frame(robust_est_w_controls)
plot_data <- na.omit(plot_data)
colnames(plot_data) <- c("est", "ci_90low","ci_90high","ci_95low", "ci_95high", "n")

plot_data$n <- as.factor(plot_data$n)

p <- ggplot(plot_data, aes(x = n, y = est)) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  # 95% CI - thinner error bars
  geom_errorbar(aes(ymin = ci_95low, ymax = ci_95high), 
                width = 0, linewidth = 0.5) +
  # 90% CI - fatter error bars  
  geom_errorbar(aes(ymin = ci_90low, ymax = ci_90high), 
                width = 0, linewidth = 3, alpha = 0.4) +
  geom_point(size = 2.5) +
  labs(x = "Number of Neighbors in Weighted Average", 
       y = "RD Estimate", 
       title = "", 
       subtitle = "",
       caption = "Thick bars: 90% CI, Thin bars: 95% CI") +
  theme_classic()

print(p)

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/TYP_draft/images/PAN_5_nearest_w_controls.png", plot = p, width = 6, height = 4)

#With controls, 1-3 order polynomial
# Pre-allocate the result matrix
poly_order2 <- matrix(NA, nrow = 100, ncol = 7)
poly_order3 <- matrix(NA, nrow = 100, ncol = 7)
poly_order1 <- matrix(NA, nrow = 100, ncol = 7)

# Vector of n values
n_values <- 1:5

# Loop through n values
for (n in n_values) {
  print(n)
  df_n <- rdd_PAN_subset %>%
    group_by(mun_id) %>%
    slice_head(n = n) %>%
    summarise(
      PAN_margin = mean(PAN_margin, na.rm = TRUE),
      weighted_avg_npp = sum(ref_next_PAN_pct * weight) / sum(weight),
      weighted_avg_pp = sum(ref_PAN_pct * weight) / sum(weight),
      main_year = first(main_year),
      main_estado = first(main_estado),
      weighted_avg_dH = sum(dH * weight) / sum(weight)
    )
  
  df_n <- df_n %>%
    mutate(change_pp_wt = weighted_avg_npp - weighted_avg_pp)
  
  df_n$main_estado <- as.factor(df_n$main_estado)
  df_n$main_year <- as.factor(df_n$main_year)
  
  p1_90 <- rdrobust(y = df_n$change_pp_wt, x = df_n$PAN_margin, p = 1,  covs = cbind(df_n$main_year, df_n$main_estado,df_n$weighted_avg_dH), bwselect = "cerrd", level= 90)
  p2_90 <- rdrobust(y = df_n$change_pp_wt, x = df_n$PAN_margin, p = 2,  covs = cbind(df_n$main_year, df_n$main_estado,df_n$weighted_avg_dH), bwselect = "cerrd", level= 90)
  p3_90 <- rdrobust(y = df_n$change_pp_wt, x = df_n$PAN_margin, p = 3,  covs = cbind(df_n$main_year, df_n$main_estado,df_n$weighted_avg_dH), bwselect = "cerrd", level= 90)
  
  p1_95 <- rdrobust(y = df_n$change_pp_wt, x = df_n$PAN_margin, p = 1,  covs = cbind(df_n$main_year, df_n$main_estado,df_n$weighted_avg_dH), bwselect = "cerrd", level= 95)
  p2_95 <- rdrobust(y = df_n$change_pp_wt, x = df_n$PAN_margin, p = 2,  covs = cbind(df_n$main_year, df_n$main_estado,df_n$weighted_avg_dH), bwselect = "cerrd", level= 95)
  p3_95 <- rdrobust(y = df_n$change_pp_wt, x = df_n$PAN_margin, p = 3,  covs = cbind(df_n$main_year, df_n$main_estado,df_n$weighted_avg_dH), bwselect = "cerrd", level= 95)
  
  poly_order1[n, ] <- c(p1_90$coef[3], p1_90$ci[3, 1], p1_90$ci[3, 2], p1_95$ci[3,1], p1_95$ci[3,2], n, 1)
  poly_order2[n, ] <- c(p2_90$coef[3], p2_90$ci[3, 1], p2_90$ci[3, 2], p2_95$ci[3,1], p2_95$ci[3,2], n, 2) 
  poly_order3[n, ] <- c(p3_90$coef[3], p3_90$ci[3, 1], p3_90$ci[3, 2], p3_95$ci[3,1], p3_95$ci[3,2], n, 3) 
}

# Create a data frame for the plot
plot_data <- as.data.frame(rbind(poly_order1,poly_order2,poly_order3))
plot_data <- na.omit(plot_data)
colnames(plot_data) <- c("est", "ci_90low","ci_90high","ci_95low", "ci_95high", "n", "order")
plot_data$Order <- factor(plot_data$order, levels = c(1,2,3))

plot_data$n <- as.factor(plot_data$n)

p <- ggplot(plot_data, aes(x = n, y = est, color = Order)) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  # 95% CI - thinner error bars
  geom_errorbar(aes(ymin = ci_95low, ymax = ci_95high), 
                width = 0, linewidth = 0.5, position = position_dodge(width = 0.5)) +
  # 90% CI - fatter error bars  
  geom_errorbar(aes(ymin = ci_90low, ymax = ci_90high), 
                width = 0, linewidth = 3, alpha = 0.4, position = position_dodge(width = 0.5)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5)) +
  labs(x = "Number of Neighbors in Weighted Average", 
       y = "RD Estimate", 
       title = "", 
       subtitle = "",
       caption = "Thick bars: 90% CI, Thin bars: 95% CI") +
  theme_classic() +
  scale_color_viridis_d()

print(p)

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/TYP Final Tables and Figures Appendix/images/PAN_5_poly.png", plot = p, width = 6, height = 4)


#COMPARE BANDWIDTH SPECIFICATIONS
# Pre-allocate the result matrix
mse_est <- matrix(NA, nrow = 5, ncol = 7)
cer_est <- matrix(NA, nrow = 5, ncol = 7)

# Vector of n values
n_values <- 1:5

# Loop through n values
for (n in n_values) {
  print(n)
  df_n <- rdd_PAN_subset %>%
    group_by(mun_id) %>%
    slice_head(n = n) %>%
    summarise(
      PAN_margin = mean(PAN_margin, na.rm = TRUE),
      weighted_avg_npp = sum(ref_next_PAN_pct * weight) / sum(weight),
      weighted_avg_pp = sum(ref_PAN_pct * weight) / sum(weight),
      main_year = first(main_year),
      main_estado = first(main_estado),
      weighted_avg_dH = sum(dH * weight) / sum(weight)
    )
  
  df_n <- df_n %>%
    mutate(change_pp_wt = weighted_avg_npp - weighted_avg_pp)
  
  df_n$main_estado <- as.factor(df_n$main_estado)
  df_n$main_year <- as.factor(df_n$main_year)
  
  mse_90 <- rdrobust(y = df_n$change_pp_wt, x = df_n$PAN_margin, p = 1,  covs = cbind(df_n$main_year, df_n$main_estado,df_n$weighted_avg_dH), bwselect = "mserd", level= 90)
  cer_90 <- rdrobust(y = df_n$change_pp_wt, x = df_n$PAN_margin, p = 1,  covs = cbind(df_n$main_year, df_n$main_estado,df_n$weighted_avg_dH), bwselect = "cerrd", level= 90)
  
  mse_95 <- rdrobust(y = df_n$change_pp_wt, x = df_n$PAN_margin, p = 1,  covs = cbind(df_n$main_year, df_n$main_estado,df_n$weighted_avg_dH), bwselect = "mserd", level= 95)
  cer_95 <- rdrobust(y = df_n$change_pp_wt, x = df_n$PAN_margin, p = 1,  covs = cbind(df_n$main_year, df_n$main_estado,df_n$weighted_avg_dH), bwselect = "cerrd", level= 95)
  
  mse_est[n, ] <- c(mse_90$coef[3], mse_90$ci[3, 1], mse_90$ci[3, 2], mse_95$ci[3,1], mse_95$ci[3,2], n,2) 
  cer_est[n, ] <- c(cer_90$coef[3], cer_90$ci[3, 1], cer_90$ci[3, 2], cer_95$ci[3,1], cer_95$ci[3,2], n,1) 
}

# Create a data frame for the plot
plot_data <- as.data.frame(rbind(mse_est,cer_est))
plot_data <- na.omit(plot_data)
colnames(plot_data) <- c("est", "ci_90low","ci_90high","ci_95low", "ci_95high", "n", "bw_type")
plot_data$bw_type <- factor(plot_data$bw_type, levels = c(1,2), labels = c("CER","MSE"))

plot_data$n <- as.factor(plot_data$n)

p <- ggplot(plot_data, aes(x = n, y = est, color = bw_type)) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  # 95% CI - thinner error bars
  geom_errorbar(aes(ymin = ci_95low, ymax = ci_95high), 
                width = 0, linewidth = 0.5, position = position_dodge(width = 0.5)) +
  # 90% CI - fatter error bars  
  geom_errorbar(aes(ymin = ci_90low, ymax = ci_90high), 
                width = 0, linewidth = 3, alpha = 0.4, position = position_dodge(width = 0.5)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5)) +
  labs(x = "Number of Neighbors in Weighted Average", 
       y = "RD Estimate", 
       title = "", 
       subtitle = "",
       caption = "Thick bars: 90% CI, Thin bars: 95% CI") +
  theme_classic() +
  scale_color_viridis_d()

print(p)

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/TYP Final Tables and Figures Appendix/images/PAN_5_bw.png", plot = p, width = 6, height = 4)
