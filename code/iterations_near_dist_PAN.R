library(ggplot2)
library(dplyr)
library(viridis)
library(rdrobust)

load("~/mexico_mun/data/rdd_distance_PAN.Rdata")

#With controls
# Pre-allocate the result matrix
robust_est_w_controls <- matrix(NA, nrow = 100, ncol = 7)

df_rdd_PAN <- subset(df_rdd_PAN, ref_PAN_wins == 0 & main_estado == ref_estado)

# Vector of n values
n_values <- 1:5

# Loop through n values
for (n in n_values) {
  print(n)
  df_n <- df_rdd_PAN %>%
    group_by(mun_id) %>%
    slice_head(n = n) %>%
    summarise(
      PAN_pct = mean(PAN_pct, na.rm = TRUE),
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
  
  md_rdr <- rdrobust(y = df_n$change_pp_wt, x = df_n$PAN_pct, p = 1, 
                     covs = cbind(df_n$main_year, df_n$main_estado,df_n$weighted_avg_dH), 
                     bwselect = "cerrd", level= 90)
  
  robust_est_w_controls[n, ] <- c(md_rdr$coef[3], md_rdr$ci[3, 1], md_rdr$ci[3, 2], md_rdr$coef[3] - md_rdr$se[3]*1.65,  md_rdr$coef[3] + md_rdr$se[3]*1.65,n,1) 
}

# Create a data frame for the plot
plot_data <- as.data.frame(robust_est_w_controls)
plot_data <- na.omit(plot_data)
colnames(plot_data) <- c("est", "ci_lower","ci_upper","ci90low", "ci90high", "n", "bw_type")
plot_data$bw_type <- factor(plot_data$bw_type, levels = c(1,2), labels = c("MSE","CER"))

plot_data$n <- as.factor(plot_data$n)

p <- ggplot(plot_data, aes(x = n, y = est)) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, position = position_dodge(width = -0.5)) +
  geom_point(position = position_dodge(width = -0.5)) +
  labs(x = "Number of References in Weighted Average", 
       y = "RD Estimate (90% CI)", 
       title = "", 
       subtitle = "Controls: state & year of municipal elections, distance") +
  theme_minimal()

print(p)

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/TYP_draft/images/PAN_5_nearest_w_controls.png", plot = p, width = 6, height = 4)

#With controls, 1-3 order polynomial
# Pre-allocate the result matrix
poly_order1 <- matrix(NA, nrow = 100, ncol = 5)
poly_order2 <- matrix(NA, nrow = 100, ncol = 5)
poly_order3 <- matrix(NA, nrow = 100, ncol = 5)

# Vector of n values
n_values <- 1:5

# Loop through n values
for (n in n_values) {
  print(n)
  df_n <- df_rdd_PAN %>%
    group_by(mun_id) %>%
    slice_head(n = n) %>%
    summarise(
      PAN_pct = mean(PAN_pct, na.rm = TRUE),
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
  
  p1 <- rdrobust(y = df_n$change_pp_wt, x = df_n$PAN_pct, p = 1,  covs = cbind(df_n$main_year, df_n$main_estado,df_n$weighted_avg_dH), bwselect = "cerrd", level= 90)
  p2 <- rdrobust(y = df_n$change_pp_wt, x = df_n$PAN_pct, p = 2,  covs = cbind(df_n$main_year, df_n$main_estado,df_n$weighted_avg_dH), bwselect = "cerrd", level= 90)
  p3 <- rdrobust(y = df_n$change_pp_wt, x = df_n$PAN_pct, p = 3,  covs = cbind(df_n$main_year, df_n$main_estado,df_n$weighted_avg_dH), bwselect = "cerrd", level= 90)
  
  poly_order1[n, ] <- c(p1$coef[3], p1$ci[3, 1], p1$ci[3, 2],n,1)
  poly_order2[n, ] <- c(p2$coef[3], p2$ci[3, 1], p2$ci[3, 2],n,2) 
  poly_order3[n, ] <- c(p3$coef[3], p3$ci[3, 1], p3$ci[3, 2],n,3) 
}

# Create a data frame for the plot
plot_data <- as.data.frame(rbind(poly_order1,poly_order2,poly_order3))
plot_data <- na.omit(plot_data)
colnames(plot_data) <- c("est", "ci_lower","ci_upper", "n", "order")
plot_data$order <- as.factor(plot_data$order)

plot_data$n <- as.factor(plot_data$n)

p <- ggplot(plot_data, aes(x = n, y = est, color = order)) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width = 0.5)) +
  labs(x = "Number of References in Weighted Average", y = "RD Estimate (90% CI)", 
       title = "", 
       subtitle = "controls: state & year of municipal elections") +
  theme_minimal()

print(p)

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/TYP Final Tables and Figures Appendix/images/PAN_5_poly.png", plot = p, width = 6, height = 4)


#COMPARE BANDWIDTH SPECIFICATIONS
# Pre-allocate the result matrix
mse_est <- matrix(NA, nrow = 100, ncol = 5)
cer_est <- matrix(NA, nrow = 100, ncol = 5)

# Vector of n values
n_values <- 1:5

# Loop through n values
for (n in n_values) {
  print(n)
  df_n <- df_rdd_PAN %>%
    group_by(mun_id) %>%
    slice_head(n = n) %>%
    summarise(
      PAN_pct = mean(PAN_pct, na.rm = TRUE),
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
  
  mse <- rdrobust(y = df_n$change_pp_wt, x = df_n$PAN_pct, p = 1,  covs = cbind(df_n$main_year, df_n$main_estado,df_n$weighted_avg_dH), bwselect = "mserd", level= 90)
  cer <- rdrobust(y = df_n$change_pp_wt, x = df_n$PAN_pct, p = 1,  covs = cbind(df_n$main_year, df_n$main_estado,df_n$weighted_avg_dH), bwselect = "cerrd", level= 90)
  
  mse_est[n, ] <- c(mse$coef[3], mse$ci[3, 1], mse$ci[3, 2],n,2) 
  cer_est[n, ] <- c(cer$coef[3], cer$ci[3, 1], cer$ci[3, 2],n,1) 
}

# Create a data frame for the plot
plot_data <- as.data.frame(rbind(mse_est,cer_est))
plot_data <- na.omit(plot_data)
colnames(plot_data) <- c("est", "ci_lower", "ci_upper", "n", "bw_type")
plot_data$bw_type <- factor(plot_data$bw_type, levels = c(1,2), labels = c("CER","MSE"))

plot_data$n <- as.factor(plot_data$n)

p <- ggplot(plot_data, aes(x = n, y = est, color = bw_type)) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width = 0.5)) +
  labs(x = "Number of References in Weighted Average", 
       y = "RD Estimate (90% CI)", 
       title = "RD Estimates by number of references included") +
  theme_minimal() +
  scale_color_grey()

print(p)

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/TYP Final Tables and Figures Appendix/images/PAN_5_bw.png", plot = p, width = 6, height = 4)
