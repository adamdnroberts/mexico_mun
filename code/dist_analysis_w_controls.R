library(ggplot2)
library(dplyr)
library(rdd)
library(viridis)
library(rdrobust)
library(RDHonest)

load("~/mexico_mun/data/rdd_near.Rdata")

df_rdd$ref_PAN_win <- ifelse(df_rdd$ref_PAN_pct > 0.5, 1, 0)

# First, sort by mun_id and then by d
df_rdd_sorted <- df_rdd %>%
  arrange(mun_id, d)

# Pre-allocate the result matrix
robust_est <- matrix(NA, nrow = 100, ncol = 6)

# Vector of n values
n_values <- 1:25

# Loop through n values
for (n in n_values) {
  print(n)
  df_n <- df_rdd_sorted %>%
    group_by(mun_id) %>%
    slice_head(n = n) %>%
    summarise(
      PAN_pct = mean(PAN_pct, na.rm = TRUE),
      weighted_avg_npp = sum(ref_next_PAN_pct * weight) / sum(weight),
      weighted_avg_pp = sum(ref_PAN_pct * weight) / sum(weight),
      wa_ref_PAN_win = sum(ref_PAN_win * weight) /sum(weight),
      wa_ref_PAN_pct = sum(ref_PAN_pct * weight) /sum(weight),
      wa_d = sum(d * weight, na.rm = T) /sum(weight),
      main_estado = first(main_estado)
    )
  
  df_n <- df_n %>%
    mutate(change_pp_wt = weighted_avg_npp - weighted_avg_pp)
  
  #md_n <- RDestimate(change_pp_wt ~ PAN_pct, cutpoint = 0.5, data = df_n)
  md_rdr <- rdrobust(y = df_n$change_pp_wt, x = df_n$PAN_pct, covs = cbind(df_n$wa_ref_PAN_win,df_n$wa_d,as.factor(df_n$main_estado)), c = 0.5, p = 1, bwselect = "mserd")
  
  
  #robust_est[n, ] <- c(md_n$est[1], md_n$ci[1, 1], md_n$ci[1, 2], n)
  robust_est[n, ] <- c(md_rdr$coef[3], md_rdr$ci[3, 1], md_rdr$ci[3, 2], md_rdr$coef[3] - md_rdr$se[3]*1.65,  md_rdr$coef[3] + md_rdr$se[3]*1.65, n)
}

# Create a data frame for the plot
plot_data <- as.data.frame(robust_est)
colnames(plot_data) <- c("est", "ci_lower","ci_upper","ci90low", "ci90high", "n")

p <- ggplot(plot_data, aes(x = n, y = est)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_errorbar(aes(ymin = ci90low, ymax = ci90high), width = 0.2, color = "red") +
  geom_point() +
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
      weighted_avg_npp = sum(ref_next_PAN_pct * weight) / sum(weight),
      weighted_avg_pp = sum(ref_PAN_pct * weight) / sum(weight),
      wa_ref_PAN_win = sum(ref_PAN_win * weight) /sum(weight),
      wa_ref_PAN_pct = sum(ref_PAN_pct * weight) /sum(weight),
      wa_d = sum(d * weight, na.rm = T) /sum(weight),
      main_estado = first(main_estado)
    )
  
  df_n <- df_n %>%
    mutate(change_pp_wt = weighted_avg_npp - weighted_avg_pp)
  
  #md_n <- RDestimate(change_pp_wt ~ PAN_pct, cutpoint = 0.5, data = df_n)
  md_honest <- RDHonest(change_pp_wt ~ PAN_pct | wa_ref_PAN_win + wa_d + main_estado, cutoff = 0.5, data = df_n)
  
  
  #robust_est[n, ] <- c(md_n$est[1], md_n$ci[1, 1], md_n$ci[1, 2], n)
  robust_est[n, ] <- c(md_honest$coefficients[1,2], md_honest$coefficients[1,5], md_honest$coefficients[1,6], md_honest$coefficients[1,2] - md_honest$coefficients[1,3]*1.65,  md_honest$coefficients[1,2] + md_honest$coefficients[1,3]*1.65, n)
}

# Create a data frame for the plot
plot_data <- as.data.frame(robust_est)
colnames(plot_data) <- c("est", "ci_lower","ci_upper","ci90low", "ci90high", "n")

p <- ggplot(plot_data, aes(x = n, y = est)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  #geom_errorbar(aes(ymin = ci90low, ymax = ci90high), width = 0.2, color = "red") +
  geom_point() +
  labs(x = "Number of References in Weighted Average", y = "RD Estimate", title = "RD Estimates by number of references included") +
  theme_minimal() 

print(p)
