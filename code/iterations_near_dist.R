library(ggplot2)
library(dplyr)
library(rdd)
library(viridis)
library(rdrobust)
library(RDHonest)

load("~/mexico_mun/data/rdd_distance.Rdata")

df_rdd$ref_PAN_wins_t <- ifelse(df_rdd$ref_PAN_pct > 0, 1, 0)

# First, sort by mun_id and then by d
df_rdd_sorted <- df_rdd %>%
  arrange(mun_id, dH)

# Pre-allocate the result matrix
robust_est <- matrix(NA, nrow = 100, ncol = 7)
cer_est <- matrix(NA, nrow = 100, ncol = 7)

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
  cer <- rdrobust(y = df_n$change_pp_wt, x = df_n$PAN_pct, c = 0.5, p = 1, bwselect = "cerrd")
  
  
  #robust_est[n, ] <- c(md_n$est[1], md_n$ci[1, 1], md_n$ci[1, 2], n)
  robust_est[n, ] <- c(md_rdr$coef[3], md_rdr$ci[3, 1], md_rdr$ci[3, 2], md_rdr$coef[3] - md_rdr$se[3]*1.65,  md_rdr$coef[3] + md_rdr$se[3]*1.65,n,"mse") 
  cer_est[n,] <- c(cer$coef[3], cer$ci[3, 1], cer$ci[3, 2], cer$coef[3] - cer$se[3]*1.65,  cer$coef[3] + cer$se[3]*1.65, n, "cer")
}

# Create a data frame for the plot
plot_data <- as.data.frame(rbind(robust_est,cer_est))
plot_data <- na.omit(plot_data)
colnames(plot_data) <- c("est", "ci_lower","ci_upper","ci90low", "ci90high", "n", "bw_type")
plot_data$est <- as.numeric(plot_data$est)
plot_data <- plot_data %>%
  mutate(across(.cols = -bw_type, .fns = ~ if(is.character(.)) as.numeric(.) else . ))

p <- ggplot(plot_data, aes(x = n, y = est, color = bw_type)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, position = position_dodge(width = -0.5)) +
  geom_point(position = position_dodge(width = -0.5)) +
  labs(x = "Number of References in Weighted Average", y = "RD Estimate", title = "RD Estimates by number of references included") +
  theme_minimal() +
  scale_color_viridis_d()

print(p)

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/3YP_Presentation_2_17_25/images/num_refs_10.png", plot = p, width = 6, height = 4)

#RD Plot for only 1 reference
df_1 <- df_rdd_sorted %>%
  group_by(mun_id) %>%
  slice_head(n = 1)

df_1 <- df_1 %>%
  mutate(change_pp = ref_next_PAN_pct - ref_PAN_pct)

df_1$main_estado <- as.factor(df_1$main_estado)
df_1$ref_estado <- as.factor(df_1$ref_estado)

#md_n <- RDestimate(change_pp_wt ~ PAN_pct, cutpoint = 0.5, data = df_n)
one_ref <- rdrobust(y = df_1$change_pp, x = df_1$PAN_pct, covs = cbind(df_1$main_year,df_1$ref_year,df_1$main_estado, df_1$ref_estado), p = 1, bwselect = "cerrd")
summary(one_ref)

df_1_ext <- subset(df_1, ref_PAN_wins_t == 0)
one_ref_ext <- rdrobust(y = df_1_ext$change_pp, x = df_1_ext$PAN_pct, covs = cbind(df_1_ext$main_year,df_1_ext$ref_year,df_1_ext$main_estado, df_1_ext$ref_estado), p = 1, bwselect = "mserd")
summary(one_ref_ext)

df_1_int <- subset(df_1, ref_PAN_wins_t == 1)
one_ref_int <- rdrobust(y = df_1_int$change_pp, x = df_1_int$PAN_pct, covs = cbind(df_1_int$main_year,df_1_int$ref_year,df_1_int$main_estado, df_1_int$ref_estado), p = 1, bwselect = "mserd")
summary(one_ref_int)

rdr_bw <- rdbwselect(y = df_1$ref_PAN_wins_t, x = df_1$PAN_pct, bwselect = "cerrd")

png(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/3YP_Presentation_2_17_25/images/rdplot_nearest.png", width = 6, height = 4, units = "in", res = 300)
rdplot(y = df_1$ref_PAN_wins_t, x = df_1$PAN_pct, h = rdr_bw$bws[1], p = 1, subset = abs(df_1$PAN_pct) < rdr_bw$bws[1], title = "RD for nearest municipality", x.label = "PAN Vote Share, t", y.label = "Nearest Municipalitiy PAN vote share, t+1")
dev.off()

png(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/3YP_Presentation_2_17_25/images/rdplot_nearest_full_running.png", width = 6, height = 4, units = "in", res = 300)
rdplot(y = df_1$change_pp, x = df_1$PAN_pct, title = "RD for nearest municipality", x.label = "PAN Vote Share, t", y.label = "Nearest Municipalitiy PAN vote share, t+1")
dev.off()

##EXAMINE SUPPLY SIDE

#are they just dropping out in nearby places?
df_1$PAN_runs_next <- ifelse(df_1$ref_PAN_pct > 0.5, 1, 0)
summary(df_1$PAN_runs_next)
pan_runs <- rdrobust(y = df_1$PAN_runs_next, x = df_1$PAN_pct, p = 1, bwselect = "mserd")
summary(pan_runs)

rdplot(y = df_1$PAN_runs_next, x = df_1$PAN_pct)

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


## RDROBUST, ALL THRE ESTIMATES ##
# First, sort by mun_id and then by d
df_rdd_sorted <- df_rdd %>%
  arrange(mun_id, dH)

# Pre-allocate the result matrix
bw1 <- matrix(NA, nrow = 100, ncol = 4)
bw2 <- matrix(NA, nrow = 100, ncol = 4)
bw3 <- matrix(NA, nrow = 100, ncol = 4)

# Vector of n values
n_values <- 1:10

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
#bw2$ci_type <- 2
bw3$ci_type <- 2
combined_df <- bind_rows(bw1, bw3)

# Convert bw to a factor variable with specified labels
combined_df$ci_type <- factor(combined_df$ci_type, levels = c(1, 2), labels = c("Conventional", "Robust"))

# Create the plot
ggplot(data = combined_df, aes(x = n, y = est, color = ci_type, fill = ci_type)) +
  #geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.3, width = 0.2, position = position_dodge(width = -0.75)) +
  #geom_point(position = position_dodge(width = -0.75)) +
  geom_line() +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.3, color = "white") +
  #scale_color_viridis_d(direction = -1) +
  labs(x = "Number of References in Average", y = "Estimate", title = "RD Estimates by number of references included") +
  theme_bw()

df_n <- df_rdd_sorted %>%
  group_by(mun_id) %>%
  slice_head(n = 2) %>%
  summarise(
    PAN_pct = mean(PAN_pct, na.rm = TRUE),
    weighted_avg_npp = sum(ref_next_PAN_pct * weight) / sum(weight),
    weighted_avg_pp = sum(ref_PAN_pct * weight) / sum(weight),
    mean_PAN_wins = mean(ref_PAN_wins)
  )

df_n <- df_n %>%
  mutate(change_pp_wt = weighted_avg_npp - weighted_avg_pp)

md_rdr <- rdrobust(y = df_n$change_pp_wt, x = df_n$PAN_pct, p = 1, bwselect = "mserd")
summary(md_rdr)

df_n_ext <- subset(df_n, mean_PAN_wins == 0)

md_rdr <- rdrobust(y = df_n_ext$change_pp_wt, x = df_n_ext$PAN_pct, p = 1, bwselect = "mserd")
summary(md_rdr)

df_n_int <- subset(df_n, mean_PAN_wins > 0)

md_rdr <- rdrobust(y = df_n_int$change_pp_wt, x = df_n_int$PAN_pct, p = 1, bwselect = "mserd")
summary(md_rdr)
