library(ggplot2)
library(dplyr)
library(rdd)
library(viridis)
library(rdrobust)
library(RDHonest)

load("~/mexico_mun/data/rdd_mah_dist.Rdata")

# First, sort by mun_id and then by mah_d
df_rdd_sorted <- df_rdd %>%
  arrange(mun_id, desc(mah_d))

#RD Plot for only 1 reference
df_1 <- df_rdd_sorted %>%
  group_by(mun_id) %>%
  slice_head(n = 1)

unique(df_1$ref_mun_id)

df_rdd$ref_PAN_wins_t <- ifelse(df_rdd$ref_PAN_pct > 0, 1, 0)

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
      weighted_avg_npp = sum(ref_next_PAN_pct * mah_d) / sum(mah_d),
      weighted_avg_pp = sum(ref_PAN_pct * mah_d) / sum(mah_d)
    )
  
  df_n <- df_n %>%
    mutate(change_pp_wt = weighted_avg_npp - weighted_avg_pp)
  
  md_rdr <- rdrobust(y = df_n$change_pp_wt, x = df_n$PAN_pct, covs = df_n$mah_d, p = 1, bwselect = "mserd")
  cer <- rdrobust(y = df_n$change_pp_wt, x = df_n$PAN_pct, covs = df_n$mah_d, p = 1, bwselect = "cerrd")
  
  
  #robust_est[n, ] <- c(md_n$est[1], md_n$ci[1, 1], md_n$ci[1, 2], n)
  robust_est[n, ] <- c(md_rdr$coef[3], md_rdr$ci[3, 1], md_rdr$ci[3, 2], md_rdr$coef[3] - md_rdr$se[3]*1.65,  md_rdr$coef[3] + md_rdr$se[3]*1.65,n,1) 
  cer_est[n,] <- c(cer$coef[3], cer$ci[3, 1], cer$ci[3, 2], cer$coef[3] - cer$se[3]*1.65,  cer$coef[3] + cer$se[3]*1.65, n,2)
}

# Create a data frame for the plot
plot_data <- as.data.frame(rbind(robust_est,cer_est))
plot_data <- na.omit(plot_data)
colnames(plot_data) <- c("est", "ci_lower","ci_upper","ci90low", "ci90high", "n", "bw_type")
plot_data$bw_type <- factor(plot_data$bw_type, levels = c(1,2), labels = c("MSE","CER"))

plot_data$n <- as.factor(plot_data$n)

p <- ggplot(plot_data, aes(x = n, y = est
                           , color = bw_type
            )) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, position = position_dodge(width = -0.5)) +
  geom_point(position = position_dodge(width = -0.5)) +
  labs(x = "Number of References in Weighted Average", y = "RD Estimate", title = "RD Estimates by number of references included", subtitle = "Controlling for matching distance") +
  theme_minimal() +
  scale_color_grey()

print(p)

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/Third Year Paper Results Outline/images/num_refs_mah_dist.png", plot = p, width = 6, height = 4)
