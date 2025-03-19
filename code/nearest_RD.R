library(ggplot2)
library(dplyr)
library(viridis)
library(rdrobust)
library(xtable)

load("~/mexico_mun/data/rdd_distance_PRD.Rdata")

df_1_PRD <- df_rdd_PRD %>%
  group_by(mun_id) %>%
  slice_head(n = 1)

df_1_PRD <- df_1_PRD %>%
  mutate(change_pp = ref_next_PRD_pct - ref_PRD_pct)

df_1_PRD$main_estado <- as.factor(df_1_PRD$main_estado)
df_1_PRD$ref_estado <- as.factor(df_1_PRD$ref_estado)

#table PRD
nc_PRD <- rdrobust(y = df_1_PRD$change_pp, x = df_1_PRD$PRD_pct, p = 1, bwselect = "cerrd", level = 90)

msem_PRD <- rdrobust(y = df_1_PRD$change_pp, x = df_1_PRD$PRD_pct, p = 1, covs = cbind(df_1_PRD$main_year, df_1_PRD$main_estado, df_1_PRD$dH), bwselect = "mserd", level = 90)

cerm_PRD <- rdrobust(y = df_1_PRD$change_pp, x = df_1_PRD$PRD_pct, p = 1, covs = cbind(df_1_PRD$main_year, df_1_PRD$main_estado, df_1_PRD$dH), bwselect = "cerrd", level = 90)

### PAN
load("~/mexico_mun/data/rdd_distance_PAN.Rdata")

df_1_PAN <- df_rdd_PAN %>%
  group_by(mun_id) %>%
  slice_head(n = 1)

df_1_PAN <- df_1_PAN %>%
  mutate(change_pp = ref_next_PAN_pct - ref_PAN_pct)

df_1_PAN$main_estado <- as.factor(df_1_PAN$main_estado)
df_1_PAN$ref_estado <- as.factor(df_1_PAN$ref_estado)

#table PAN
nc_PAN <- rdrobust(y = df_1_PAN$change_pp, x = df_1_PAN$PAN_pct, p = 1, bwselect = "cerrd", level = 90)

msem_PAN <- rdrobust(y = df_1_PAN$change_pp, x = df_1_PAN$PAN_pct, p = 1, covs = cbind(df_1_PAN$main_year, df_1_PAN$main_estado, df_1_PAN$dH), bwselect = "mserd", level = 90)

cer_PAN <- rdrobust(y = df_1_PAN$change_pp, x = df_1_PAN$PAN_pct, p = 1, 
                 covs = cbind(df_1_PAN$main_year, df_1_PAN$main_estado, df_1_PAN$dH), 
                 bwselect = "cerrd", level = 90)

create_model_table <- function(..., metrics = c("Party", "Coefficient", "Standard Error", "p Value", "BW Type", "Bandwidth", "N", "Effective N")) {
  models <- list(...)
  
  # Initialize an empty list to store the values for each model
  model_values <- list()
  
  for (model in models) {
    coef_value <- round(model$coef[3], 3)
    se_value <- round(model$se[3], 3)
    pv_value <- round(model$pv[3], 3)
    bw_type <- ifelse(model$bwselect=="mserd","MSE","CER")
    bws_value <- round(model$bws[1], 3)
    N_sum <- sum(model$N)
    N_b_sum <- sum(model$N_h)
    party <- ifelse(pv_value < 0.1, "PRD","PAN")
    
    model_values[[length(model_values) + 1]] <- c(party, coef_value, se_value, pv_value, bw_type, bws_value, N_sum, N_b_sum)
  }
  
  # Combine the values into a data frame
  result_table <- data.frame(
    Metric = metrics,
    do.call(cbind, model_values)
  )
  
  # Convert the data frame to a LaTeX table using xtable
  latex_table <- xtable(result_table, include.rownames = FALSE)
  
  # Print the LaTeX table
  print(latex_table, type = "latex")
}

# Example usage
create_model_table(nc_PRD, cerm_PRD, msem_PRD, nc_PAN, cer_PAN, msem_PAN)

#PLOTS

plot_PAN <- subset(df_1_PAN, abs(df_1_PAN$PAN_pct) < 0.05)

percentiles <- seq(0, 1, by = 0.05)

# Calculate the percentiles
percentile_values <- quantile(plot_PAN$PAN_pct, percentiles, na.rm = TRUE)

plot_PAN_bins <- plot_PAN %>%
  mutate(percentile = cut(PAN_pct, breaks = percentile_values, include.lowest = TRUE, labels = FALSE)) %>%
  group_by(percentile) %>%
  summarise(avg_change_pp = mean(change_pp, na.rm = TRUE),
            bin_center = mean(PAN_pct, na.rm = TRUE),
            count = n())


PAN_RD <- ggplot(plot_PAN, aes(x = PAN_pct, y = change_pp)) +
  geom_point(aes(x = bin_center, y = avg_change_pp), alpha = 0.5, data = subset(plot_PAN_bins, bin_center < 0), color = "darkgreen") +
  geom_point(aes(x = bin_center, y = avg_change_pp), alpha = 0.5, data = subset(plot_PAN_bins, bin_center > 0), color = "blue") +
  geom_smooth(method = "lm", color = "darkgreen", fill = "lightgreen", data = subset(plot_PAN, PAN_pct < 0), level = 0.9) +
  geom_smooth(method = "lm", color = "blue", fill = "lightblue", data = subset(plot_PAN, PAN_pct > 0), level = 0.9) +
  geom_vline(xintercept = 0, color = "black", linetype = 2) +
  #geom_hline(yintercept = 0.5, color = "blue", size = 1, alpha = 0.5) +
  labs(title = "",
       x = "PAN vote share, t",
       y = "Change in PAN vote share, t+1") +
  theme_minimal()

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/TYP Final Tables and Figures/images/PAN_RD.png", plot = PAN_RD, width = 6, height = 4)


plot_PRD <- subset(df_1_PRD, abs(df_1_PRD$PRD_pct) < 0.05)

percentiles <- seq(0, 1, by = 0.05)

# Calculate the percentiles
percentile_values <- quantile(plot_PRD$PRD_pct, percentiles, na.rm = TRUE)

plot_PRD_bins <- plot_PRD %>%
  mutate(percentile = cut(PRD_pct, breaks = percentile_values, include.lowest = TRUE, labels = FALSE)) %>%
  group_by(percentile) %>%
  summarise(avg_change_pp = mean(change_pp, na.rm = TRUE),
             bin_center = mean(PRD_pct, na.rm = TRUE),
             count = n())

PRD_RD <- ggplot(plot_PRD, aes(x = PRD_pct, y = change_pp)) +
  geom_point(aes(x = bin_center, y = avg_change_pp), alpha = 0.5, data = subset(plot_PRD_bins, bin_center < 0 & avg_change_pp > -0.2), color = "darkgreen") +
  geom_point(aes(x = bin_center, y = avg_change_pp), alpha = 0.5, data = subset(plot_PRD_bins, bin_center > 0), color = "goldenrod") +
  geom_smooth(method = "lm", color = "darkgreen", fill = "lightgreen", data = subset(plot_PRD, PRD_pct < 0), level = 0.9) +
  geom_smooth(method = "lm", color = "goldenrod", fill = "yellow", data = subset(plot_PRD, PRD_pct > 0), level = 0.9) +
  geom_vline(xintercept = 0, color = "black", linetype = 2) +
  labs(title = "",
       x = "PRD vote share, t",
       y = "Change in PRD vote share, t+1") +
  theme_minimal()

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/TYP Final Tables and Figures/images/PRD_RD.png", plot = PRD_RD, width = 6, height = 4)


#APPENDIX STUFF
# DO I NEED THIS?

##EXAMINE SUPPLY SIDE

#are they just dropping out in nearby places?
df_1_PRD$PRD_runs_next <- ifelse(df_1_PRD$ref_next_PRD_pct > -0.5, 1, 0)
summary(df_1_PRD$PRD_runs_next)

PRD_runs <- rdrobust(y = df_1_PRD$PRD_runs_next, x = df_1_PRD$PRD_pct, p = 1, covs = cbind(df_1_PRD$main_year, df_1_PRD$main_estado,df_1_PRD$dH), bwselect = "cerrd", level= 90)
summary(PRD_runs)

df_1_PRD$ref_PRD_wins_t2 <- ifelse(df_1_PRD$ref_next_PRD_pct > 0, 1, 0)
PRD_wins <- rdrobust(y = df_1_PRD$ref_PRD_wins_t2, x = df_1_PRD$PRD_pct, p = 1, covs = cbind(df_1_PRD$main_year, df_1_PRD$main_estado,df_1_PRD$dH), bwselect = "cerrd", level= 90)
summary(PRD_wins)

df_1_PAN$PAN_runs_next <- ifelse(df_1_PAN$ref_next_PAN_pct > -0.5, 1, 0)
summary(df_1_PAN$PAN_runs_next)

PAN_runs <- rdrobust(y = df_1_PAN$PAN_runs_next, x = df_1_PAN$PAN_pct, p = 1, covs = cbind(df_1_PAN$main_year, df_1_PAN$main_estado,df_1_PAN$dH), bwselect = "cerrd", level= 90)
summary(PAN_runs)

df_1_PAN$ref_PAN_wins_t2 <- ifelse(df_1_PAN$ref_next_PAN_pct > 0, 1, 0)
PAN_wins <- rdrobust(y = df_1_PAN$ref_PAN_wins_t2, x = df_1_PAN$PAN_pct, p = 1, covs = cbind(df_1_PAN$main_year, df_1_PAN$main_estado,df_1_PAN$dH), bwselect = "cerrd", level= 90)
summary(PAN_wins)

rdplot(y = df_1_PAN$PAN_runs_next, x = df_1_PAN$PAN_pct, p = 1, subset = abs(df_1_PAN$PAN_pct) < 0.058)
