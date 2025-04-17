library(ggplot2)
library(dplyr)
library(rdrobust)

#load datasets

load("~/mexico_mun/data/rdd_distance_PRD.Rdata")

df_rdd_PRD_new <- subset(df_rdd_PRD, ref_PRD_wins == 0 & main_estado == ref_estado)

PRD_nn <- df_rdd_PRD_new %>%
  group_by(mun_id) %>%
  slice_head(n = 1)

PRD_nn <- PRD_nn %>%
  mutate(change_pp_PRD = ref_next_PRD_pct - ref_PRD_pct,
         change_pp_PAN = ref_next_PAN_pct - ref_PAN_pct)

PRD_nn$main_estado <- as.factor(PRD_nn$main_estado)

load("~/mexico_mun/data/rdd_distance_PAN.Rdata")

df_rdd_PAN_new <- subset(df_rdd_PAN, ref_PAN_wins == 0 & main_estado == ref_estado)

PAN_nn <- df_rdd_PAN_new %>%
  group_by(mun_id) %>%
  slice_head(n = 1)

PAN_nn <- PAN_nn %>%
  mutate(change_pp_PRD = ref_next_PRD_pct - ref_PRD_pct,
         change_pp_PAN = ref_next_PAN_pct - ref_PAN_pct)

PAN_nn$main_estado <- as.factor(PAN_nn$main_estado)

# Define function to run models at different bandwidths and confidence levels
run_rd_models <- function(data, y, x, bandwidths_pct, confidence_level) {
  # Get the base bandwidth from rdbwselect
  main_bw <- rdbwselect(
    y = y, 
    x = x, 
    p = 1, 
    covs = cbind(data$main_year, data$main_estado, data$dH), 
    bwselect = "cerrd"
  )
  
  # Calculate bandwidth multipliers
  bw_multipliers <- bandwidths_pct / 100
  
  # Initialize list to store models
  models <- list()
  
  # Run models for different bandwidths
  for (i in seq_along(bw_multipliers)) {
    models[[i]] <- rdrobust(
      y = y, 
      x = x, 
      p = 1, 
      covs = cbind(data$main_year, data$main_estado, data$dH), 
      bwselect = "cerrd", 
      level = confidence_level, 
      h = main_bw$bws[1] * bw_multipliers[i],
      rho = 0.417
    )
  }
  
  # Extract coefficients and confidence intervals
  results <- data.frame(
    Bandwidth = bandwidths_pct,
    Coefficient = sapply(models, function(m) m$coef[3]),
    CI_Lower = sapply(models, function(m) m$ci[3, 1]),
    CI_Upper = sapply(models, function(m) m$ci[3, 2])
  )
  
  return(results)
}

# Define bandwidth percentages to test
bandwidths_pct <- c(50, 75, 100, 125, 150, 175, 200)

#PRD

# Run models with 90% confidence intervals
results_90 <- run_rd_models(PRD_nn, PRD_nn$change_pp_PRD, PRD_nn$PRD_margin, bandwidths_pct, 90)
names(results_90)[3:4] <- c("CI_Lower90", "CI_Upper90")

# Run models with 95% confidence intervals
results_95 <- run_rd_models(PRD_nn, PRD_nn$change_pp_PRD, PRD_nn$PRD_margin, bandwidths_pct, 95)
names(results_95)[3:4] <- c("CI_Lower95", "CI_Upper95")

# Merge results
coef_data <- merge(results_90, results_95, by = c("Bandwidth", "Coefficient"))

PRD_bws <- ggplot(coef_data, aes(x = as.factor(Bandwidth), y = Coefficient)) +
  # 95% CI - thinner error bars
  geom_errorbar(aes(ymin = CI_Lower95, ymax = CI_Upper95), 
                width = 0, linewidth = 0.5) +
  # 90% CI - fatter error bars  
  geom_errorbar(aes(ymin = CI_Lower90, ymax = CI_Upper90), 
                width = 0, linewidth = 2, alpha = 0.4) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", alpha = 0.5) +
  labs(title = "",
       x = "Percent Bandwidth",
       y = "Coefficient",
       caption = "Thick bars: 90% CI, Thin bars: 95% CI") +
  theme_minimal()

print(PRD_bws)

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/TYP Final Tables and Figures Appendix/images/PRD_bw_pcts.png", plot = PRD_bws, width = 6, height = 4)

#PAN

# Run models with 90% confidence intervals
results_90 <- run_rd_models(PAN_nn, PAN_nn$change_pp_PAN, PAN_nn$PAN_margin, bandwidths_pct, 90)
names(results_90)[3:4] <- c("CI_Lower90", "CI_Upper90")

# Run models with 95% confidence intervals
results_95 <- run_rd_models(PAN_nn, PAN_nn$change_pp_PAN, PAN_nn$PAN_margin, bandwidths_pct, 95)
names(results_95)[3:4] <- c("CI_Lower95", "CI_Upper95")

# Merge results
coef_data <- merge(results_90, results_95, by = c("Bandwidth", "Coefficient"))

PAN_bws <- ggplot(coef_data, aes(x = as.factor(Bandwidth), y = Coefficient)) +
  # 95% CI - thinner error bars
  geom_errorbar(aes(ymin = CI_Lower95, ymax = CI_Upper95), 
                width = 0, linewidth = 0.5) +
  # 90% CI - fatter error bars  
  geom_errorbar(aes(ymin = CI_Lower90, ymax = CI_Upper90), 
                width = 0, linewidth = 2, alpha = 0.4) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", alpha = 0.5) +
  labs(title = "",
       x = "Percent Bandwidth",
       y = "Coefficient",
       caption = "Thick bars: 90% CI, Thin bars: 95% CI") +
  theme_minimal()

print(PAN_bws)

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/TYP Final Tables and Figures Appendix/images/PAN_bw_pcts.png", plot = PAN_bws, width = 6, height = 4)

