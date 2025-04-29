library(ggplot2)
library(dplyr)
library(rdrobust)

source("~/mexico_mun/code/function_create_rd_table.R") #for function create_model_table

load("C:/Users/adamd/Documents/mexico_mun/data/PRD_nn.Rdata")
load("C:/Users/adamd/Documents/mexico_mun/data/PAN_nn.Rdata")

#PLOTS
bw <- rdbwselect(y = PAN_nn$change_pp_PAN, x = PAN_nn$PAN_margin, p = 1, covs = cbind(PAN_nn$main_year, PAN_nn$main_estado, PAN_nn$dH), bwselect = "cerrd")

plot_PAN <- subset(PAN_nn, abs(PAN_nn$PAN_margin) < bw$bws[1])

percentiles <- seq(0, 1, by = 0.05)

# Calculate the percentiles
percentile_values <- quantile(plot_PAN$PAN_margin, percentiles, na.rm = TRUE)

plot_PAN_bins <- plot_PAN %>%
  mutate(percentile = cut(PAN_margin, breaks = percentile_values, include.lowest = TRUE, labels = FALSE)) %>%
  group_by(percentile) %>%
  summarise(avg_change_pp = mean(change_pp_PAN, na.rm = TRUE),
            bin_center = mean(PAN_margin, na.rm = TRUE),
            count = n())

PAN_RD <- ggplot(plot_PAN, aes(x = PAN_margin, y = change_pp_PAN)) +
  geom_point(aes(x = bin_center, y = avg_change_pp), alpha = 0.5, size = 3, data = subset(plot_PAN_bins, bin_center < 0), color = "darkgreen") +
  geom_point(aes(x = bin_center, y = avg_change_pp), alpha = 0.5, size = 3, data = subset(plot_PAN_bins, bin_center > 0), color = "blue") +
  geom_smooth(method = "lm", color = "darkgreen", fill = "lightgreen", data = subset(plot_PAN, PAN_margin < 0), level = 0.9) +
  geom_smooth(method = "lm", color = "blue", fill = "lightblue", data = subset(plot_PAN, PAN_margin > 0), level = 0.9) +
  geom_vline(xintercept = 0, color = "black", linetype = 2) +
  #geom_hline(yintercept = 0.5, color = "blue", size = 1, alpha = 0.5) +
  labs(title = "",
       x = "PAN Neighbor Vote Margin, t",
       y = "Change in PAN Vote Margin, t+1") +
  theme_minimal()

print(PAN_RD)

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/TYP_draft/images/PAN_RD.png", plot = PAN_RD, width = 6, height = 4)

#PRD plot
bw <- rdbwselect(y = PRD_nn$change_pp_PRD, x = PRD_nn$PRD_margin, p = 1, covs = cbind(PRD_nn$main_year, PRD_nn$main_estado, PRD_nn$dH), bwselect = "cerrd")

plot_PRD <- subset(PRD_nn, abs(PRD_nn$PRD_margin) < bw$bws[1])
percentiles <- seq(0, 1, by = 0.05)

# Calculate the percentiles
percentile_values <- quantile(plot_PRD$PRD_margin, percentiles, na.rm = TRUE)

plot_PRD_bins <- plot_PRD %>%
  mutate(percentile = cut(PRD_margin, breaks = percentile_values, include.lowest = TRUE, labels = FALSE)) %>%
  group_by(percentile) %>%
  summarise(avg_change_pp = mean(change_pp_PRD, na.rm = TRUE),
            bin_center = mean(PRD_margin, na.rm = TRUE),
            count = n())

PRD_RD <- ggplot(plot_PRD, aes(x = PRD_margin, y = change_pp_PRD)) +
  geom_point(aes(x = bin_center, y = avg_change_pp), alpha = 0.5, size = 3, data = subset(plot_PRD_bins, bin_center < 0 & avg_change_pp > -0.2), color = "darkgreen") +
  geom_point(aes(x = bin_center, y = avg_change_pp), alpha = 0.5, size = 3, data = subset(plot_PRD_bins, bin_center > 0), color = "goldenrod") +
  geom_smooth(method = "lm", color = "darkgreen", fill = "lightgreen", data = subset(plot_PRD, PRD_margin < 0), level = 0.9) +
  geom_smooth(method = "lm", color = "goldenrod", fill = "yellow", data = subset(plot_PRD, PRD_margin > 0), level = 0.9) +
  geom_vline(xintercept = 0, color = "black", linetype = 2) +
  labs(title = "",
       x = "PRD Vote Margin, t1",
       y = "Change in PRD vote share") +
  theme_minimal()

print(PRD_RD)

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/TYP_draft/images/PRD_RD.png", plot = PRD_RD, width = 6, height = 4)

