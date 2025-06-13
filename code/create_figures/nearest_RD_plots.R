library(ggplot2)
library(dplyr)
library(rdrobust)

load("C:/Users/adamd/Documents/mexico_mun/data/nearest_neighbor_PRD.Rdata")
load("C:/Users/adamd/Documents/mexico_mun/data/nearest_neighbor_PAN.Rdata")

#PRD plot
bw <- rdbwselect(y = nearest_neighbor_PRD$change_pct_PRD, x = nearest_neighbor_PRD$PRD_margin, p = 1, 
                 #covs = cbind(nearest_neighbor_PRD$main_year, nearest_neighbor_PRD$main_estado, nearest_neighbor_PRD$dH), 
                 bwselect = "cerrd")

plot_PRD <- subset(nearest_neighbor_PRD, abs(nearest_neighbor_PRD$PRD_margin) < bw$bws[1])
percentiles <- seq(0, 1, by = 0.1)

# Calculate the percentiles
percentile_values <- quantile(plot_PRD$PRD_margin, percentiles, na.rm = TRUE)

plot_PRD_bins <- plot_PRD %>%
  mutate(percentile = cut(PRD_margin, breaks = percentile_values, include.lowest = TRUE, labels = FALSE)) %>%
  group_by(percentile) %>%
  summarise(avg_change_pp = mean(change_pct_PRD, na.rm = TRUE),
            bin_center = mean(PRD_margin, na.rm = TRUE),
            count = n())

PRD_RD <- ggplot(plot_PRD, aes(x = PRD_margin, y = change_pct_PRD)) +
  geom_point(aes(x = bin_center, y = avg_change_pp), alpha = 0.5, size = 3, data = subset(plot_PRD_bins, bin_center < 0), color = "darkgreen") +
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


#PAN plot
bw <- rdbwselect(y = nearest_neighbor_PAN$change_pct_PAN, x = nearest_neighbor_PAN$PAN_margin, p = 1, covs = cbind(nearest_neighbor_PAN$main_year, nearest_neighbor_PAN$main_estado, nearest_neighbor_PAN$dH), bwselect = "cerrd")

plot_PAN <- subset(nearest_neighbor_PAN, abs(nearest_neighbor_PAN$PAN_margin) < bw$bws[1])

percentiles <- seq(0, 1, by = 0.05)

# Calculate the percentiles
percentile_values <- quantile(plot_PAN$PAN_margin, percentiles, na.rm = TRUE)

plot_PAN_bins <- plot_PAN %>%
  mutate(percentile = cut(PAN_margin, breaks = percentile_values, include.lowest = TRUE, labels = FALSE)) %>%
  group_by(percentile) %>%
  summarise(avg_change_pp = mean(change_pct_PAN, na.rm = TRUE),
            bin_center = mean(PAN_margin, na.rm = TRUE),
            count = n())

PAN_RD <- ggplot(plot_PAN, aes(x = PAN_margin, y = change_pct_PAN)) +
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

