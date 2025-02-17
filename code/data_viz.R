library(dplyr)
library(ggplot2)
library(data.table)
library(readxl)
library(rdd)

#dataviz
load("~/mexico_mun/data/full_dataset_mexelec.Rdata")

df <- subset(big_df, year>=1995 & year <= 2000) # & (p1_name == "PRI" | p2_name == "PRI") & (p1_name == "PAN" | p2_name == "PAN"))

df_plot <- subset(big_df, year>=1995 & year <= 1997)

png(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/3YP_Presentation_2_17_25/images/McCrary.png", width = 6, height = 4, units = "in", res = 300)
DCdensity(df_plot$PAN_pct, cutpoint = 0.5)
abline(v = 0.5, col = "red", lwd = 0.5)
title(x = "PAN vote share")
dev.off()

## OLD CODE ##

next_elec <- RDestimate(next_PAN_pct ~ PAN_pct, cutpoint = 0.5, data = df)
summary(next_elec)

#probably good that this doesn't show an effect?
ggplot(df, aes(x = PAN_pct, y = next_PAN_pct)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "darkgreen", data = subset(df, PAN_pct < 0.5)) +
  geom_smooth(method = "loess", color = "blue", data = subset(df, PAN_pct >= 0.5)) +
  geom_vline(xintercept = 0.5, color = "red") +
  #geom_hline(yintercept = 0.5, color = "blue", size = 1, alpha = 0.5) +
  labs(title = "Regression Discontinuity Design",
       x = "PAN vote share, t",
       y = "PAN vote share, t+1") +
  theme_minimal()

df <- subset(df, estado != "Tlaxcala" & estado != "Baja California Sur")

#by estado
ggplot(df, aes(x = PAN_pct)) +
  geom_histogram(alpha = 0.5) +
  geom_vline(xintercept = 0.5, color = "red") +
  facet_wrap(~estado) +
  theme_minimal()