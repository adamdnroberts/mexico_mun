library(dplyr)
library(ggplot2)
library(data.table)
library(readxl)
library(rdd)

setwd("~/mexico_RD")

#dataviz
load("~/mexico_RD/full_dataset_mexelec.Rdata")

df <- subset(big_df, year>=1998 & year <= 2000) # & (p1_name == "PRI" | p2_name == "PRI") & (p1_name == "PAN" | p2_name == "PAN"))

DCdensity(df$PAN_pct, cutpoint = 0.5)
abline(v = 0.5, col = "red", lwd = 2)
title(x = "PAN vote share")

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

