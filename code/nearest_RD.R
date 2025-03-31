library(ggplot2)
library(dplyr)
library(viridis)
library(rdrobust)
library(xtable)

source("~/mexico_mun/code/function_create_rd_table.R") #for function create_model_table

load("~/mexico_mun/data/rdd_distance_PRD.Rdata")

df_rdd_PRD_new <- subset(df_rdd_PRD, ref_PRD_wins == 0 & main_estado == ref_estado & ref_next_PRD_pct > -0.5)

PRD_nn <- df_rdd_PRD_new %>%
  group_by(mun_id) %>%
  slice_head(n = 1)

PRD_nn <- PRD_nn %>%
  mutate(change_pp_PRD = ref_next_PRD_pct - ref_PRD_pct,
         change_pp_PAN = ref_next_PAN_pct - ref_PAN_pct)

PRD_nn$main_estado <- as.factor(PRD_nn$main_estado)

#table PRD
nc_PRD <- rdrobust(y = PRD_nn$change_pp_PRD, x = PRD_nn$PRD_pct, p = 1, bwselect = "cerrd", level = 90)

msem_PRD <- rdrobust(y = PRD_nn$change_pp_PRD, x = PRD_nn$PRD_pct, p = 1, covs = cbind(PRD_nn$main_year, PRD_nn$main_estado, PRD_nn$dH), bwselect = "mserd", level = 90)

cerm_PRD <- rdrobust(y = PRD_nn$change_pp_PRD, x = PRD_nn$PRD_pct, p = 1, covs = cbind(PRD_nn$main_year, PRD_nn$main_estado, PRD_nn$dH), bwselect = "cerrd", level = 90)

### PAN
load("~/mexico_mun/data/rdd_distance_PAN.Rdata")

df_rdd_PAN_new <- subset(df_rdd_PAN, ref_PAN_wins == 0 & main_estado == ref_estado & ref_next_PAN_pct > -0.5)

PAN_nn <- df_rdd_PAN_new %>%
  group_by(mun_id) %>%
  slice_head(n = 1)

PAN_nn <- PAN_nn %>%
  mutate(change_pp_PAN = ref_next_PAN_pct - ref_PAN_pct,
         change_pp_PAN = ref_next_PAN_pct - ref_PAN_pct)

PAN_nn$main_estado <- as.factor(PAN_nn$main_estado)

#table PAN
nc_PAN <- rdrobust(y = PAN_nn$change_pp_PAN, x = PAN_nn$PAN_pct, p = 1, bwselect = "cerrd", level = 90)

msem_PAN <- rdrobust(y = PAN_nn$change_pp_PAN, x = PAN_nn$PAN_pct, p = 1, covs = cbind(PAN_nn$main_year, PAN_nn$main_estado, PAN_nn$dH), bwselect = "mserd", level = 90)

cerm_PAN <- rdrobust(y = PAN_nn$change_pp_PAN, x = PAN_nn$PAN_pct, p = 1, covs = cbind(PAN_nn$main_year, PAN_nn$main_estado, PAN_nn$dH), bwselect = "cerrd", level = 90)

#CREATE THE TABLE
create_model_table(nc_PRD, cerm_PRD, nc_PAN, cerm_PAN, output_type = "text")

#Effect on other party
#table PRD
nc_PRD <- rdrobust(y = PRD_nn$change_pp_PAN, x = PRD_nn$PRD_pct, p = 1, bwselect = "cerrd", level = 90)

cerm_PRD <- rdrobust(y = PRD_nn$change_pp_PAN, x = PRD_nn$PRD_pct, p = 1, covs = cbind(PRD_nn$main_year, PRD_nn$main_estado, PRD_nn$dH), bwselect = "cerrd", level = 90)

#table PAN
nc_PAN <- rdrobust(y = PAN_nn$change_pp_PRD, x = PAN_nn$PAN_pct, p = 1, bwselect = "cerrd", level = 90)

cerm_PAN <- rdrobust(y = PAN_nn$change_pp_PRD, x = PAN_nn$PAN_pct, p = 1, covs = cbind(PAN_nn$main_year, PAN_nn$main_estado, PAN_nn$dH), bwselect = "cerrd", level = 90)

create_model_table(nc_PRD, cerm_PRD, nc_PAN, cerm_PAN, output_type = "latex")

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


plot_PRD <- subset(PRD_nn, abs(PRD_nn$PRD_pct) < 0.05)

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

##EXAMINE SUPPLY SIDE
df_rdd_PRD_new2 <- subset(df_rdd_PRD, ref_PRD_wins == 0 & main_estado == ref_estado)

PRD_nns <- df_rdd_PRD_new2 %>%
  group_by(mun_id) %>%
  slice_head(n = 1)

PRD_nns <- PRD_nns %>%
  mutate(change_pp_PRD = ref_next_PRD_pct - ref_PRD_pct,
         change_pp_PAN = ref_next_PAN_pct - ref_PAN_pct)

PRD_nns$main_estado <- as.factor(PRD_nns$main_estado)
PRD_nns$ref_estado <- as.factor(PRD_nns$ref_estado)

df_rdd_PAN_new2 <- subset(df_rdd_PAN, ref_PAN_wins == 0 & main_estado == ref_estado)

PAN_nns <- df_rdd_PAN_new2 %>%
  group_by(mun_id) %>%
  slice_head(n = 1)

PAN_nns <- PAN_nns %>%
  mutate(change_pp_PAN = ref_next_PAN_pct - ref_PAN_pct,
         change_pp_PAN = ref_next_PAN_pct - ref_PAN_pct)

PAN_nns$main_estado <- as.factor(PAN_nns$main_estado)
PAN_nns$ref_estado <- as.factor(PAN_nns$ref_estado)

#are they just dropping out in nearby places?
PRD_nns$PRD_runs_next <- ifelse(PRD_nns$ref_next_PRD_pct > -0.5, 1, 0)
summary(PRD_nns$PRD_runs_next)

PRD_runs <- rdrobust(y = PRD_nns$PRD_runs_next, x = PRD_nns$PRD_pct, p = 1, covs = cbind(PRD_nns$main_year, PRD_nns$main_estado,PRD_nns$dH), bwselect = "cerrd", level= 90)
summary(PRD_runs)

create_model_table(PRD_runs)

PRD_nns$ref_PRD_wins_t2 <- ifelse(PRD_nns$ref_next_PRD_pct > 0, 1, 0)
PRD_wins <- rdrobust(y = PRD_nns$ref_PRD_wins_t2, x = PRD_nns$PRD_pct, p = 1, covs = cbind(PRD_nns$main_year, PRD_nns$main_estado,PRD_nns$dH), bwselect = "cerrd", level= 90)
summary(PRD_wins)

#PAN
PAN_nn$PAN_runs_next <- ifelse(PAN_nn$ref_next_PAN_pct > -0.5, 1, 0)
summary(PAN_nn$PAN_runs_next) #no muns where PAN doesn't run!

PAN_nn$ref_PAN_wins_t2 <- ifelse(PAN_nn$ref_next_PAN_pct > 0, 1, 0)
PAN_wins <- rdrobust(y = PAN_nn$ref_PAN_wins_t2, x = PAN_nn$PAN_pct, p = 1, covs = cbind(PAN_nn$main_year, PAN_nn$main_estado,PAN_nn$dH), bwselect = "cerrd", level= 90)
summary(PAN_wins)

create_model_table(PRD_wins, PAN_wins)
