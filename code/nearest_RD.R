library(ggplot2)
library(dplyr)
library(rdrobust)
library(xtable)

source("~/mexico_mun/code/function_create_rd_table.R") #for function create_model_table

load("C:/Users/adamd/Documents/mexico_mun/data/PRD_nn.Rdata")
load("C:/Users/adamd/Documents/mexico_mun/data/PAN_nn.Rdata")

#table PRD
nc_PRD <- rdrobust(y = PRD_nn$change_pp_PRD, x = PRD_nn$PRD_margin, p = 1, bwselect = "cerrd", level = 90)
summary(nc_PRD)

cerm_PRD <- rdrobust(y = PRD_nn$change_pp_PRD, x = PRD_nn$PRD_margin, p = 1, covs = cbind(PRD_nn$main_year, PRD_nn$main_estado, PRD_nn$dH), bwselect = "cerrd", level = 90)
summary(cerm_PRD)

mich <- subset(PRD_nn, main_estado == "Michoacan")

mich_PRD <- rdrobust(y = mich$change_pp_PRD, x = mich$PRD_margin, p = 1, covs = cbind(mich$main_year, mich$main_estado, mich$dH), bwselect = "cerrd", level = 90)
summary(mich_PRD)

### PAN

#table PAN
nc_PAN <- rdrobust(y = PAN_nn$change_pp_PAN, x = PAN_nn$PAN_margin, p = 1, bwselect = "cerrd", level = 90)

msem_PAN <- rdrobust(y = PAN_nn$change_pp_PAN, x = PAN_nn$PAN_margin, p = 1, covs = cbind(PAN_nn$main_year, PAN_nn$main_estado, PAN_nn$dH), bwselect = "mserd", level = 90)

cerm_PAN <- rdrobust(y = PAN_nn$change_pp_PAN, x = PAN_nn$PAN_margin, p = 1, covs = cbind(PAN_nn$main_year, PAN_nn$main_estado, PAN_nn$dH), bwselect = "cerrd", level = 90)

#CREATE THE TABLE
create_model_table(nc_PRD, cerm_PRD, nc_PAN, cerm_PAN, output_type = "latex")

### Effect on turnout

#remove outliers
PRD_nn_turnout <- subset(PRD_nn, 
                         ref_next_turnout_pct <= 70 & 
                           ref_next_turnout_pct > 0)

PRD_turnout <- rdrobust(y = PRD_nn_turnout$ref_next_turnout_pct, x = PRD_nn_turnout$PRD_margin, p = 1, bwselect = "mserd", level = 90)
summary(PRD_turnout)

PRD_turnout_controls <- rdrobust(y = PRD_nn_turnout$ref_next_turnout_pct, x = PRD_nn_turnout$PRD_margin, p = 1, 
                     covs = cbind(PRD_nn_turnout$main_year, PRD_nn_turnout$main_estado, PRD_nn_turnout$dH), bwselect = "cerrd", level = 90)
summary(PRD_turnout_controls)

test <- rdrobust(y = PRD_nn$ref_next_turnout_pct, x = PRD_nn$PRD_margin, p = 1, 
                 covs = cbind(PRD_nn$main_year, PRD_nn$main_estado, PRD_nn$dH), 
                 bwselect = "cerrd", level = 90)
summary(test)

rdplot(y = PRD_nn$ref_next_turnout_pct, x = PRD_nn$PRD_margin, p = 1, covs = cbind(PRD_nn$main_year, PRD_nn$main_estado, PRD_nn$dH))

plot_df <- subset(PRD_nn, abs(PRD_margin) <= .082)

ggplot(plot_df, aes(x = PRD_margin, y = change_pp_PRI)) +
  geom_point(color = "darkgreen", fill = "lightgreen", data = subset(plot_df, PRD_margin < 0), level = 0.9) +
  geom_point(color = "goldenrod", fill = "yellow", data = subset(plot_df, PRD_margin > 0), level = 0.9) +
  geom_smooth(method = "lm", color = "darkgreen", fill = "lightgreen", data = subset(plot_df, PRD_margin < 0), level = 0.9) +
  geom_smooth(method = "lm", color = "goldenrod", fill = "yellow", data = subset(plot_df, PRD_margin > 0), level = 0.9) +
  geom_vline(xintercept = 0, color = "black", linetype = 2) +
  labs(title = "",
       x = "PRD Vote Margin, t1",
       y = "Change in Turnout") +
  theme_minimal()
