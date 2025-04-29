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


