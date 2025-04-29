library(ggplot2)
library(dplyr)
library(rdrobust)
library(xtable)

source("~/mexico_mun/code/function_create_rd_table.R") #for function create_model_table

load("C:/Users/adamd/Documents/mexico_mun/data/PRD_nn.Rdata")
load("C:/Users/adamd/Documents/mexico_mun/data/PAN_nn.Rdata")

#APPENDIX STUFF

#Effect on other party
#table PRD

nc_PRD <- rdrobust(y = PRD_nn$change_pp_PAN, x = PRD_nn$PRD_margin, p = 1, bwselect = "cerrd", level = 90)

cerm_PRD <- rdrobust(y = PRD_nn$change_pp_PAN, x = PRD_nn$PRD_margin, p = 1, covs = cbind(PRD_nn$main_year, PRD_nn$main_estado, PRD_nn$dH), bwselect = "cerrd", level = 90)

PRD_PRI <- rdrobust(y = PRD_nn$change_pp_PRI, x = PRD_nn$PRD_margin, p = 1, bwselect = "cerrd", level = 90)

PRD_PRI_controls <- rdrobust(y = PRD_nn$change_pp_PRI, x = PRD_nn$PRD_margin, p = 1, covs = cbind(PRD_nn$main_year, PRD_nn$main_estado, PRD_nn$dH), bwselect = "cerrd", level = 90)

create_model_table(PRD_PRI, PRD_PRI_controls,nc_PRD,cerm_PRD)

#table PAN
nc_PAN <- rdrobust(y = PAN_nn$change_pp_PRD, x = PAN_nn$PAN_margin, p = 1, bwselect = "cerrd", level = 90)

cerm_PAN <- rdrobust(y = PAN_nn$change_pp_PRD, x = PAN_nn$PAN_margin, p = 1, covs = cbind(PAN_nn$main_year, PAN_nn$main_estado, PAN_nn$dH), bwselect = "cerrd", level = 90)

create_model_table(nc_PRD, cerm_PRD, nc_PAN, cerm_PAN, output_type = "latex")

##EXAMINE SUPPLY SIDE

#are they just dropping out in nearby places?
PRD_nn$PRD_runs_next <- ifelse(PRD_nn$ref_next_PRD_pct > 0, 1, 0)
summary(PRD_nn$PRD_runs_next)

PRD_runs <- rdrobust(y = PRD_nn$PRD_runs_next, x = PRD_nn$PRD_margin, p = 1, covs = cbind(PRD_nn$main_year, PRD_nn$main_estado,PRD_nn$dH), bwselect = "cerrd", level= 90)
summary(PRD_runs)

create_model_table(PRD_runs)

PRD_nn$ref_PRD_wins_t2 <- ifelse(PRD_nn$ref_next_PRD_margin > 0, 1, 0)
summary(PRD_nn$ref_PRD_wins_t2)

PRD_wins <- rdrobust(y = PRD_nn$ref_PRD_wins_t2, x = PRD_nn$PRD_margin, p = 1, 
                     covs = cbind(PRD_nn$main_year,PRD_nn$main_estado,PRD_nn$dH), 
                     bwselect = "cerrd", level= 90)
summary(PRD_wins)

create_model_table(PRD_wins)


#PAN
PAN_nn$PAN_runs_next <- ifelse(PAN_nn$ref_next_PAN_pct > 0, 1, 0)
summary(PAN_nn$PAN_runs_next) #no muns where PAN doesn't run!

PAN_runs <- rdrobust(y = PAN_nn$PAN_runs_next, x = PAN_nn$PAN_margin, p = 1, covs = cbind(PAN_nn$main_year, PAN_nn$main_estado,PAN_nn$dH), bwselect = "cerrd", level= 90)
summary(PAN_runs)

PAN_nn$ref_PAN_wins_t2 <- ifelse(PAN_nn$ref_next_PAN_pct > 0, 1, 0)
PAN_wins <- rdrobust(y = PAN_nn$ref_PAN_wins_t2, x = PAN_nn$PAN_pct, p = 1, covs = cbind(PAN_nn$main_year, PAN_nn$main_estado,PAN_nn$dH), bwselect = "cerrd", level= 90)
summary(PAN_wins)

create_model_table(PRD_wins, PAN_wins)

