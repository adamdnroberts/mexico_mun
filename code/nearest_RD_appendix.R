library(ggplot2)
library(dplyr)
library(rdrobust)
library(xtable)

source("~/mexico_mun/code/functions/function_create_rd_table.R")

load("C:/Users/adamd/Documents/mexico_mun/data/nearest_neighbor_PRD.Rdata")
load("C:/Users/adamd/Documents/mexico_mun/data/nearest_neighbor_PAN.Rdata")

#APPENDIX STUFF

#Effect on other party
#table PRD

nc_PRD <- rdrobust(y = nearest__neighbor_PRD$change_pct_PAN, x = nearest__neighbor_PRD$PRD_margin, p = 1, bwselect = "cerrd", level = 90)

cerm_PRD <- rdrobust(y = nearest__neighbor_PRD$change_pct_PAN, x = nearest__neighbor_PRD$PRD_margin, p = 1, covs = cbind(nearest__neighbor_PRD$main_year, nearest__neighbor_PRD$main_estado, nearest__neighbor_PRD$dH), bwselect = "cerrd", level = 90)

PRD_PRI <- rdrobust(y = nearest__neighbor_PRD$change_pct_PRI, x = nearest__neighbor_PRD$PRD_margin, p = 1, bwselect = "cerrd", level = 90)

PRD_PRI_controls <- rdrobust(y = nearest__neighbor_PRD$change_pct_PRI, x = nearest__neighbor_PRD$PRD_margin, p = 1, covs = cbind(nearest__neighbor_PRD$main_year, nearest__neighbor_PRD$main_estado, nearest__neighbor_PRD$dH), bwselect = "cerrd", level = 90)

create_model_table(PRD_PRI, PRD_PRI_controls,nc_PRD,cerm_PRD)

#table PAN
nc_PAN <- rdrobust(y = nearest_neighbor_PAN$change_pct_PRD, x = nearest_neighbor_PAN$PAN_margin, p = 1, bwselect = "cerrd", level = 90)

cerm_PAN <- rdrobust(y = nearest_neighbor_PAN$change_pct_PRD, x = nearest_neighbor_PAN$PAN_margin, p = 1, covs = cbind(nearest_neighbor_PAN$main_year, nearest_neighbor_PAN$main_estado, nearest_neighbor_PAN$dH), bwselect = "cerrd", level = 90)

create_model_table(nc_PRD, cerm_PRD, nc_PAN, cerm_PAN, output_type = "latex")

##EXAMINE SUPPLY SIDE

#are they just dropping out in nearby places?
nearest__neighbor_PRD$PRD_runs_next <- ifelse(nearest__neighbor_PRD$ref_next_PRD_pct > 0, 1, 0)
summary(nearest__neighbor_PRD$PRD_runs_next)

PRD_runs <- rdrobust(y = nearest__neighbor_PRD$PRD_runs_next, x = nearest__neighbor_PRD$PRD_margin, p = 1, covs = cbind(nearest__neighbor_PRD$main_year, nearest__neighbor_PRD$main_estado,nearest__neighbor_PRD$dH), bwselect = "cerrd", level= 90)
summary(PRD_runs)

create_model_table(PRD_runs)

nearest__neighbor_PRD$ref_PRD_wins_t2 <- ifelse(nearest__neighbor_PRD$ref_next_PRD_margin > 0, 1, 0)
summary(nearest__neighbor_PRD$ref_PRD_wins_t2)

PRD_wins <- rdrobust(y = nearest__neighbor_PRD$ref_PRD_wins_t2, x = nearest__neighbor_PRD$PRD_margin, p = 1, 
                     covs = cbind(nearest__neighbor_PRD$main_year,nearest__neighbor_PRD$main_estado,nearest__neighbor_PRD$dH), 
                     bwselect = "cerrd", level= 90)
summary(PRD_wins)

create_model_table(PRD_wins)


#PAN
nearest_neighbor_PAN$PAN_runs_next <- ifelse(nearest_neighbor_PAN$ref_next_PAN_pct > 0, 1, 0)
summary(nearest_neighbor_PAN$PAN_runs_next) #no muns where PAN doesn't run!

PAN_runs <- rdrobust(y = nearest_neighbor_PAN$PAN_runs_next, x = nearest_neighbor_PAN$PAN_margin, p = 1, covs = cbind(nearest_neighbor_PAN$main_year, nearest_neighbor_PAN$main_estado,nearest_neighbor_PAN$dH), bwselect = "cerrd", level= 90)
summary(PAN_runs)

nearest_neighbor_PAN$ref_PAN_wins_t2 <- ifelse(nearest_neighbor_PAN$ref_next_PAN_pct > 0, 1, 0)
PAN_wins <- rdrobust(y = nearest_neighbor_PAN$ref_PAN_wins_t2, x = nearest_neighbor_PAN$PAN_pct, p = 1, covs = cbind(nearest_neighbor_PAN$main_year, nearest_neighbor_PAN$main_estado,nearest_neighbor_PAN$dH), bwselect = "cerrd", level= 90)
summary(PAN_wins)

create_model_table(PRD_wins, PAN_wins)

