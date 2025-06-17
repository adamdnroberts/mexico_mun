library(ggplot2)
library(dplyr)
library(rdrobust)
library(xtable)

source("~/mexico_mun/code/functions/function_create_rd_table.R")

load("C:/Users/adamd/Documents/mexico_mun/data/nearest_neighbor_PRD.Rdata")
load("C:/Users/adamd/Documents/mexico_mun/data/nearest_neighbor_PAN.Rdata")
load("C:/Users/adamd/Documents/mexico_mun/data/mexico_municipal_elections.Rdata")

###APPENDIX TABLES###

##Selection RD##
nearest_neighbor_PRD$PRD_runs_next <- ifelse(nearest_neighbor_PRD$ref_next_PRD_pct > 0, 1, 0)

PRD_runs <- rdrobust(y = nearest_neighbor_PRD$PRD_runs_next, x = nearest_neighbor_PRD$PRD_margin, p = 1, 
                     covs = cbind(nearest_neighbor_PRD$main_year, nearest_neighbor_PRD$main_estado, 
                                  nearest_neighbor_PRD$dH, nearest_neighbor_PRD$treated_neighbors), 
                     bwselect = "cerrd")

create_model_table(PRD_runs)

#PAN
nearest_neighbor_PAN$PAN_runs_next <- ifelse(nearest_neighbor_PAN$ref_next_PAN_pct > 0, 1, 0)

PAN_runs <- rdrobust(y = nearest_neighbor_PAN$change_pct_PAN, x = nearest_neighbor_PAN$PAN_margin, p = 1, 
                                covs = cbind(nearest_neighbor_PAN$main_year, nearest_neighbor_PAN$main_estado, 
                                             nearest_neighbor_PAN$dH, nearest_neighbor_PAN$treated_neighbors), 
                                bwselect = "cerrd")

create_model_table(PRD_runs, PAN_runs)

##Probability of Winning RD##

nearest_neighbor_PRD$ref_PRD_wins_t2 <- ifelse(nearest_neighbor_PRD$ref_next_PRD_margin > 0, 1, 0)

PRD_wins <- rdrobust(y = nearest_neighbor_PRD$ref_PRD_wins_t2, x = nearest_neighbor_PRD$PRD_margin, p = 1, 
                     covs = cbind(nearest_neighbor_PRD$main_year,nearest_neighbor_PRD$main_estado,
                                  nearest_neighbor_PRD$dH, nearest_neighbor_PRD$treated_neighbors), 
                     bwselect = "cerrd")


#PAN
nearest_neighbor_PAN$ref_PAN_wins_t2 <- ifelse(nearest_neighbor_PAN$ref_next_PAN_pct > 0, 1, 0)
PAN_wins <- rdrobust(y = nearest_neighbor_PAN$ref_PAN_wins_t2, x = nearest_neighbor_PAN$PAN_margin, p = 1, 
                     covs = cbind(nearest_neighbor_PAN$main_year, nearest_neighbor_PAN$main_estado,
                                  nearest_neighbor_PAN$dH, nearest_neighbor_PAN$treated_neighbors), 
                     bwselect = "cerrd")

create_model_table(PRD_wins, PAN_wins)


##Nearest Neighbor Results for Other Opposition Party##

no_covariates_PRD_PAN <- rdrobust(y = nearest_neighbor_PRD$change_pct_PAN, x = nearest_neighbor_PRD$PRD_margin, 
                              p = 1, bwselect = "cerrd")

covariates_PRD_PAN <- rdrobust(y = nearest_neighbor_PRD$change_pct_PAN, x = nearest_neighbor_PRD$PRD_margin, p = 1, 
                               covs = cbind(nearest_neighbor_PRD$main_year, nearest_neighbor_PRD$main_estado, 
                                               nearest_neighbor_PRD$dH, nearest_neighbor_PRD$treated_neighbors), 
                           bwselect = "cerrd")

no_covariates_PRD_PRI <- rdrobust(y = nearest_neighbor_PRD$change_pct_PRI, x = nearest_neighbor_PRD$PRD_margin, p = 1, 
                    bwselect = "cerrd")

covariates_PRD_PRI <- rdrobust(y = nearest_neighbor_PRD$change_pct_PRI, x = nearest_neighbor_PRD$PRD_margin, p = 1, 
                             covs = cbind(nearest_neighbor_PRD$main_year, nearest_neighbor_PRD$main_estado, 
                                          nearest_neighbor_PRD$dH, nearest_neighbor_PRD$treated_neighbors), 
                             bwselect = "cerrd")

create_model_table(no_covariates_PRD_PAN, covariates_PRD_PAN, no_covariates_PRD_PRI, covariates_PRD_PRI, output_type = "latex")

##Within Municipality Results##

mexico_municipalities <- subset(mexico_municipal_elections, year >=1995 & year <= 1997)

PRD_within <- rdrobust(y = mexico_municipalities$PRD_pct, x = mexico_municipalities$PRD_margin, p = 1,
                 covs = cbind(mexico_municipalities$year, as.factor(mexico_municipalities$estado)), 
                 bwselect = "cerrd")

PAN_within <- rdrobust(y = mexico_municipalities$PAN_pct, x = mexico_municipalities$PAN_margin, p = 1,
                       covs = cbind(mexico_municipalities$year, as.factor(mexico_municipalities$estado)), 
                       bwselect = "cerrd")

create_model_table(PRD_within, PAN_within)


##Nearest Neighbor Effect on Turnout##

nearest_neighbor_PRD_no_outliers <- subset(nearest_neighbor_PRD, ref_next_turnout_pct <= 2 & ref_next_turnout_pct > 0)

#PRD models
prd_non_covariate_turnout <- rdrobust(y = nearest_neighbor_PRD_no_outliers$ref_next_turnout_pct, x = nearest_neighbor_PRD_no_outliers$PRD_margin, p = 1, 
                                    bwselect = "cerrd")

prd_covariate_turnout <- rdrobust(y = nearest_neighbor_PRD_no_outliers$ref_next_turnout_pct, x = nearest_neighbor_PRD_no_outliers$PRD_margin, p = 1, 
                                covs = cbind(nearest_neighbor_PRD_no_outliers$main_year, nearest_neighbor_PRD_no_outliers$main_estado, 
                                             nearest_neighbor_PRD_no_outliers$dH, nearest_neighbor_PRD_no_outliers$treated_neighbors), 
                                bwselect = "cerrd", level = 90)

pan_non_covariate_turnout <- rdrobust(y = nearest_neighbor_PAN$ref_next_turnout_pct, x = nearest_neighbor_PAN$PAN_margin, p = 1, 
                                      bwselect = "cerrd")

pan_covariate_turnout <- rdrobust(y = nearest_neighbor_PAN$ref_next_turnout_pct, x = nearest_neighbor_PAN$PAN_margin, p = 1, 
                                  covs = cbind(nearest_neighbor_PAN$main_year, nearest_neighbor_PAN$main_estado, 
                                               nearest_neighbor_PAN$dH, nearest_neighbor_PAN$treated_neighbors), 
                                  bwselect = "cerrd", level = 90)

create_model_table(prd_non_covariate_turnout, prd_covariate_turnout, pan_non_covariate_turnout, pan_covariate_turnout)
