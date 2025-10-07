library(ggplot2)
library(dplyr)
library(rdrobust)
library(xtable)

source("~/mexico_mun/code/functions/function_create_rd_table.R")

load("C:/Users/adamd/Documents/mexico_mun/data/nearest_neighbor_PRD.Rdata")
load("C:/Users/adamd/Documents/mexico_mun/data/nearest_neighbor_PAN.Rdata")

#PRD models
prd_non_covariate_model <- rdrobust(
  y = nearest_neighbor_PRD$change_pct_PRD,
  x = nearest_neighbor_PRD$PRD_margin,
  p = 1,
  bwselect = "cerrd"
)

prd_covariate_model <- rdrobust(
  y = nearest_neighbor_PRD$change_pct_PRD,
  x = nearest_neighbor_PRD$PRD_margin,
  p = 1,
  covs = cbind(
    nearest_neighbor_PRD$main_year,
    nearest_neighbor_PRD$main_estado,
    nearest_neighbor_PRD$dH,
    nearest_neighbor_PRD$treated_neighbors
  ),
  bwselect = "cerrd"
)

#PAN models
pan_non_covariate_model <- rdrobust(
  y = nearest_neighbor_PAN$change_pct_PAN,
  x = nearest_neighbor_PAN$PAN_margin,
  p = 1,
  bwselect = "cerrd"
)

pan_covariate_model <- rdrobust(
  y = nearest_neighbor_PAN$change_pct_PAN,
  x = nearest_neighbor_PAN$PAN_margin,
  p = 1,
  covs = cbind(
    nearest_neighbor_PAN$main_year,
    nearest_neighbor_PAN$main_estado,
    nearest_neighbor_PAN$dH,
    nearest_neighbor_PAN$treated_neighbors
  ),
  bwselect = "cerrd"
)

#CREATE THE TABLE
create_model_table(
  prd_non_covariate_model,
  prd_covariate_model,
  pan_non_covariate_model,
  pan_covariate_model,
  output_type = "latex"
)
