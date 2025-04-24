library(rdrobust)

load("~/mexico_mun/data/PRD_not_treated.Rdata")
source("~/mexico_mun/code/function_create_rd_table.R") #for function create_model_table


main_mun_PRD_not_treated$next_PRD_win <- ifelse(main_mun_PRD_not_treated$next_PRD_margin > 0, 1, 0)

cerm_PRD <- rdrobust(y = main_mun_PRD_not_treated$next_PRD_win, x = main_mun_PRD_not_treated$PRD_margin, p = 1, 
                     covs = cbind(main_mun_PRD_not_treated$main_year, main_mun_PRD_not_treated$main_estado, main_mun_PRD_not_treated$dH), 
                     bwselect = "cerrd", level = 90)

create_model_table(cerm_PRD)
