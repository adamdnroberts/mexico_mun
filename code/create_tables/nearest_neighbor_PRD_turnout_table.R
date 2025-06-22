library(dplyr)
library(sf)
library(fixest)
library(rdrobust)


# Load electoral data and standardize municipality IDs ----
all_states_final <- read.csv("~/mexico_mun/raw/all_states_final.csv")
all_states_final$mun_id <- sprintf("%05d", all_states_final$mun_code)

# Load PRD nearest neighbor data ----
load("C:/Users/adamd/Documents/mexico_mun/data/merged_step1.Rdata")

mun_summary <- all_states_final %>%
  group_by(mun_id,year) %>%
  summarize(total_votes = sum(total), 
            total_valid_votes = sum(valid), 
            registered_voters = sum(registered_voters))

mun_summary2 <- mun_summary %>%
  arrange(year) %>%
  group_by(mun_id) %>%
  mutate(lead_total_votes = lead(total_votes, 1), 
         lead_valid_votes = lead(total_valid_votes, 1), 
         lead_registered_voters = lead(registered_voters))

# Merge analysis data with nearest municipality information ----
merged_step1 <- left_join(merged_step1, 
                          mun_summary2, 
                          by = c("neighbor" = "mun_id", "year"))

merged_step1$turnout_t1 <- merged_step1$total_votes/merged_step1$registered_voters
merged_step1$turnout_t2 <- merged_step1$lead_total_votes/merged_step1$lead_registered_voters
merged_step1$change_in_turnout <- merged_step1$turnout_t1 - merged_step1$turnout_t2

merged_step1$valid_turnout_t1 <- merged_step1$total_valid_votes/merged_step1$registered_voters
merged_step1$valid_turnout_t2 <- merged_step1$lead_valid_votes/merged_step1$lead_registered_voters
merged_step1$change_in_valid_turnout <- merged_step1$valid_turnout_t1 - merged_step1$valid_turnout_t2

#PRD models
prd_non_covariate_turnout <- rdrobust(y = merged_step1$change_in_turnout, x = merged_step1$PRD_margin, p = 1, 
                                      bwselect = "cerrd")

prd_covariate_turnout <- rdrobust(y = merged_step1$change_in_turnout, x = merged_step1$PRD_margin, p = 1, 
                                  covs = cbind(merged_step1$main_year, merged_step1$main_estado, 
                                               merged_step1$dH, merged_step1$treated_neighbors), 
                                  bwselect = "cerrd")

prd_non_covariate_valid_turnout <- rdrobust(y = merged_step1$change_in_valid_turnout, x = merged_step1$PRD_margin, p = 1, 
                                      bwselect = "cerrd")

prd_covariate_valid_turnout <- rdrobust(y = merged_step1$change_in_valid_turnout, x = merged_step1$PRD_margin, p = 1, 
                                  covs = cbind(merged_step1$main_year, merged_step1$main_estado, 
                                               merged_step1$dH, merged_step1$treated_neighbors), 
                                  bwselect = "cerrd")

source("~/mexico_mun/code/functions/function_create_rd_table.R")

create_model_table(prd_non_covariate_turnout, 
                   prd_covariate_turnout, 
                   prd_non_covariate_valid_turnout,
                   prd_covariate_valid_turnout,
                   output_type = "text")
