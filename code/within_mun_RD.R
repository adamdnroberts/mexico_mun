library(rdrobust)

load("~/mexico_mun/data/full_dataset_mexelec_pcts.Rdata")

#PAN

treated_df_before <- subset(big_df, year <= 1994 & PAN_treat == 1)
treated_before <- unique(treated_df_before$mun_id)

df <- subset(big_df, !(mun_id %in% treated_before) & year >=1995 & year <= 1997)
df$estado <- as.factor(df$estado)
df$year <- as.factor(df$year)
df <- df %>%
  mutate(change_pp = next_PAN_pct - PAN_pct)

PAN_within <- rdrobust(y = df$change_pp, x = df$PAN_pct, 
                       covs = cbind(df$estado, df$year), 
                       p = 1, bwselect = "cerrd", level = 90)
summary(PAN_within)

#PRD 

treated_df_before <- subset(big_df, year <= 1994 & PRD_treat == 1)
treated_before <- unique(treated_df_before$mun_id)

df <- subset(big_df, !(mun_id %in% treated_before) & year >=1995 & year <= 1997)
df$estado <- as.factor(df$estado)
df$year <- as.factor(df$year)
df <- df %>%
  mutate(change_pp = next_PRD_pct -PRD_pct)

#what's going on
PRD_within <- rdrobust(y = df$change_pp, x = df$PRD_pct, 
                       covs = cbind(df$estado, df$year), 
                       p = 1, bwselect = "cerrd", level = 90)
summary(PRD_within)

create_model_table <- function(..., metrics = c("Party", "Coefficient", "Standard Error", "p Value", "BW Type", "Bandwidth", "N", "Effective N")) {
  models <- list(...)
  
  # Initialize an empty list to store the values for each model
  model_values <- list()
  
  for (model in models) {
    coef_value <- round(model$coef[3], 3)
    se_value <- round(model$se[3], 3)
    pv_value <- round(model$pv[3], 3)
    bw_type <- ifelse(model$bwselect=="mserd","MSE","CER")
    bws_value <- round(model$bws[1], 3)
    N_sum <- sum(model$N)
    N_b_sum <- sum(model$N_h)
    party <- ifelse(pv_value < 0.1, "PRD","PAN")
    
    model_values[[length(model_values) + 1]] <- c(party, coef_value, se_value, pv_value, bw_type, bws_value, N_sum, N_b_sum)
  }
  
  # Combine the values into a data frame
  result_table <- data.frame(
    Metric = metrics,
    do.call(cbind, model_values)
  )
  
  # Convert the data frame to a LaTeX table using xtable
  latex_table <- xtable(result_table, include.rownames = FALSE)
  
  # Print the LaTeX table
  print(latex_table, type = "latex")
}

# Example usage
create_model_table(PRD_within, PAN_within)
