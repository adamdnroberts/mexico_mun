library(ggplot2)
library(dplyr)
library(viridis)
library(rdrobust)
library(xtable)

load("~/mexico_mun/data/rdd_distance_PRD.Rdata")

df_1_PRD <- df_rdd_PRD %>%
  group_by(mun_id) %>%
  slice_head(n = 1)

df_1_PRD <- df_1_PRD %>%
  mutate(change_pp = ref_next_PRD_pct - ref_PRD_pct)

df_1_PRD$main_estado <- as.factor(df_1_PRD$main_estado)
df_1_PRD$ref_estado <- as.factor(df_1_PRD$ref_estado)

#table PRD
msem_PRD <- rdrobust(y = df_1_PRD$change_pp, x = df_1_PRD$PRD_pct, p = 1, covs = cbind(df_1_PRD$main_year, df_1_PRD$main_estado, df_1_PRD$dH), bwselect = "mserd", level = 90)

cerm_PRD <- rdrobust(y = df_1_PRD$change_pp, x = df_1_PRD$PRD_pct, p = 1, covs = cbind(df_1_PRD$main_year, df_1_PRD$main_estado, df_1_PRD$dH), bwselect = "cerrd", level = 90)


plot_PRD <- subset(df_1_PRD, abs(df_1_PRD$PRD_pct) < cerm$bws[1])
#Plot
png(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/TYP Final Tables and Figures/images/PRD_nearest_rdplot.png", width = 6, height = 4, units = "in", res = 300)
rdplot(y = plot_PRD$change_pp, x = plot_PRD$PRD_pct, p=1,
       covs = cbind(plot_PRD$main_year, plot_PRD$main_estado, plot_PRD$dH),
       #subset = abs(plot_PRD$PRD_pct) < cerm$bws[1], 
       title = "", x.label = "PRD Vote Share, t", y.label = "Nearest Municipalitiy PRD vote share, t+1")
dev.off()

### PAN
load("~/mexico_mun/data/rdd_distance_PAN.Rdata")

df_1_PAN <- df_rdd_PAN %>%
  group_by(mun_id) %>%
  slice_head(n = 1)

df_1_PAN <- df_1_PAN %>%
  mutate(change_pp = ref_next_PAN_pct - ref_PAN_pct)

df_1_PAN$main_estado <- as.factor(df_1_PAN$main_estado)
df_1_PAN$ref_estado <- as.factor(df_1_PAN$ref_estado)

#table PAN
msem_PAN <- rdrobust(y = df_1_PAN$change_pp, x = df_1_PAN$PAN_pct, p = 1, covs = cbind(df_1_PAN$main_year, df_1_PAN$main_estado, df_1_PAN$dH), bwselect = "mserd", level = 90)

cer_PAN <- rdrobust(y = df_1_PAN$change_pp, x = df_1_PAN$PAN_pct, p = 1, 
                 covs = cbind(df_1_PAN$main_year, df_1_PAN$main_estado, df_1_PAN$dH), 
                 bwselect = "cerrd", level = 90)

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
create_model_table(msem_PRD, cerm_PRD, msem_PAN, cer_PAN)

plot_PAN <- subset(df_1_PAN, abs(df_1_PAN$PAN_pct) < cer_PAN$bws[1])
#Plot
png(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/TYP Final Tables and Figures/images/PAN_nearest_rdplot.png", width = 6, height = 4, units = "in", res = 300)
rdplot(y = plot_PAN$change_pp, x = plot_PAN$PAN_pct, p=3,
       covs = cbind(plot_PAN$main_year, plot_PAN$main_estado, plot_PAN$dH),
       title = "", x.label = "PAN Vote Share, t", y.label = "Nearest Municipalitiy PAN vote share, t+1")
dev.off()

# DO I NEED THIS?

##EXAMINE SUPPLY SIDE

#are they just dropping out in nearby places?
df_1_PRD$PRD_runs_next <- ifelse(df_1_PRD$ref_next_PRD_pct > -0.5, 1, 0)
summary(df_1_PRD$PRD_runs_next)

PRD_runs <- rdrobust(y = df_1_PRD$PRD_runs_next, x = df_1_PRD$PRD_pct, p = 3, covs = cbind(df_1_PRD$main_year, df_1_PRD$main_estado), bwselect = "cerrd", level = 90)
summary(PRD_runs)

PRD_runs <- rdrobust(y = df_1_PRD$PRD_runs_next, x = df_1_PRD$PRD_pct, p = 2, covs = cbind(df_1_PRD$main_year, df_1_PRD$main_estado,df_1_PRD$dH), bwselect = "cerrd", level= 90)
summary(PRD_runs)

PRD_runs <- rdrobust(y = df_1_PRD$PRD_runs_next, x = df_1_PRD$PRD_pct, p = 1, covs = cbind(df_1_PRD$main_year, df_1_PRD$main_estado,df_1_PRD$dH), bwselect = "cerrd", level= 90)
summary(PRD_runs)

#drop places where PRD doesn't run (PRD Always Runs)
PAR <- subset(df_1_PRD, PRD_runs_next == 1)

PAR_m <- rdrobust(y = PAR$change_pp, x = PAR$PRD_pct, p = 3, covs = cbind(PAR$main_year, PAR$main_estado), bwselect = "cerrd", level = 90)
summary(PAR_m)

load("~/mexico_mun/data/rdd_distance_PRD.Rdata")

df_1_PRD <- df_rdd_PRD %>%
  group_by(mun_id) %>%
  slice_head(n = 1)

df_1_PRD <- df_1_PRD %>%
  mutate(change_pp = ref_next_PRD_pct - ref_PRD_pct)

df_1_PRD$main_estado <- as.factor(df_1_PRD$main_estado)
df_1_PRD$ref_estado <- as.factor(df_1_PRD$ref_estado)

#table PRD
msem <- rdrobust(y = df_1_PRD_PAN$change_pp, x = df_1_PRD_PAN$PRD_pct, p = 1, covs = cbind(df_1_PRD_PAN$main_year, df_1_PRD_PAN$main_estado, df_1_PRD_PAN$dH), bwselect = "mserd", level = 90)

cerm <- rdrobust(y = df_1_PRD_PAN$change_pp, x = df_1_PRD_PAN$PRD_pct, p = 1, covs = cbind(df_1_PRD_PAN$main_year, df_1_PRD_PAN$main_estado, df_1_PRD_PAN$dH), bwselect = "cerrd", level = 90)
