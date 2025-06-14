library(dplyr)
library(rdrobust)
library(xtable)

#PRD

load("~/mexico_mun/data/nearest_neighbor_PRD.Rdata")

load("~/mexico_mun/data/mexpop.Rdata")
pop_full <- subset(mun_cen_final, year >= 1995 & year <= 1997)
pop <- subset(pop_full, select = c(mun_id, year, pop))
pop <- pop %>% mutate(pop = gsub(",", "", pop))
pop$pop <- as.numeric(pop$pop)
pop$main_year <- as.numeric(pop$year)

load("C:/Users/adamd/Documents/sismos_y_media/data/full_dataset_efipem.Rdata")

bud <- subset(wide_budget_df, ANIO <= 1997 & ANIO >=1995)

budget <- subset(bud, select = c(mun_id, ANIO, Ingresos.Tema.Total.de.ingresos))
colnames(budget) <- c("mun_id","main_year", "income")
budget$income_m <- budget$income/1000000

merge1 <- merge(nearest_neighbor_PRD, pop, by = c("mun_id","main_year"))
merge2 <- merge(merge1, budget, by = c("mun_id","main_year"), all.x = T)

population <- rdrobust(y = merge2$pop, x =merge2$PRD_margin, p = 1, bwselect = "cerrd", level = 90)
income <- rdrobust(y = merge2$income_m, x =merge2$PRD_margin, p = 1, bwselect = "cerrd", level = 90)

distance <- rdrobust(y = nearest_neighbor_PRD$dH, x = nearest_neighbor_PRD$PRD_margin, p = 1, bwselect = "cerrd", level = 90)

nearest_neighbor_PRD$y1995 <- ifelse(nearest_neighbor_PRD$main_year == 1995, 1, 0)
y95 <- rdrobust(y = nearest_neighbor_PRD$y1995, x = nearest_neighbor_PRD$PRD_margin, p = 1, bwselect = "cerrd", level = 90)

nearest_neighbor_PRD$y1996 <- ifelse(nearest_neighbor_PRD$main_year == 1996, 1, 0)
y96 <- rdrobust(y = nearest_neighbor_PRD$y1996, x = nearest_neighbor_PRD$PRD_margin, p = 1, bwselect = "cerrd", level = 90)

nearest_neighbor_PRD$y1997 <- ifelse(nearest_neighbor_PRD$main_year == 1997, 1, 0)
y97 <- rdrobust(y = nearest_neighbor_PRD$y1997, x = nearest_neighbor_PRD$PRD_margin, p = 1, bwselect = "cerrd", level = 90)

#LOOP
nearest_neighbor_PRD$es_num <- as.numeric(nearest_neighbor_PRD$main_estado)
model_values <- list()

# Loop through values from 1 to 29
for (i in 1:29) {
  print(i)
  
  # Create a new column based on the current loop value
  nearest_neighbor_PRD[[paste0("es_", i)]] <- ifelse(nearest_neighbor_PRD$es_num == i, 1, 0)
  
  state <- as.character(unique(nearest_neighbor_PRD$main_estado)[i])
  coef_value <- NA
  se_value <- NA
  pv_value <- NA
  N_sum <- NA
  
  if (sum(nearest_neighbor_PRD[[paste0("es_", i)]][nearest_neighbor_PRD$PRD_margin > 0], na.rm = T) == 0) {
    model_values[[i]] <- c(state, coef_value, se_value, pv_value, N_sum)
  } else {
    
    # Run the rdrobust function with the new column
    model <- rdrobust(y = nearest_neighbor_PRD[[paste0("es_", i)]], x = nearest_neighbor_PRD$PRD_margin, p = 1, bwselect = "cerrd", level = 90)
    
    state <- as.character(unique(nearest_neighbor_PRD$main_estado)[i])
    coef_value <- round(model$coef[3], 3)
    se_value <- round(model$se[3], 3)
    pv_value <- round(model$pv[3], 3)
    N_sum <- sum(model$N)
    
    # Print the summary of the test
    model_values[[i]] <- c(state, coef_value, se_value, pv_value, N_sum)
  }
  
}

models <- list(distance, y95, y96, y97, population, income )
i = 29

get_object_name <- function(obj) {
  deparse(substitute(obj))
}

for (model in models) {
  i = i+1
  
  model_name <- get_object_name(model)
  
  state <- model_name
  coef_value <- round(model$coef[3], 3)
  se_value <- round(model$se[3], 3)
  pv_value <- round(model$pv[3], 3)
  N_sum <- sum(model$N)
  
  model_values[[i]] <- c(state, coef_value, se_value, pv_value, N_sum)
}

# Combine the values into a data frame
result_table <- data.frame(
  do.call(rbind, t(model_values)))

colnames(result_table) <- c("Covariate","Est.", "SE","p-value", "N")


latex_table <- xtable(result_table, caption = "Model Results", label = "tab:model_results")
print(latex_table, include.rownames = FALSE)


#PAN

load("~/mexico_mun/data/nearest_neighbor_PAN.Rdata")

load("~/mexico_mun/data/mexpop.Rdata")
pop_full <- subset(mun_cen_final, year >= 1995 & year <= 1997)
pop <- subset(pop_full, select = c(mun_id, year, pop))
pop <- pop %>% mutate(pop = gsub(",", "", pop))
pop$pop <- as.numeric(pop$pop)
pop$main_year <- as.numeric(pop$year)

load("~/mexico_mun/data/full_dataset_mexbudget.Rdata")

bud <- subset(wide_budget_df, ANIO <= 1997 & ANIO >=1995)

budget <- subset(bud, select = c(mun_id, ANIO, Ingresos.Tema.Total.de.ingresos))
colnames(budget) <- c("mun_id","main_year", "income")
budget$income_m <- budget$income/1000000

merge1 <- merge(nearest_neighbor_PAN, pop, by = c("mun_id","main_year"))
merge2 <- merge(merge1, budget, by = c("mun_id","main_year"), all.x = T)

population <- rdrobust(y = merge2$pop, x =merge2$PAN_margin, p = 1, bwselect = "cerrd", level = 90)
income <- rdrobust(y = merge2$income_m, x =merge2$PAN_margin, p = 1, bwselect = "cerrd", level = 90)

distance <- rdrobust(y = nearest_neighbor_PAN$dH, x = nearest_neighbor_PAN$PAN_margin, p = 1, bwselect = "cerrd", level = 90)

nearest_neighbor_PAN$y1995 <- ifelse(nearest_neighbor_PAN$main_year == 1995, 1, 0)
y95 <- rdrobust(y = nearest_neighbor_PAN$y1995, x = nearest_neighbor_PAN$PAN_margin, p = 1, bwselect = "cerrd", level = 90)

nearest_neighbor_PAN$y1996 <- ifelse(nearest_neighbor_PAN$main_year == 1996, 1, 0)
y96 <- rdrobust(y = nearest_neighbor_PAN$y1996, x = nearest_neighbor_PAN$PAN_margin, p = 1, bwselect = "cerrd", level = 90)

nearest_neighbor_PAN$y1997 <- ifelse(nearest_neighbor_PAN$main_year == 1997, 1, 0)
y97 <- rdrobust(y = nearest_neighbor_PAN$y1997, x = nearest_neighbor_PAN$PAN_margin, p = 1, bwselect = "cerrd", level = 90)

#LOOP
nearest_neighbor_PAN$es_num <- as.numeric(nearest_neighbor_PAN$main_estado)
model_values <- list()

# Loop through values from 1 to 29
for (i in 1:29) {
  print(i)
  
  # Create a new column based on the current loop value
  nearest_neighbor_PAN[[paste0("es_", i)]] <- ifelse(nearest_neighbor_PAN$es_num == i, 1, 0)
  
  state <- as.character(unique(nearest_neighbor_PAN$main_estado)[i])
  coef_value <- NA
  se_value <- NA
  pv_value <- NA
  N_sum <- NA
  
  if (sum(nearest_neighbor_PAN[[paste0("es_", i)]][nearest_neighbor_PAN$PAN_margin > 0], na.rm = T) == 0) {
    model_values[[i]] <- c(state, coef_value, se_value, pv_value, N_sum)
  } else {
    
    # Run the rdrobust function with the new column
    model <- rdrobust(y = nearest_neighbor_PAN[[paste0("es_", i)]], x = nearest_neighbor_PAN$PAN_margin, p = 1, bwselect = "cerrd", level = 90)
    
    state <- as.character(unique(nearest_neighbor_PAN$main_estado)[i])
    coef_value <- round(model$coef[3], 3)
    se_value <- round(model$se[3], 3)
    pv_value <- round(model$pv[3], 3)
    N_sum <- sum(model$N)
    
    # Print the summary of the test
    model_values[[i]] <- c(state, coef_value, se_value, pv_value, N_sum)
  }
  
}

models <- list(distance, y95, y96, y97, population, income )
i = 29

get_object_name <- function(obj) {
  deparse(substitute(obj))
}

for (model in models) {
  i = i+1
  
  model_name <- get_object_name(model)
  
  state <- model_name
  coef_value <- round(model$coef[3], 3)
  se_value <- round(model$se[3], 3)
  pv_value <- round(model$pv[3], 3)
  N_sum <- sum(model$N)
  
  model_values[[i]] <- c(state, coef_value, se_value, pv_value, N_sum)
}

# Combine the values into a data frame
result_table <- data.frame(
  do.call(rbind, t(model_values)))

colnames(result_table) <- c("Covariate","Est.", "SE","p-value", "N")


latex_table <- xtable(result_table, caption = "Model Results", label = "tab:model_results")
print(latex_table, include.rownames = FALSE)
