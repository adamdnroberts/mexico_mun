load("~/mexico_mun/data/rdd_distance_PRD.Rdata")

df_1_PRD <- df_rdd_PRD %>%
  group_by(mun_id) %>%
  slice_head(n = 1)

df_1_PRD <- df_1_PRD %>%
  mutate(change_pp = ref_next_PRD_pct - ref_PRD_pct)

df_1_PRD$main_estado <- as.factor(df_1_PRD$main_estado)
df_1_PRD$ref_estado <- as.factor(df_1_PRD$ref_estado)

#table PRD
test <- rdrobust(y = df_1_PRD$dH, x = df_1_PRD$PRD_pct, p = 1, bwselect = "cerrd", level = 90)
summary(test)

df_1_PRD$y1995 <- ifelse(df_1_PRD$main_year == 1995, 1, 0)
test <- rdrobust(y = df_1_PRD$y1995, x = df_1_PRD$PRD_pct, p = 1, bwselect = "cerrd", level = 90)
summary(test)

df_1_PRD$y1995 <- ifelse(df_1_PRD$main_year == 1995, 1, 0)
test <- rdrobust(y = df_1_PRD$y1995, x = df_1_PRD$PRD_pct, p = 1, bwselect = "cerrd", level = 90)
summary(test)

df_1_PRD$y1996 <- ifelse(df_1_PRD$main_year == 1996, 1, 0)
test <- rdrobust(y = df_1_PRD$y1996, x = df_1_PRD$PRD_pct, p = 1, bwselect = "cerrd", level = 90)
summary(test)

df_1_PRD$y1997 <- ifelse(df_1_PRD$main_year == 1997, 1, 0)
test <- rdrobust(y = df_1_PRD$y1997, x = df_1_PRD$PRD_pct, p = 1, bwselect = "cerrd", level = 90)
summary(test)

df_1_PRD$es_num <- as.numeric(df_1_PRD$main_estado)

for (i in 1:length(unique(df_1_PRD$main_estado))) {
  print(as.character(unique(df_1_PRD$main_estado)[i]))
}

df_1_PRD$es_4 <- ifelse(df_1_PRD$es_num == 4, 1, 0)
if (sum(df_1_PRD$es_4[df_1_PRD$PRD_pct > 0], na.rm = T) == 0 | sum(df_1_PRD$es_4[df_1_PRD$PRD_pct < 0], na.rm = T) == 0) {
  print("uh oh")
}
test <- rdrobust(y = df_1_PRD$es_4, x = df_1_PRD$PRD_pct, p = 1, bwselect = "cerrd", level = 90)
summary(test)

# Loop through values from 1 to 29
for (i in 1:5) {
  print(i)
  model_values <- list()
  
  # Create a new column based on the current loop value
  df_1_PRD[[paste0("es_", i)]] <- ifelse(df_1_PRD$es_num == i, 1, 0)
  
  if (sum(df_1_PRD[[paste0("es_", i)]][df_1_PRD$PRD_pct > 0], na.rm = T) == 0) {
    print("uh oh")
  } else {
  
  # Run the rdrobust function with the new column
  model <- rdrobust(y = df_1_PRD[[paste0("es_", i)]], x = df_1_PRD$PRD_pct, p = 1, bwselect = "cerrd", level = 90)
  
  state <- as.character(unique(df_1_PRD$main_estado)[i])
  coef_value <- round(model$coef[3], 3)
  se_value <- round(model$se[3], 3)
  pv_value <- round(model$pv[3], 3)
  N_sum <- sum(model$N)

  # Print the summary of the test
  model_values[[length(model_values) + 1]] <- c(state, coef_value, se_value, pv_value, N_sum)
  }
  
}

# Combine the values into a data frame
result_table <- data.frame(
  do.call(rbind, t(model_values)))

colnames(result_table) <- c("Covariate","Est.", "SE","p Value", "N")
