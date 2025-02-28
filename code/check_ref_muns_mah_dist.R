library(dplyr)
library(GGally)

load("~/mexico_mun/data/rdd_mah_dist.Rdata")

ref1 <- subset(df_rdd, mun_id == "19009" & main_estado == ref_estado & ref_PAN_wins == 0 & ref_next_PAN_pct > -0.5)

ref1_sorted <- ref1 %>%
  arrange(mun_id, desc(mah_d))

df1 <- ref1_sorted %>%
  group_by(mun_id) %>%
  slice_head(n = 5)

refs <- unique(df1$ref_mun_id)

test <- subset(mah_dist_full, mun_id %in% refs)

ref1_values <- subset(mah_dist_full, mun_id == "19009")

# List of variables to calculate the difference for
variables <- c("prev_PAN_pct", "pop", 
               #"income", 
               "LAT_DECIMAL", "LON_DECIMAL", 
               #"depMR_PAN_pct", "depPR_PAN_pct", "senate_PAN_pct", 
               #"pres_PAN_pct",
               #"transfers_pct",
               "pop_rural"
               )

# Initialize an empty data frame to store the differences
diff_data <- data.frame(matrix(ncol = length(variables), nrow = nrow(test)))
colnames(diff_data) <- paste0(variables, "_diff")

#diff_data$mun_id <- test$mun_id
# Perform the difference calculation for each variable and add to the new data frame
for (var in variables) {
  diff_col_name <- paste0(var, "_diff")
  diff_data[[diff_col_name]] <- test[[var]] - ref1_values[[var]][1]
}

# Print the new data frame with the differences
ggpairs(diff_data)
