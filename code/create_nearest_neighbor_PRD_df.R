library(dplyr)

load("~/mexico_mun/data/rdd_PRD_subset.Rdata")

rdd_PRD_subset_filtered <- subset(rdd_PRD_subset, ref_PRD_wins == 0 & main_estado == ref_estado)

nearest_neighbor_PRD <- rdd_PRD_subset_filtered %>%
  group_by(mun_id) %>%
  slice_head(n = 1)

nearest_neighbor_PRD <- nearest_neighbor_PRD %>%
  mutate(change_pct_PRD = ref_next_PRD_pct - ref_PRD_pct,
         change_pct_PAN = ref_next_PAN_pct - ref_PAN_pct,
         change_pct_PRI = ref_next_PRI_pct - ref_PRI_pct)

nearest_neighbor_PRD$main_estado <- as.factor(nearest_neighbor_PRD$main_estado)

save(nearest_neighbor_PRD, file = "C:/Users/adamd/Documents/mexico_mun/data/nearest_neighbor_PRD.Rdata")