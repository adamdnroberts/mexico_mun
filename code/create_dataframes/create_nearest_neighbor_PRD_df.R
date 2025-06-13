# Load required libraries
library(dplyr)

# Load datasets
load("~/mexico_mun/data/rdd_PRD_subset.Rdata")
load("~/mexico_mun/data/pairwise_km.Rdata")
load("~/mexico_mun/data/mexico_municipal_elections.Rdata")

# Create nearest neighbor dataset with change variables
nearest_neighbor_PRD <- rdd_PRD_subset %>%
  filter(ref_PRD_wins == 0, main_estado == ref_estado) %>%
  group_by(mun_id) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(
    change_pct_PRD = ref_next_PRD_pct - ref_PRD_pct,
    change_pct_PAN = ref_next_PAN_pct - ref_PAN_pct,
    change_pct_PRI = ref_next_PRI_pct - ref_PRI_pct,
    main_estado = as.factor(main_estado)
  )

# Create treated neighbors index
# Step 1: Extract distances to nearest untreated municipalities
nearest_untreated_distances <- nearest_neighbor_PRD %>%
  select(mun_id, nearest_untreated_distance = dH)

# Step 2: Get all pairwise distances and filter for those closer than nearest untreated
pairs_closer_than_untreated <- dH_df %>%
  left_join(nearest_untreated_distances, join_by(mun == mun_id), relationship = "many-to-many") %>%
  filter(dH < nearest_untreated_distance)

# Step 3: Identify treated municipalities (clean municipality IDs first)
treated_municipalities <- mexico_municipal_elections %>%
  filter(year <= 1997, PRD_treat == 1) %>%
  pull(mun_id) %>%
  unique()

# Step 4: Count treated neighbors for each municipality
treated_neighbor_counts <- pairs_closer_than_untreated %>%
  filter(mun %in% treated_municipalities) %>%
  group_by(mun) %>%
  summarise(treated_neighbors = n(), .groups = "drop")

# Step 5: Add treated neighbor counts to main dataset
nearest_neighbor_PRD <- nearest_neighbor_PRD %>%
  left_join(treated_neighbor_counts, join_by(mun_id == mun)) %>%
  mutate(treated_neighbors = coalesce(treated_neighbors, 0))

# save the final dataset
save(nearest_neighbor_PRD, file = "C:/Users/adamd/Documents/mexico_mun/data/nearest_neighbor_PRD.Rdata")

