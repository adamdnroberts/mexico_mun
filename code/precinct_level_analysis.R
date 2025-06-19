# =============================================================================
# Mexico Municipal Analysis: PRD Electoral Effects
# =============================================================================

# Load Required Libraries ----
library(dplyr)
library(sf)
library(fixest)
library(rdrobust)

# =============================================================================
# 1. DATA LOADING AND PREPARATION
# =============================================================================

# Load spatial data and create municipality IDs ----
mex_sf <- read_sf("~/mexico_mun/raw/SECCION.shp")
mex_sf$mun_id <- paste0(sprintf("%02d", mex_sf$ENTIDAD), 
                        sprintf("%03d", mex_sf$MUNICIPIO))

# Load electoral data and standardize municipality IDs ----
all_states_final <- read.csv("~/mexico_mun/raw/all_states_final.csv")
all_states_final$mun_id <- sprintf("%05d", all_states_final$mun_code)

# Load PRD nearest neighbor data ----
load("C:/Users/adamd/Documents/mexico_mun/data/nearest_neighbor_PRD.Rdata")

# Load precinct distance data ----
precinct_distances_mexico <- read.csv("~/new_one_I_think.csv")

# =============================================================================
# 2. SAMPLE SELECTION AND FILTERING
# =============================================================================

# Calculate precincts per municipality by year (2013 and earlier) ----
precinct_counts <- all_states_final %>%
  filter(year <= 2013) %>%
  group_by(mun_id, year) %>%
  summarize(n_precincts = n(), .groups = "drop")

# Calculate variance in precinct counts over time ----
precinct_variance <- precinct_counts %>%
  group_by(mun_id) %>%
  summarize(variance = var(n_precincts), .groups = "drop")

# Determine RD bandwidth for PRD analysis ----
bw <- rdbwselect(y = nearest_neighbor_PRD$change_pct_PRD, 
                 x = nearest_neighbor_PRD$PRD_margin, 
                 p = 1, 
                 bwselect = "cerrd")

# Identify municipalities within bandwidth ----
nn_muns <- unique(nearest_neighbor_PRD$neighbor[abs(nearest_neighbor_PRD$PRD_margin) < bw$bws[1]])

# =============================================================================
# 3. MUNICIPALITY MATCHING AND VALIDATION
# =============================================================================

# Filter to years 2000 and earlier ----
precinct_counts_2000 <- subset(precinct_counts, year <= 2000)

# Count precincts in shapefile by municipality ----
shapefile_precinct_counts <- as.data.frame(mex_sf) %>%
  group_by(mun_id) %>%
  summarize(n_precincts_shapefile = n(), .groups = "drop")

# Match precinct counts between data sources ----
matched_precincts <- left_join(precinct_counts_2000, 
                               shapefile_precinct_counts, 
                               by = "mun_id") %>%
  filter(n_precincts == n_precincts_shapefile)  # Keep only perfect matches

# Keep municipalities with exactly 2 years of data ----
stable_municipalities <- matched_precincts %>%
  group_by(mun_id) %>%
  filter(n() == 2) %>%  # Exactly 2 years
  ungroup()

# Final municipality list for analysis ----
muns_to_use <- unique(stable_municipalities$mun_id)

# Create final analysis dataset ----
analysis_data <- subset(all_states_final, 
                        year <= 2000 & 
                          mun_id %in% muns_to_use & 
                          mun_id %in% nn_muns)

# =============================================================================
# 4. PRECINCT-LEVEL DISTANCE ANALYSIS
# =============================================================================

# Merge precinct distances with spatial data ----
precinct_with_distances <- merge(precinct_distances_mexico, 
                                 mex_sf, 
                                 by.x = "InputID", 
                                 by.y = "ID")

# Create standardized municipal seat IDs ----
precinct_with_distances$mun_seat_id <- sprintf("%05d", precinct_with_distances$TargetID)

# Find nearest municipality for each precinct (excluding own municipality) ----
nearest_municipality_by_precinct <- precinct_with_distances %>%
  filter(mun_id != mun_seat_id) %>%  # Exclude same municipality
  group_by(mun_id, SECCION) %>%
  arrange(Distance) %>%
  slice(1) %>%  # Keep closest municipality
  ungroup()

# =============================================================================
# 5. FINAL DATA MERGING AND TREATMENT VARIABLES
# =============================================================================

# Merge analysis data with nearest municipality information ----
merged_step1 <- left_join(analysis_data, 
                          nearest_municipality_by_precinct, 
                          by = c("mun_id", "precinct" = "SECCION"))

# Merge with PRD margin data ----
final_merged_data <- left_join(merged_step1, 
                               nearest_neighbor_PRD, 
                               by = c("mun_seat_id" = "mun_id"))

# Create treatment and time variables ----
final_merged_data <- final_merged_data %>%
  mutate(
    PRD_treat = ifelse(PRD_margin > 0, 1, 0),           # Treatment indicator
    post_treatment = ifelse(year.x > 1997, 1, 0),       # Post-treatment period
    mun_pair_id = paste0(mun_id, mun_seat_id)           # Municipality pair ID
  )

# =============================================================================
# 6. REGRESSION ANALYSIS
# =============================================================================

# Model 1: PRD vote share with municipality and year fixed effects ----
model_1 <- feols(share_PRD_valid_vote ~ PRD_treat * post_treatment + Distance | 
                   mun_id + mun_seat_id + year.x, 
                 cluster = "precinct", 
                 data = final_merged_data)

print("=== Model 1: PRD Vote Share (Municipality FE) ===")
etable(model_1)

# Model 2: PRD vote share with municipality pair fixed effects ----
model_2 <- feols(share_PRD_valid_vote ~ PRD_treat * post_treatment + Distance | 
                   mun_pair_id + year.x, 
                 cluster = "precinct", 
                 data = final_merged_data)

print("=== Model 2: PRD Vote Share (Municipality Pair FE) ===")
etable(model_2)

# Model 3: PRD registered voters ----
model_3 <- feols(share_PRD_registered_voters ~ PRD_treat * post_treatment + Distance | 
                   mun_id + mun_seat_id + year.x, 
                 cluster = "precinct", 
                 data = final_merged_data)

print("=== Model 3: PRD Registered Voters ===")
summary(model_3)

# Model 3: PRD registered voters ----
model_4 <- feols(turnout ~ PRD_treat * post_treatment + Distance | 
                   mun_id + mun_seat_id + year.x, 
                 cluster = "precinct", 
                 data = final_merged_data)

print("=== Model 4: Turnout ===")
summary(model_4)

# =============================================================================
# 7. SUMMARY STATISTICS AND DIAGNOSTICS
# =============================================================================

cat("\n=== Sample Summary ===\n")
cat("Total municipalities in analysis:", length(unique(final_merged_data$mun_id)), "\n")
cat("Total precincts:", nrow(final_merged_data), "\n")
cat("Bandwidth used:", round(bw$bws[1], 3), "\n")
cat("Treatment municipalities:", sum(final_merged_data$PRD_treat, na.rm = TRUE), "\n")
cat("Control municipalities:", sum(1 - final_merged_data$PRD_treat, na.rm = TRUE), "\n")

