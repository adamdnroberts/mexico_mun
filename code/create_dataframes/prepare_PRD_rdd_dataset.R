library(dplyr)
library(data.table)  # For faster operations

# Load data
load("~/mexico_mun/data/pairwise_km.Rdata")
load("~/mexico_mun/data/mexico_municipal_elections.Rdata")

# Create mun_id once and convert to data.table for faster operations
setDT(mexico_municipal_elections)

# Create datasets more efficiently using conditions within data.table
df <- mexico_municipal_elections[year >= 1995 & year <= 1997 & 
                                   (p1_name == "PRI" | p2_name == "PRI")]
df_ref <- mexico_municipal_elections[year >= 1995 & year <= 1997]

# Identify treated municipalities more efficiently
treated_muns <- mexico_municipal_elections[year <= 1997 & PRD_treat == 1, 
                                           unique(mun_id)]
treated_muns_before <- mexico_municipal_elections[year <= 1994 & PRD_treat == 1, 
                                                  unique(mun_id)]

# Define column selections once
ref_cols <- c("year", "mun_id", "next_PRD_pct", "PRD_pct", "next_PAN_pct", 
              "PAN_pct", "estado", "PRD_margin", "next_PRD_margin", 
              "PRI_pct", "next_PRI_pct", "next_turnout_pct")

main_cols <- c("year", "mun_id", "PRD_pct", "PRD_margin", "next_PRD_margin", 
               "PAN_pct", "estado")

# Create filtered datasets with proper exclusions
ref_PRD_not_treated <- df_ref[!mun_id %in% treated_muns, ..ref_cols]
main_mun_PRD_not_treated <- df[!mun_id %in% treated_muns_before, ..main_cols]

# Save intermediate data
save(main_mun_PRD_not_treated, 
     file = "C:/Users/adamd/Documents/mexico_mun/data/PRD_untreated.Rdata")

# Perform merge operations more efficiently
# First merge: neighbors with reference data
ref_merged <- merge(dH_df, ref_PRD_not_treated, 
                    by.x = "neighbor", by.y = "mun_id", 
                    all.x = FALSE)  # Inner join for efficiency

# Rename columns in bulk using setnames for data.table
old_names <- c("PRD_pct", "next_PRD_pct", "PAN_pct", "next_PAN_pct", 
               "estado", "year", "PRD_margin", "next_PRD_margin", 
               "PRI_pct", "next_PRI_pct", "next_turnout_pct")

new_names <- c("ref_PRD_pct", "ref_next_PRD_pct", "ref_PAN_pct", "ref_next_PAN_pct",
               "ref_estado", "ref_year", "ref_PRD_margin", "ref_next_PRD_margin",
               "ref_PRI_pct", "ref_next_PRI_pct", "ref_next_turnout_pct")

setnames(ref_merged, old_names, new_names)

# Create treatment indicator more efficiently
ref_merged[, ref_PRD_wins := as.integer(ref_PRD_margin > 0)]

# Final merge and variable creation
rdd_PRD_subset <- merge(main_mun_PRD_not_treated, ref_merged, 
                by.x = "mun_id", by.y = "mun",
                all.x = FALSE)

# Rename and create derived variables
rdd_PRD_subset[, `:=`(
  main_year = year,
  main_estado = estado,
  weight = 1/dH
)]

# Sort using data.table syntax (much faster)
setorder(rdd_PRD_subset, mun_id, dH)

# Save final dataset
save(rdd_PRD_subset, file = "C:/Users/adamd/Documents/mexico_mun/data/rdd_PRD_subset.Rdata")

# OPTIONAL: Clean up intermediate objects to free memory
rm(ref_merged, ref_PRD_not_treated, df, df_ref)
gc()  # Garbage collection
