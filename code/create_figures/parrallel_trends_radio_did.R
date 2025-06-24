library(dplyr)
library(data.table)  # For faster operations
library(fixest)
library(ggplot2)
library(stringr)

# Load data
load("~/mexico_mun/data/pairwise_km.Rdata")
load("~/mexico_mun/data/mexico_municipal_elections.Rdata")

# Create mun_id once and convert to data.table for faster operations
setDT(mexico_municipal_elections)

# Create datasets more efficiently using conditions within data.table
df <- mexico_municipal_elections[year <= 1997 & 
                                   (p1_name == "PRI" | p2_name == "PRI")]
df_ref <- mexico_municipal_elections[year <= 1997]

# Identify treated municipalities more efficiently
treated_muns <- mexico_municipal_elections[year <= 1997 & PRD_treat == 1, 
                                           unique(mun_id)]
treated_muns_before <- mexico_municipal_elections[year <= 1994 & PRD_treat == 1, 
                                                  unique(mun_id)]
treated_muns_did <- mexico_municipal_elections[year >= 1995 & year <= 1997 & PRD_treat == 1, 
                                           unique(mun_id)]

# Define column selections once
ref_cols <- c("year", "mun_id", "next_PRD_pct", "PRD_pct", "next_PAN_pct", 
              "PAN_pct", "estado", "PRD_margin", "next_PRD_margin", 
              "PRI_pct", "next_PRI_pct", "next_turnout_pct")

main_cols <- c("year", "mun_id", "PRD_pct", "PRD_margin", "next_PRD_margin", 
               "PAN_pct", "estado", "PRD_treat")

# Create filtered datasets with proper exclusions
ref_PRD_not_treated <- df_ref[!mun_id %in% treated_muns, ..ref_cols]
main_mun_PRD_not_treated <- df[!mun_id %in% treated_muns_before, ..main_cols]

# Perform merge operations more efficiently
# First merge: neighbors with reference data
ref_merged <- left_join(ref_PRD_not_treated, dH_df, by = c("mun_id" = "neighbor"), relationship = "many-to-many")

# Rename columns in bulk using setnames for data.table
old_names <- c("mun_id", "PRD_pct", "next_PRD_pct", "PAN_pct", "next_PAN_pct", 
               "estado", "year", "PRD_margin", "next_PRD_margin", 
               "PRI_pct", "next_PRI_pct", "next_turnout_pct")

new_names <- c("ref_mun_id", "ref_PRD_pct", "ref_next_PRD_pct", "ref_PAN_pct", "ref_next_PAN_pct",
               "ref_estado", "ref_year", "ref_PRD_margin", "ref_next_PRD_margin",
               "ref_PRI_pct", "ref_next_PRI_pct", "ref_next_turnout_pct")

setnames(ref_merged, old_names, new_names)

# Final merge and variable creation
rdd_PRD_subset <- left_join(main_mun_PRD_not_treated, ref_merged, 
                            by = join_by("mun_id" == "mun", "year" == "ref_year"))

# Sort using data.table syntax (much faster)
setorder(rdd_PRD_subset, mun_id, dH)

new0 <- subset(rdd_PRD_subset, estado == ref_estado)

load("~/mexico_mun/data/jaccard_similarity_AM_FM.Rdata")

new <- merge(new0, js, by = c("mun_id", "ref_mun_id"))

new$change_pct_PRD <- new$ref_PRD_pct - new$ref_next_PRD_pct
new$PRD_treat <- ifelse(new$mun_id %in% treated_muns_did, 1, 0)

result <- t.test(share_PRD_valid_vote ~ PRD_treat, data = subset(new, year <= ))
result

ggplot(new) +
  geom_smooth(aes(x = year, y = change_pct_PRD)) +
  facet_wrap(~PRD_treat)

new$t <- NA
new$t[new$year == 1985] <- -3
new$t[new$year >= 1986 & new$year <= 1988] <- -2
new$t[new$year >= 1989 & new$year <= 1991] <- -1
new$t[new$year >= 1992 & new$year <= 1994] <- 0
new$t[new$year >= 1995 & new$year <= 1997] <- 1

#no outcome audit
m1 <- feols(change_pct_PRD ~ PRD_treat*js*as.factor(t) + dH #+ PRD_treat*js^2 
            | mun_id + year,
            cluster = "ref_mun_id",
            data = new)
etable(m1, digits = "r3")

#plot
plot_data <- as.data.frame(cbind(m1$coefficients, m1$se))
colnames(plot_data) <- c("est","se")
plot_data$vars <- row.names(plot_data)

#CIs
plot_data$cihigh <- plot_data$est + 1.96*plot_data$se
plot_data$cilow <- plot_data$est - 1.96*plot_data$se

# Filter the data frame
plot_data <- plot_data %>%
  filter(vars %in% c("PRD_treat:js:as.factor(t)-2",
                     "PRD_treat:js:as.factor(t)-1",
                     "PRD_treat:js:as.factor(t)0",
                     "PRD_treat:js:as.factor(t)1")) %>%
  mutate(t = as.numeric(str_extract(vars, "(?<=\\(t\\))-?\\d+")))

p <- ggplot(plot_data, aes(x = t, y = est)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_errorbar(aes(ymin = cilow, ymax = cihigh), width = 0) +
  geom_point(position = position_dodge(width = -0.5)) +
  labs(x = "", y = "Coefficient and 95% CI", title = "Model Coefficients") +
  theme_minimal()
print(p)

linearHypothesis(m1, "js = PRD_treat:js")
