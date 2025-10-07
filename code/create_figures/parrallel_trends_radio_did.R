library(dplyr)
library(data.table)
library(fixest)
library(ggplot2)
library(stringr)

# Load data
load("~/mexico_mun/data/pairwise_km.Rdata")
load("~/mexico_mun/data/mexico_municipal_elections.Rdata")

# Create mun_id once and convert to data.table for faster operations
setDT(mexico_municipal_elections)

# Create datasets more efficiently using conditions within data.table
df <- mexico_municipal_elections[
  year <= 1997 &
    (p1_name == "PRI" | p2_name == "PRI")
]
df_ref <- mexico_municipal_elections[year <= 1997]

# Identify treated municipalities more efficiently
treated_muns <- mexico_municipal_elections[
  year <= 1997 & PRD_treat == 1,
  unique(mun_id)
]
treated_muns_before <- mexico_municipal_elections[
  year <= 1994 & PRD_treat == 1,
  unique(mun_id)
]
treated_muns_did <- mexico_municipal_elections[
  year >= 1995 & year <= 1997 & PRD_treat == 1,
  unique(mun_id)
]

# Define column selections once
ref_cols <- c(
  "year",
  "mun_id",
  "next_PRD_pct",
  "PRD_pct",
  "next_PAN_pct",
  "PAN_pct",
  "estado",
  "PRD_margin",
  "next_PRD_margin",
  "PRI_pct",
  "next_PRI_pct",
  "next_turnout_pct"
)

main_cols <- c(
  "year",
  "mun_id",
  "PRD_pct",
  "PRD_margin",
  "next_PRD_margin",
  "PAN_pct",
  "estado",
  "PRD_treat"
)

# Create filtered datasets with proper exclusions
ref_PRD_not_treated <- df_ref[!mun_id %in% treated_muns, ..ref_cols]
main_mun_PRD_not_treated <- df[!mun_id %in% treated_muns_before, ..main_cols]

# Perform merge operations more efficiently
# First merge: neighbors with reference data
ref_merged <- left_join(
  ref_PRD_not_treated,
  dH_df,
  by = c("mun_id" = "neighbor"),
  relationship = "many-to-many"
)

# Rename columns in bulk using setnames for data.table
old_names <- c(
  "mun_id",
  "PRD_pct",
  "next_PRD_pct",
  "PAN_pct",
  "next_PAN_pct",
  "estado",
  "year",
  "PRD_margin",
  "next_PRD_margin",
  "PRI_pct",
  "next_PRI_pct",
  "next_turnout_pct"
)

new_names <- c(
  "ref_mun_id",
  "ref_PRD_pct",
  "ref_next_PRD_pct",
  "ref_PAN_pct",
  "ref_next_PAN_pct",
  "ref_estado",
  "ref_year",
  "ref_PRD_margin",
  "ref_next_PRD_margin",
  "ref_PRI_pct",
  "ref_next_PRI_pct",
  "ref_next_turnout_pct"
)

setnames(ref_merged, old_names, new_names)

# Final merge and variable creation
rdd_PRD_subset <- left_join(
  main_mun_PRD_not_treated,
  ref_merged,
  by = join_by("mun_id" == "mun", "year" == "ref_year")
)

# Sort using data.table syntax (much faster)
setorder(rdd_PRD_subset, mun_id, dH)

new0 <- subset(rdd_PRD_subset, estado == ref_estado)

load("~/mexico_mun/data/jaccard_similarity_AM_FM.Rdata")

new <- merge(new0, js, by = c("mun_id", "ref_mun_id"))

new$change_pct_PRD <- new$ref_PRD_pct - new$ref_next_PRD_pct
new$change_pct_PRI <- new$ref_PRI_pct - new$ref_next_PRI_pct
new$change_pct_PAN <- new$ref_PAN_pct - new$ref_next_PAN_pct

new$PRD_treat <- ifelse(new$mun_id %in% treated_muns_did, 1, 0)

new$t <- NA
new$t[new$year == 1985] <- -3
new$t[new$year >= 1986 & new$year <= 1988] <- -2
new$t[new$year >= 1989 & new$year <= 1991] <- -1
new$t[new$year >= 1992 & new$year <= 1994] <- 0
new$t[new$year >= 1995 & new$year <= 1997] <- 1

new$t_factor <- as.factor(new$t)
new$t_factor <- relevel(new$t_factor, ref = "0")

new$mun_year <- paste(new$mun_id, new$year)

new$dist_std <- scale(new$dH)

pt <- new %>%
  filter(t != -3) %>%
  group_by(PRD_treat, t) %>%
  summarize(avg_outcome = mean(change_pct_PRD, na.rm = T)) %>%
  mutate(PRD_treat = as.factor(PRD_treat))

parallel_trends <- ggplot(pt, aes(x = t, y = avg_outcome, color = PRD_treat)) +
  geom_vline(xintercept = 0, color = "red", linetype = 2) +
  geom_point() +
  geom_line(data = pt %>% filter(PRD_treat == 0)) +
  geom_line(data = pt %>% filter(PRD_treat == 1)) +
  scale_color_manual(
    name = "PRD Exposure",
    values = c("0" = "gray30", "1" = "gray70")
  ) +
  labs(x = "Election", y = "Average Change in PRD Vote Share") +
  theme_classic()
print(parallel_trends)

ggsave(
  filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/TYP_draft/images/parallel_trends_test.pdf",
  plot = parallel_trends,
  width = 6,
  height = 4
)

m1 <- feols(
  change_pct_PRD ~
    PRD_treat *
      js *
      t_factor +
      dist_std +
      ref_PRD_pct +
      ref_PRI_pct +
      ref_PAN_pct |
      mun_id + year + mun_year,
  cluster = "ref_mun_id",
  data = new %>% filter(t != -3)
)
etable(m1, digits = "r3")

#plot
plot_data <- as.data.frame(cbind(m1$coefficients, m1$se))
colnames(plot_data) <- c("est", "se")
plot_data$vars <- row.names(plot_data)

#CIs
plot_data$ci_95high <- plot_data$est + 1.96 * plot_data$se
plot_data$ci_95low <- plot_data$est - 1.96 * plot_data$se

plot_data$ci_90high <- plot_data$est + 1.645 * plot_data$se
plot_data$ci_90low <- plot_data$est - 1.645 * plot_data$se

# Filter the data frame
plot_data <- plot_data %>%
  filter(
    vars %in%
      c(
        "PRD_treat:js:t_factor-3",
        "PRD_treat:js:t_factor-2",
        "PRD_treat:js:t_factor-1",
        "PRD_treat:js:t_factor1"
      )
  ) %>%
  mutate(t = as.numeric(str_extract(vars, "-?\\d+")))

plot_data <- rbind(plot_data, c(0, NA, NA, NA, NA, NA, NA, 0))

p <- ggplot(plot_data, aes(x = t, y = est)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "red", linetype = 2) +
  geom_errorbar(
    aes(ymin = ci_95low, ymax = ci_95high),
    width = 0,
    linewidth = 0.5
  ) +
  geom_errorbar(
    aes(ymin = ci_90low, ymax = ci_90high),
    width = 0,
    linewidth = 3,
    alpha = 0.4
  ) +
  geom_point() +
  #geom_line() +
  labs(x = "Election", y = "Coefficient and 95% CI", title = "") +
  theme_minimal()
print(p)

ggsave(
  filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/TYP_draft/images/did_placebo_test.pdf",
  plot = p,
  width = 6,
  height = 4
)

linearHypothesis(
  m1,
  c(
    "PRD_treat:js:t_factor-2 = 0",
    "PRD_treat:js:t_factor-1 = 0",
    "PRD_treat:js:t_factor0 = 0"
  )
)
linearHypothesis(m1, "PRD_treat:js:as.factor(t)-2 = PRD_treat:js:as.factor(t)1")
linearHypothesis(m1, "PRD_treat:js:as.factor(t)-1 = PRD_treat:js:as.factor(t)1")
linearHypothesis(m1, "PRD_treat:js:as.factor(t)0 = PRD_treat:js:as.factor(t)1")


m1 <- feols(
  change_pct_PRD ~
    PRD_treat *
      js *
      t_factor +
      dist_std +
      ref_PRD_pct +
      ref_PRI_pct +
      ref_PAN_pct |
      mun_id + year + mun_year,
  cluster = "ref_mun_id",
  data = subset(new, t >= 1)
)

m2 <- feols(
  change_pct_PRI ~
    PRD_treat *
      js *
      t_factor +
      dist_std +
      ref_PRD_pct +
      ref_PRI_pct +
      ref_PAN_pct |
      mun_id + year + mun_year,
  cluster = "ref_mun_id",
  data = subset(new, t >= 1)
)

m3 <- feols(
  change_pct_PAN ~
    PRD_treat *
      js *
      t_factor +
      dist_std +
      ref_PRD_pct +
      ref_PRI_pct +
      ref_PAN_pct |
      mun_id + year + mun_year,
  cluster = "ref_mun_id",
  data = subset(new, t >= 1)
)

etable(m1, m2, m3, digits = "r3", tex = T)
