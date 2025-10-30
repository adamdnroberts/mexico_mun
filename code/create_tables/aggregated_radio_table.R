library(car)
library(dplyr)
library(ggplot2)
library(fixest)
library(data.table)

load("~/mexico_mun/data/jaccard_similarity_AM_FM.Rdata")
load("~/mexico_mun/data/mexico_municipal_elections.Rdata")
load("~/mexico_mun/data/rdd_PRD_subset.Rdata")
load("C:/Users/adamd/Documents/mexico_mun/data/nearest_neighbor_PRD.Rdata")

new1 <- rdd_PRD_subset %>%
  inner_join(js, by = join_by("mun_id" == "mun_id", "neighbor" == "ref_mun_id"))
new2 <- nearest_neighbor_PRD %>%
  inner_join(js, by = join_by("mun_id" == "mun_id", "neighbor" == "ref_mun_id"))

new <- new1 %>% filter(main_estado == ref_estado)

muns <- unique(nearest_neighbor_PRD$mun_id)

new$change_pct_PRD <- new$ref_PRD_pct - new$ref_next_PRD_pct
new$change_pct_PRI <- new$ref_PRI_pct - new$ref_next_PRI_pct
new$change_pct_PAN <- new$ref_PAN_pct - new$ref_next_PAN_pct
new$PRD_treat <- as.numeric(new$PRD_margin > 0)
new$above_median_js <- ifelse(new$js >= median(new$js), 1, 0)
new$PRD_treat_times_js <- new$PRD_treat * new$js
new$dist_std <- scale(new$dH)

new <- new %>% filter(mun_id %in% muns)

aggregated_data <- new %>%
  group_by(neighbor) %>%
  summarize(
    treatment_intensity = sum(PRD_treat_times_js),
    treatment = sum(PRD_treat),
    radio = mean(js),
    change_pct_PRD = first(change_pct_PRD),
    dist = mean(dH),
    ref_PRD_pct = first(ref_PRD_pct),
    ref_PAN_pct = first(ref_PAN_pct),
    ref_PRI_pct = first(ref_PRI_pct),
    estado = first(ref_estado),
    year = mean(ref_year)
  )

#base model
m1 <- feols(
  change_pct_PRD ~ treatment_intensity + treatment | estado,
  data = aggregated_data
)
summary(m1)

m2 <- feols(
  change_pct_PRD ~
    treatment_intensity +
      treatment +
      dist |
      estado,
  data = aggregated_data
)
summary(m2)

m3 <- feols(
  change_pct_PRD ~
    treatment_intensity +
      treatment +
      dist +
      ref_PRD_pct |
      estado,
  data = filter(aggregated_data, treatment_intensity > 0)
)
summary(m3)

etable(m1, m2, digits = "r3", digits.stats = "r3", tex = F)
