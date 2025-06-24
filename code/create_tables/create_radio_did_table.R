library(car)
library(dplyr)
library(ggplot2)
library(fixest)

load("~/mexico_mun/data/jaccard_similarity_AM_FM.Rdata")
load("~/mexico_mun/data/mexico_municipal_elections.Rdata")
load("~/mexico_mun/data/rdd_PRD_subset.Rdata")
load("C:/Users/adamd/Documents/mexico_mun/data/nearest_neighbor_PRD.Rdata")

new1 <- merge(rdd_PRD_subset, js, by.x = c("mun_id", "neighbor"), by.y = c("mun_id", "ref_mun_id"))
new2 <- merge(nearest_neighbor_PRD, js, by.x = c("mun_id", "neighbor"), by.y = c("mun_id", "ref_mun_id"))

new <- subset(new1, main_estado == ref_estado)

new$change_pct_PRD <- new$ref_PRD_pct - new$ref_next_PRD_pct
new$change_pct_PRI <- new$ref_PRD_pct - new$ref_next_PRD_pct
new$PRD_treat <- ifelse(new$PRD_margin > 0, 1, 0)
new$above_median_js <- ifelse(new$js >= mean(new$js), 1, 0)

#no outcome audit
m1 <- feols(change_pct_PRD ~ PRD_treat*js + dH #+ PRD_treat*js^2 
             | mun_id,
             cluster = "neighbor",
             data = new)
etable(m1, digits = "r3")

coefplot(m1)

linearHypothesis(m1, "js = PRD_treat:js")
