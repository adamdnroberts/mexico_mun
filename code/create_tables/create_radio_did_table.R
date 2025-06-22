library(car)
library(dplyr)
library(ggplot2)
library(fixest)

load("~/mexico_mun/data/rdd_PRD_subset.Rdata")
load("C:/Users/adamd/Documents/mexico_mun/data/nearest_neighbor_PRD.Rdata")
load("C:/Users/adamd/Documents/mexico_mun/data/PRD_untreated_until.Rdata")

new <- merge(rdd_PRD_subset, js, by.x = c("mun_id", "neighbor"), by.y = c("mun_id", "ref_mun_id"))
new2 <- merge(nearest_neighbor_PRD, js, by.x = c("mun_id", "neighbor"), by.y = c("mun_id", "ref_mun_id"))

m <- rdrobust(y = new$change_pct_PRD, x = new$PRD_margin, p = 1, 
                     covs = cbind(new$main_year, new$main_estado, 
                                  new$dH, new$treated_neighbors,
                                  new$js), 
                     bwselect = "cerrd")
summary(m)

new$change_pct_PRD <- new$ref_PRD_pct - new$ref_next_PRD_pct
new$PRD_treat <- ifelse(new$PRD_margin > 0, 1, 0)
new$above_median_js <- ifelse(new$js >= mean(new$js), 1, 0)

#no outcome audit
m1 <- feols(change_pct_PRD ~ PRD_treat*js # + dH #+ PRD_treat*js^2 
             | mun_id + ref_estado,
             cluster = "neighbor",
             data = new)
etable(m1, digits = "r3")

coefplot(m1)

linearHypothesis(m1, "js = PRD_treat:js")
