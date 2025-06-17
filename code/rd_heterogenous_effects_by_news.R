library(fixest)

options(scipen = 99)

load("C:/Users/adamd/Documents/mexico_mun/data/nearest_neighbor_PRD.Rdata")
load("~/mexico_mun/data/rdd_PRD_subset.Rdata")

new <- merge(rdd_PRD_subset, js, by.x = c("mun_id", "neighbor"), by.y = c("mun_id", "ref_mun_id"), all.x = T)

new$PRD_treated <- ifelse(new$PRD_margin > 0, 1, 0)
new$pair_id <- paste(new$mun_id,new$neighbor)

new2 <- new %>%
  filter(ref_PRD_wins == 0, main_estado == ref_estado) %>%
  mutate(
    change_pct_PRD = ref_next_PRD_pct - ref_PRD_pct,
    change_pct_PAN = ref_next_PAN_pct - ref_PAN_pct,
    change_pct_PRI = ref_next_PRI_pct - ref_PRI_pct,
    main_estado = as.factor(main_estado)
  )

test <- feols(change_pct_PRD ~ PRD_margin*PRD_treated*js + dH | main_estado + mun_id, 
                     cluster = c("mun_id", "main_year"), data = new2)
summary(test)

new <- merge(nearest_neighbor_PRD, js, by.x = c("mun_id", "neighbor"), by.y = c("mun_id", "ref_mun_id"), all.x = T)

min <- summary(new$js)[1]
q1 <- summary(new$js)[2]
med <- summary(new$js)[3]
q3 <- summary(new$js)[5]
max <- summary(new$js)[6]

new$q1_js <- ifelse(new$js >= min & new$js <= q1, 1, 0)
new$q2_js <- ifelse(new$js > q1 & new$js <= med, 1, 0)
new$q3_js <- ifelse(new$js > med & new$js <= q3, 1, 0)
new$q4_js <- ifelse(new$js > q3 & new$js <= max, 1, 0)

new1 <- subset(new, q1_js == 1)

media_1 <- rdrobust(y = new1$change_pct_PRD, x = new1$PRD_margin, p = 1, 
                                covs = cbind(new1$main_year, new1$main_estado, 
                                             new1$dH, new1$treated_neighbors), 
                                bwselect = "cerrd")
summary(media_1)

new2 <- subset(new, q2_js == 1)

media_2 <- rdrobust(y = new2$change_pct_PRD, x = new2$PRD_margin, p = 1, 
                                covs = cbind(new2$main_year, new2$main_estado, 
                                             new2$dH, new2$treated_neighbors), 
                                bwselect = "cerrd")
summary(media_2)

new3 <- subset(new, q3_js == 1)

media_3 <- rdrobust(y = new3$change_pct_PRD, x = new3$PRD_margin, p = 1, 
                                covs = cbind(new3$main_year, new3$main_estado, 
                                             new3$dH, new3$treated_neighbors), 
                                bwselect = "cerrd")
summary(media_3)

new4 <- subset(new, q4_js == 1)

media_4<- rdrobust(y = new4$change_pct_PRD, x = new4$PRD_margin, p = 1, 
                                covs = cbind(new4$main_year, new4$main_estado, 
                                             new4$dH, new4$treated_neighbors), 
                                bwselect = "cerrd")
summary(media_4)



optimal_bw <- rdbwselect(y = nearest_neighbor_PRD$change_pct_PRD, x = nearest_neighbor_PRD$PRD_margin, p = 1, 
                                           covs = cbind(nearest_neighbor_PRD$main_year, nearest_neighbor_PRD$main_estado, 
                                                        nearest_neighbor_PRD$dH, nearest_neighbor_PRD$treated_neighbors), 
                                           bwselect = "cerrd")

diff_in_disc <- lm(change_pct_PRD ~ PRD_margin*PRD_treated + as.factor(main_year) + main_estado + dH, 
                   data = subset(new, abs(PRD_margin) < optimal_bw$bws[1]))
summary(diff_in_disc)

diff_in_disc <- lm(change_pct_PRD ~ PRD_margin*PRD_treated*above_median_js + as.factor(main_year) + main_estado + dH, data = new)
summary(diff_in_disc)                   
