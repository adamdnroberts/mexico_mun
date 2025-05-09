library(dplyr)

load("C:/Users/adamd/Documents/mexico_mun/data/PRD_nn.Rdata")
load("~/mexico_mun/data/full_dataset_mexelec_pcts.Rdata")
load("~/mexico_mun/data/pairwise_km.Rdata")

#distances of nearest untreated municipalities
nearest_untreated <- subset(PRD_nn, select= c(neighbor, dH))
nearest_untreated$tsm_distance <- nearest_untreated$dH
nearest_untreated$dH <- NULL

#merge
pairs_with_untreated_dh <- merge(nearest_untreated, dH_df, by.x = "neighbor", by.y = "neighbor")

#get treated municipalities
big_df$mun_id <- gsub(" ", "", big_df$Municipio)

df_ref <- subset(big_df, year>= 1995 & year <= 1997)

treated_df <- subset(big_df, year <= 1997 & PRD_treat == 1)
treated <- unique(treated_df$mun_id)

#subset pars df
pairs_closer_than_untreated <- subset(pairs_with_untreated_dh, dH < tsm_distance)
close_treated <- pairs_closer_than_untreated[pairs_closer_than_untreated$mun %in% treated, ]

new_new_new_test <- close_treated %>%
  group_by(neighbor) %>%
  summarize(treated_neighbors = n())

hi <- merge(PRD_nn, new_new_new_test, by = "neighbor", all.x = T)
hi$treated_neighbors[is.na(hi$treated_neighbors)] <- 0
summary(hi$treated_neighbors)

nc_PRD <- rdrobust(y = hi$change_pp_PRD, x = hi$PRD_margin, p = 1, bwselect = "cerrd", level = 90)
summary(nc_PRD)

cerm_PRD <- rdrobust(y = hi$change_pp_PRD, x = hi$PRD_margin, p = 1, 
                     covs = cbind(hi$main_year, hi$main_estado, hi$dH, hi$treated_neighbors), 
                     bwselect = "cerrd", level = 90)
summary(cerm_PRD)

#only those with 0 closer treated
hi2 <- subset(hi, treated_neighbors > 0)

cerm_PRD <- rdrobust(y = hi2$change_pp_PRD, x = hi2$PRD_margin, p = 1, 
                     covs = cbind(hi2$main_year, hi2$main_estado, hi2$dH, hi2$treated_neighbors), 
                     bwselect = "cerrd", level = 90)
summary(cerm_PRD)

hi$PRD_treated <- ifelse(hi$PRD_margin > 0, 1, 0)
test <- lm(change_pp_PRD ~ PRD_margin*PRD_treated*treated_neighbors + main_estado + main_year + dH, data = subset(hi, abs(PRD_margin) < 0.85))
summary(test)

coefs <- test$coefficients
filtered_coefs <- coefs[grepl("PRD_margin:PRD_treated:main_estado", names(coefs))]
test <- as.data.frame(filtered_coefs)
test$state <- gsub("PRD_margin:PRD_treated:main_estado", "", rownames(filtered_coefs))
test$coef <- test$`coefs[grepl("PRD_margin:PRD_treated:main_estado", names(coefs))]`
test$`coefs[grepl("PRD_margin:PRD_treated:main_estado", names(coefs))]` <- NULL
rownames(test) <- NULL



