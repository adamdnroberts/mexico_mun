library(dplyr)
library(rdrobust)
library(fixest)

load("~/mexico_mun/data/pairwise_km.Rdata")

load("~/mexico_mun/data/full_dataset_mexelec_pcts.Rdata")
big_df$mun_id <- gsub(" ", "", big_df$Municipio)

big_df$PRD_cand <- 0
big_df$PRD_cand[big_df$PRD > 0] <- 1

cand_index <- big_df %>%
  filter(year <= 1997) %>%
  group_by(mun_id) %>%
  summarise(c_index = sum(PRD_cand))

df <- subset(big_df, year>= 1995 & year <= 1997 
             #& estado!="Tlaxcala" 
             #& (p1_name == "PRI" | p2_name == "PRI") & (p1_name == "PRD" | p2_name == "PRD")
)

#df <- df[, colSums(is.na(df)) != nrow(df)] No coalitions in this period!

df_ref_no_index <- subset(big_df, year>= 1995 & year <= 1997 
                          #& estado!="Tlaxcala" 
                          #& (p1_name == "PRI" | p2_name == "PRI") & (p1_name == "PRD" | p2_name == "PRD")
)

df_ref <- merge(df_ref_no_index, cand_index, by = "mun_id", all.x = T)

#summary(df_ref$c_index)
#hist(df_ref$c_index)

treated_df <- subset(big_df, year <= 1997 & PRD_treat == 1)
treated <- unique(treated_df$mun_id)

treated_df_before <- subset(big_df, year <= 1994 & PRD_treat == 1)
treated_before <- unique(treated_df_before$mun_id)

#create smaller datasets for merge
ref_PRD <- subset(df_ref, select = c(year, mun_id, next_PRD_pct, PRD_pct, next_PAN_pct, PAN_pct, estado, c_index))
ref_PRD_not_treated <- ref_PRD[!ref_PRD$mun_id %in% treated, ]

main_mun_PRD <- subset(df, select = c(year, mun_id, PRD_pct, PAN_pct, estado))
main_mun_PRD_not_treated <- main_mun_PRD[!main_mun_PRD$mun_id %in% treated_before, ]

#merge datasets using adjacent municipalities index
ref2 <- merge(dH_df,ref_PRD_not_treated, by.x = c("neighbor"), by.y = c("mun_id"))
ref2 <- ref2 %>% rename(ref_PRD_pct = PRD_pct, ref_next_PRD_pct = next_PRD_pct, 
                        ref_PAN_pct = PAN_pct, ref_next_PAN_pct = next_PAN_pct, 
                        ref_estado = estado, ref_year = year)
ref2$ref_PRD_wins <- ifelse(ref2$ref_PRD_pct > 0, 1, 0)

df_rdd <- merge(main_mun_PRD_not_treated,ref2, by.x = c("mun_id"), by.y = c("mun"))
df_rdd <- df_rdd %>% 
  rename(main_year = year, main_estado = estado)

df_rdd$weight <- 1/df_rdd$dH

# First, sort by mun_id and then by d
df_rdd_sorted <- df_rdd %>%
  arrange(mun_id, dH)

df_rdd_PRD <- df_rdd_sorted

df_rdd_PRD_new <- subset(df_rdd_PRD, ref_PRD_wins == 0 & main_estado == ref_estado & ref_next_PRD_pct > -0.5)

PRD_nn <- df_rdd_PRD_new %>%
  group_by(mun_id) %>%
  slice_head(n = 1)

PRD_nn <- PRD_nn %>%
  mutate(change_pp_PRD = ref_next_PRD_pct - ref_PRD_pct,
         change_pp_PAN = ref_next_PAN_pct - ref_PAN_pct)

PRD_nn$main_estado <- as.factor(PRD_nn$main_estado)

test <- PRD_nn

test$PRD_treat <- ifelse(test$PRD_pct > 0, 1, 0)
m <- feols(change_pp_PRD ~ PRD_pct + PRD_treat*c_index | main_estado, cluster = "neighbor", data = test)
summary(m)

cerm_PRD <- rdrobust(y = PRD_nn$change_pp_PRD, x = PRD_nn$PRD_pct, p = 1, covs = cbind(PRD_nn$main_year, PRD_nn$main_estado, PRD_nn$dH, PRD_nn$c_index), bwselect = "cerrd", level = 90)
summary(cerm_PRD)

test <- subset(PRD_nn, c_index <= 2)

cerm_PRD <- rdrobust(y = test$change_pp_PRD, x = test$PRD_pct, p = 1, covs = cbind(test$main_year, test$main_estado, test$dH, test$c_index), bwselect = "cerrd", level = 90)
summary(cerm_PRD)

test <- subset(PRD_nn, c_index > 2)

cerm_PRD <- rdrobust(y = test$change_pp_PRD, x = test$PRD_pct, p = 1, covs = cbind(test$main_year, test$main_estado, test$dH, test$c_index), bwselect = "cerrd", level = 90)
summary(cerm_PRD)
