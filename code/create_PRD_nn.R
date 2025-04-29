library(dplyr)

load("~/mexico_mun/data/rdd_distance_PRD.Rdata")

df_rdd_PRD_new <- subset(df_rdd_PRD, ref_PRD_wins == 0 & main_estado == ref_estado)

PRD_nn <- df_rdd_PRD_new %>%
  group_by(mun_id) %>%
  slice_head(n = 1)

PRD_nn <- PRD_nn %>%
  mutate(change_pp_PRD = ref_next_PRD_pct - ref_PRD_pct,
         change_pp_PAN = ref_next_PAN_pct - ref_PAN_pct,
         change_pp_PRI = ref_next_PRI_pct - ref_PRI_pct)

PRD_nn$main_estado <- as.factor(PRD_nn$main_estado)

save(PRD_nn, file = "C:/Users/adamd/Documents/mexico_mun/data/PRD_nn.Rdata")