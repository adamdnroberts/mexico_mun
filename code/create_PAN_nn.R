library(dplyr)

load("~/mexico_mun/data/rdd_distance_PAN.Rdata")

df_rdd_PAN_new <- subset(df_rdd_PAN, ref_PAN_wins == 0 & main_estado == ref_estado)

PAN_nn <- df_rdd_PAN_new %>%
  group_by(mun_id) %>%
  slice_head(n = 1)

PAN_nn <- PAN_nn %>%
  mutate(change_pp_PRD = ref_next_PRD_pct - ref_PRD_pct,
         change_pp_PAN = ref_next_PAN_pct - ref_PAN_pct)

PAN_nn$main_estado <- as.factor(PAN_nn$main_estado)

save(PAN_nn, file = "C:/Users/adamd/Documents/mexico_mun/data/PAN_nn.Rdata")
