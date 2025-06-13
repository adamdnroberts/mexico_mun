
load("~/mexico_mun/data/full_dataset_mexelec_pcts.Rdata")

big_df$PRD_wins <- as.numeric(grepl("PRD", big_df$p1_name))
big_df$PRD_coalition_wins <- ifelse(big_df$p1_name != "PRD" & big_df$PRD_wins == 1, 1, 0)

df <- subset(big_df, year <= 2000)

summary(df$PRD_wins[df$year == 1985])
