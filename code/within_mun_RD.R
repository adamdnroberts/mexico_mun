library(rdrobust)

load("~/mexico_mun/data/full_dataset_mexelec_pcts.Rdata")

df <- subset(big_df, year>= 1995 & year <= 1997 )

#what's going on
test <- rdrobust(y = df$next_PAN_pct, x = df$PAN_pct, p = 1, bwselect = "cerrd", level = 90)
summary(test)

rdplot(y = df$next_PAN_pct, x = df$PAN_pct, p = 1, title = "PAN subsequent election model")

test <- rdrobust(y = df$next_PRD_pct, x = df$PRD_pct, p = 1, bwselect = "cerrd", level = 90)
summary(test)

rdplot(y = df$next_PRD_pct, x = df$PRD_pct, p = 1, title = "PRD subsequent election model")
