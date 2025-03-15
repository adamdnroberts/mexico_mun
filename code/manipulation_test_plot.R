library(rddensity)
library(ggplot2)

set.seed(42)

load("~/mexico_mun/data/full_dataset_mexelec_pcts.Rdata")

df <- subset(big_df, year>= 1995 & year <= 1997)
test <- na.omit(c(df$PAN_pct,df$PRD_pct))

#Manipulation testing
summary(rddensity(X = test))

mt <- rddensity(X = test)

plot <- rdplotdensity(mt, test, xlabel = "Opposition party vote share")

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/TYP Final Tables and Figures/images/manipulation_test.png", plot = plot$Estplot, width = 6, height = 4)
