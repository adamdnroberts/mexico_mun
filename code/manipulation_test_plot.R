library(rddensity)
library(ggplot2)

set.seed(42)

load("~/mexico_mun/data/full_dataset_mexelec_pcts.Rdata")

df <- subset(big_df, year>= 1995 & year <= 1997 & (p1_name == "PRI" | p2_name == "PRI"))
test <- na.omit(c(df$PAN_pct,df$PRD_pct))

#Manipulation testing
summary(rddensity(X = df$PAN_pct))
summary(rddensity(X = df$PRD_pct))

mtPAN <- rddensity(X = df$PAN_pct)
mtPRD <- rddensity(X = df$PRD_pct)

plotPAN <- rdplotdensity(mtPAN, df$PAN_pct, xlabel = "Opposition party vote share") #, plotRange = c(-0.3,0.3))
plotPRD <- rdplotdensity(mtPRD, df$PRD_pct, xlabel = "Opposition party vote share") #, plotRange = c(-0.3,0.3))

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/TYP Final Tables and Figures Appendix/images/manipulation_testPAN.png", plot = plotPAN$Estplot, width = 6, height = 4)

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/TYP Final Tables and Figures Appendix/images/manipulation_testPRD.png", plot = plotPRD$Estplot, width = 6, height = 4)

