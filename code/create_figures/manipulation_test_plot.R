library(rddensity)
library(ggplot2)

set.seed(42)

#load("~/mexico_mun/data/mexico_municipal_elections.Rdata")
load("~/mexico_mun/data/PRD_untreated.Rdata")
load("~/mexico_mun/data/PAN_untreated.Rdata")

#Manipulation testing
#summary(rddensity(X = main_mun_PAN_not_treated$PAN_margin))
#summary(rddensity(X = main_mun_PRD_not_treated$PRD_margin))

mtPAN <- rddensity(X = main_mun_PAN_not_treated$PAN_margin)
mtPRD <- rddensity(X = main_mun_PRD_not_treated$PRD_margin)

plotPAN <- rdplotdensity(mtPAN, main_mun_PAN_not_treated$PAN_margin, xlabel = "PAN vote share")
plotPRD <- rdplotdensity(mtPRD, main_mun_PRD_not_treated$PRD_margin, xlabel = "PRD vote share")

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/TYP Final Tables and Figures Appendix/images/manipulation_testPAN.png", plot = plotPAN$Estplot, width = 6, height = 4)

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/TYP Final Tables and Figures Appendix/images/manipulation_testPRD.png", plot = plotPRD$Estplot, width = 6, height = 4)

