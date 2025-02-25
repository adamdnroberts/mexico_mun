library(ggplot2)

mex94_senate <- read.csv("~/mexico_mun/data/mex94_senate.csv")
summary(mex94_senate$Estado)

ggplot(mex94_senate) +
  geom_histogram(aes(x = PAN))

mex94_depPR <- read.csv("~/mexico_mun/data/mex94_drp.csv")

mex94_depMR <- read.csv("~/mexico_mun/data/mex94_dmr.csv")

mex94_pres <- read.csv("~/mexico_mun/data/mex94_pres.csv")
