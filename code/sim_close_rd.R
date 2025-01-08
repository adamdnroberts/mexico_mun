library(truncnorm)
library(ggplot2)

# Set the seed for reproducibility
set.seed(123)

el_pct <- rtruncnorm(n = 10000, a = 0, b = 1, mean = .3, sd = .1)

DCdensity(el_pct, cutpoint = 0.5)
hist(el_pct, breaks = 50)

tiny <- round(el_pct*500)/500
small <- round(el_pct*10000)/10000
med <- round(el_pct*250000)/250000
big <- round(el_pct*1000000)/1000000

crd <- as.data.frame(t(rbind(tiny,small, med, big)))

ggplot(crd) +
  geom_density(aes(x = small), bins = 100) +
  geom_density(aes(x = med), fill = "blue", alpha = 0.3, bins = 100) +
  geom_density(aes(x = big), alpha = 0.1, color = "red", bins = 100) +
  geom_density(aes(x = tiny), color = "green")

#using 

small_nom <- rtruncnorm(n = 10000, a = 0, b = 1, mean = 5000, sd = 1000)
med_nom <- rtruncnorm(n = 10000, a = 0, b = 1, mean = 5000, sd = 1000)
big_nom <- rtruncnorm(n = 10000, a = 0, b = 1, mean = 5000, sd = 1000)


small <- round(el_pct*10000)/10000
med <- round(el_pct*250000)/250000
big <- round(el_pct*1000000)/1000000

crd <- as.data.frame(t(rbind(small, med, big)))

ggplot(crd) +
  geom_density(aes(x = small), bins = 100) +
  geom_density(aes(x = med), fill = "blue", alpha = 0.3, bins = 100) +
  geom_density(aes(x = big), alpha = 0.1, color = "red", bins = 100)




