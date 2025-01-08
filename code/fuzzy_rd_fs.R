library(dplyr)
library(rdd)

setwd("~/mexico_mun")

load("~/mexico_RD/data/ingresos.Rdata")
load("~/mexico_RD/data/full_dataset_mexelec.Rdata")

#merge datasets
df <- merge(big_df,ing, by = c("mun_id", "year"))
df <- subset(df, year <= 1997)

DCdensity(df$PAN_pct, cutpoint = 0.5)

#income (raw)
rd0 <- RDestimate(ing0 ~ PAN_pct, cutpoint = 0.5, data = df)
summary(rd0)

rd1 <- RDestimate(ing_diff1 ~ PAN_pct, cutpoint = 0.5, data = df)
summary(rd1)

rd2 <- RDestimate(ing_diff2 ~ PAN_pct, cutpoint = 0.5, data = df)
summary(rd2)

rd3 <- RDestimate(ing_diff3 ~ PAN_pct, cutpoint = 0.5, data = df)
summary(rd3)

#taxes(raw)
rd4 <- RDestimate(imp0 ~ PAN_pct, cutpoint = 0.5, data = df)
summary(rd4)

rd5 <- RDestimate(imp_diff1 ~ PAN_pct, cutpoint = 0.5, data = df)
summary(rd5)

rd6 <- RDestimate(imp_diff2 ~ PAN_pct, cutpoint = 0.5, data = df)
summary(rd6)

rd7 <- RDestimate(imp_diff3 ~ PAN_pct, cutpoint = 0.5, data = df)
summary(rd7)

#pct income from taxes
rd8 <- RDestimate(pct_imp0 ~ PAN_pct, cutpoint = 0.5, data = df)
summary(rd8)

rd9 <- RDestimate(pi_diff1 ~ PAN_pct, cutpoint = 0.5, data = df)
summary(rd9)

rd10 <- RDestimate(pi_diff2 ~ PAN_pct, cutpoint = 0.5, data = df)
summary(rd10)

plot(rd10, range = c(0.5-rd10$bw[1],0.5+rd10$bw[1]))
abline(v = 0.5, col = "red", lwd = 1)
title(main = "Effect of PAN win on % income from taxes", x = "PAN vote share", y = "Change in % Income from Taxes, t+2")

plot(rd10, range = c(0.5-rd10$bw[2],0.5+rd10$bw[2]))
plot(rd10, range = c(0.5-rd10$bw[3],0.5+rd10$bw[3]))

rd11 <- RDestimate(pi_diff3 ~ PAN_pct, cutpoint = 0.5, data = df)
summary(rd11)

#what about changing to changes from the previous year?
df$new <- df$pct_imp2 - df$pct_imp1
rdnew <- RDestimate(new ~ PAN_pct, cutpoint = 0.5, data = df)
summary(rdnew)

plot(rdnew, range = c(0.5-rdnew$bw[1],0.5+rdnew$bw[1]))
abline(v = 0.5, col = "red", lwd = 1)
title(x = "PAN vote share", y = "Change in % Income from Taxes, t+2")

plot(rdnew, range = c(0.5-rdnew$bw[2],0.5+rdnew$bw[2]))


df$new2 <- df$pct_imp3 - df$pct_imp2
rdnew2 <- RDestimate(new2 ~ PAN_pct, cutpoint = 0.5, data = df)
summary(rdnew2)

#participaciones federales
rd12 <- RDestimate(pct_part0 ~ PAN_pct, cutpoint = 0.5, data = df)
summary(rd12)

rd13 <- RDestimate(pp_diff1 ~ PAN_pct, cutpoint = 0.5, data = df)
summary(rd13)

rd14 <- RDestimate(pp_diff2 ~ PAN_pct, cutpoint = 0.5, data = df)
summary(rd14)

rd15 <- RDestimate(pp_diff3 ~ PAN_pct, cutpoint = 0.5, data = df)
summary(rd15)

#ayudas sociales (pct de egresos)
rd12 <- RDestimate(pct_ayd0 ~ PAN_pct, cutpoint = 0.5, data = df)
summary(rd12)

rd13 <- RDestimate(pa_diff1 ~ PAN_pct, cutpoint = 0.5, data = df)
summary(rd13) #very small sample, so I don't really trust this

plot(rd13, range = c(0.5-rd13$bw[1],0.5+rd13$bw[1]))
plot(rd13, range = c(0.5-rd13$bw[2],0.5+rd13$bw[2]))
plot(rd13, range = c(0.5-rd13$bw[3],0.5+rd13$bw[3]))

rd14 <- RDestimate(pa_diff2 ~ PAN_pct, cutpoint = 0.5, data = df)
summary(rd14)

rd15 <- RDestimate(pa_diff3 ~ PAN_pct, cutpoint = 0.5, data = df)
summary(rd15)

