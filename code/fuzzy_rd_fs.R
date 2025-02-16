library(dplyr)
library(rdd)
library(rdrobust)
library(ggplot2)

setwd("~/mexico_mun")

load("~/mexico_mun/data/ingresos.Rdata")
load("~/mexico_mun/data/full_dataset_mexelec.Rdata")

#merge datasets
df <- merge(big_df,ing, by = c("mun_id", "year"))
df <- subset(df, year <= 1999 & year >= 1994)

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

#rdrobust estimates
robust_taxes0 <- rdrobust(y = df$pct_imp0, x = df$PAN_pct, c = 0.5, bwselect = "mserd")
summary(robust_taxes0)

robust_taxes1 <- rdrobust(y = df$pi_diff1, x = df$PAN_pct, c = 0.5, bwselect = "mserd")
summary(robust_taxes1)

robust_taxes2 <- rdrobust(y = df$pi_diff2, x = df$PAN_pct, c = 0.5, bwselect = "mserd")
summary(robust_taxes2)

robust_taxes3 <- rdrobust(y = df$pi_diff3, x = df$PAN_pct, c = 0.5, bwselect = "mserd")
summary(robust_taxes3)

#GRAPH RESULTS

# Extract coefficients and confidence intervals
extract_ci <- function(model, model_name) {
  coef <- model$coef[3]
  ci_lower <- model$ci[3, 1]
  ci_upper <- model$ci[3, 2]
  ci_lower90 <- model$coef[3] - model$se[3]*1.645
  ci_upper90 <- model$coef[3] + model$se[3]*1.645
  data.frame(Model = model_name, Coefficient = coef, CI_Lower = ci_lower, CI_Upper = ci_upper, CI_Lower90 = ci_lower90, CI_Upper90 = ci_upper90)
}

ci_data <- bind_rows(
  extract_ci(robust_taxes0, "t"),
  extract_ci(robust_taxes1, "t+1"),
  extract_ci(robust_taxes2, "t+2"),
  extract_ci(robust_taxes3, "t+3")
)

# Create the plot
ggplot(ci_data, aes(x = Model, y = Coefficient)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  geom_shape(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
  geom_errorbar(aes(ymin = CI_Lower90, ymax = CI_Upper90), width = 0.2, alpha = 0.5, color = "red") +
  theme_minimal() +
  labs(title = "Effect of PAN win on municipal taxation",
       subtitle = "Robust 95% (black) and 90% (red) CIs",
       x = "",
       y = "% increase in income from taxes")

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

pct_as1 <- rdrobust(y = df$pa_diff1, x = df$PAN_pct, c = 0.5, bwselect = "mserd")
summary(pct_as1)

test <- subset(df, PAN_pct <= 0.5 + pct_as1$bws[1,1] & PAN_pct >= 0.5 - pct_as1$bws[1,1])
rdplot(y = test$pa_diff1, x = test$PAN_pct, c = 0.5, p = 1)

rd14 <- RDestimate(pa_diff2 ~ PAN_pct, cutpoint = 0.5, data = df)
summary(rd14)

pct_as2 <- rdrobust(y = df$pa_diff2, x = df$PAN_pct, c = 0.5, bwselect = "mserd")
summary(pct_as2)

rdplot(y = df$pa_diff2, x = df$PAN_pct, c = 0.5, p = 1)


rd15 <- RDestimate(pa_diff3 ~ PAN_pct, cutpoint = 0.5, data = df)
summary(rd15)

#ayudas sociales (no transformation)
rd12 <- RDestimate(ayd0 ~ PAN_pct, cutpoint = 0.5, data = df)
summary(rd12)

rd13 <- RDestimate(ayd_diff1 ~ PAN_pct, cutpoint = 0.5, data = df)
summary(rd13) #very small sample, so I don't really trust this

pct_as1 <- rdrobust(y = df$ayd_diff1, x = df$PAN_pct, c = 0.5, bwselect = "mserd")
summary(pct_as1)

test <- subset(df, PAN_pct <= 0.5 + pct_as1$bws[1,1] & PAN_pct >= 0.5 - pct_as1$bws[1,1])
rdplot(y = test$pa_diff1, x = test$PAN_pct, c = 0.5, p = 1)

rd14 <- RDestimate(pa_diff2 ~ PAN_pct, cutpoint = 0.5, data = df)
summary(rd14)

pct_as2 <- rdrobust(y = df$pa_diff2, x = df$PAN_pct, c = 0.5, bwselect = "mserd")
summary(pct_as2)

rdplot(y = df$pa_diff2, x = df$PAN_pct, c = 0.5, p = 1)


rd15 <- RDestimate(pa_diff3 ~ PAN_pct, cutpoint = 0.5, data = df)
summary(rd15)

