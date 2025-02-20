library(dplyr)
library(rdd)
library(rdrobust)
library(ggplot2)
library(data.table)

load("~/mexico_mun/data/full_dataset_mexbudget.Rdata")
load("~/mexico_mun/data/full_dataset_mexelec.Rdata")

wide_budget_df$year <- wide_budget_df$ANIO

#merge datasets
df <- merge(big_df,wide_budget_df, by = c("mun_id", "year"))
df <- subset(df, year <= 2000 & year >= 1995)

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
df <-  subset(big_df, year >= 1995 & year <= 1997)
              #& estado != "Gunajuato" & estado != "Chihuahua" & estado != "Baja California" & estado != "Jalisco")

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

#t model is confusing, change to zero
ci_data[1,2] <- 0
ci_data[1,3] <- NA
ci_data[1,4] <- NA
ci_data[1,5] <- NA
ci_data[1,6] <- NA

# Create the plot
taxation <- ggplot(ci_data, aes(x = Model, y = Coefficient)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
  geom_errorbar(aes(ymin = CI_Lower90, ymax = CI_Upper90), width = 0.2, alpha = 0.5, color = "red") +
  theme_minimal() +
  labs(title = "Effect of PAN win on municipal taxation",
       subtitle = "Robust 95% (black) and 90% (red) CIs",
       x = "",
       y = "% increase in income from taxes")

print(taxation)

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/3YP_Presentation_2_17_25/images/taxation.png", plot = taxation, width = 6, height = 4)

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
df$estado <- as.factor(df$estado)

rd12 <- rdrobust(df$pct_part0, df$PAN_pct, c = 0.5, covs = cbind(df$year,df$estado))
summary(rd12)

rd13 <- rdrobust(df$pp_diff1, df$PAN_pct, c = 0.5, covs = cbind(df$year,df$estado))
summary(rd13)

rd14 <- rdrobust(df$pp_diff2, df$PAN_pct, c = 0.5, covs = cbind(df$year,df$estado))
summary(rd14)

rd15 <- rdrobust(df$pp_diff3, df$PAN_pct, c = 0.5, covs = cbind(df$year,df$estado))
summary(rd15)

#participaciones diversas
lags <- 1:3

# Order the dataframe ing by year
df <- df %>% arrange(year)

setDT(df)[, paste0("diversas", lags) := lapply(lags, function(x) shift(Ingresos.Partida.Genérica.Participaciones.diversas, x, fill = NA, type = "lead")), by = mun_id]

rd12 <- rdrobust(log(df$Ingresos.Partida.Genérica.Participaciones.diversas), df$PAN_pct, c = 0.5, covs = cbind(df$year,df$estado))
summary(rd12)

rd13 <- rdrobust(df$diversas1, df$PAN_pct, c = 0.5, covs = cbind(df$year,df$estado))
summary(rd13)

rd14 <- rdrobust(df$diversas2, df$PAN_pct, c = 0.5, covs = cbind(df$year,df$estado))
summary(rd14)

rd15 <- rdrobust(df$diversas3, df$PAN_pct, c = 0.5, covs = cbind(df$year,df$estado))
summary(rd15)



