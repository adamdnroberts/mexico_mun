library(dplyr)
library(rdd)
library(rdrobust)
library(ggplot2)

setwd("~/mexico_mun")

load("~/mexico_mun/data/ingresos.Rdata")
load("~/mexico_mun/data/full_dataset_mexelec_pcts.Rdata")

#merge datasets
df <- merge(big_df,ing, by = c("mun_id", "year"))
df <- subset(df, year <= 1999 & year >=1994)

DCdensity(df$PAN_pct, cutpoint = 0)
DCdensity(df$PRD_pct, cutpoint = 0)


#PAN estimates
PAN_taxes0 <- rdrobust(y = df$pct_imp0, x = df$PAN_pct,  bwselect = "mserd")
summary(PAN_taxes0)

PAN_taxes1 <- rdrobust(y = df$pi_diff1, x = df$PAN_pct,  bwselect = "mserd")
summary(PAN_taxes1)

PAN_taxes2 <- rdrobust(y = df$pi_diff2, x = df$PAN_pct,  bwselect = "mserd")
summary(PAN_taxes2)

PAN_taxes3 <- rdrobust(y = df$pi_diff3, x = df$PAN_pct,  bwselect = "mserd")
summary(PAN_taxes3)

#PRD estimates
PRD_taxes0 <- rdrobust(y = df$pct_imp0, x = df$PRD_pct,  bwselect = "mserd")
summary(PRD_taxes0)

PRD_taxes1 <- rdrobust(y = df$pi_diff1, x = df$PRD_pct,  bwselect = "mserd")
summary(PRD_taxes1)

PRD_taxes2 <- rdrobust(y = df$pi_diff2, x = df$PRD_pct,  bwselect = "mserd")
summary(PRD_taxes2)

PRD_taxes3 <- rdrobust(y = df$pi_diff3, x = df$PRD_pct,  bwselect = "mserd")
summary(PRD_taxes3)

#GRAPH RESULTS

# Extract coefficients and confidence intervals
extract_ci <- function(model, model_name, party) {
  coef <- model$coef[3]
  ci_lower <- model$ci[3, 1]
  ci_upper <- model$ci[3, 2]
  ci_lower90 <- model$coef[3] - model$se[3]*1.645
  ci_upper90 <- model$coef[3] + model$se[3]*1.645
  data.frame(Model = model_name, party = party, Coefficient = coef, CI_Lower = ci_lower, CI_Upper = ci_upper, CI_Lower90 = ci_lower90, CI_Upper90 = ci_upper90)
}

ci_data <- bind_rows(
  extract_ci(PAN_taxes0, "t", "PAN"),
  extract_ci(PAN_taxes1, "t+1", "PAN"),
  extract_ci(PAN_taxes2, "t+2", "PAN"),
  extract_ci(PAN_taxes3, "t+3", "PAN"),
  extract_ci(PRD_taxes0, "t", "PRD"),
  extract_ci(PRD_taxes1, "t+1", "PRD"),
  extract_ci(PRD_taxes2, "t+2", "PRD"),
  extract_ci(PRD_taxes3, "t+3", "PRD")
)

ci_data$Model <- as.factor(ci_data$Model)

# Create the plot
taxation <- ggplot(ci_data, aes(x = Model, y = Coefficient, color = party)) +
  geom_errorbar(aes(ymin = CI_Lower90, ymax = CI_Upper90), width = 0.1, position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  theme_minimal() +
  labs(title = "Effect of PAN win on municipal taxation",
       subtitle = "90% CIs",
       x = "",
       y = "% increase in income from taxes") +
  scale_color_manual(values = c("PAN" = "blue", "PRD" = "goldenrod"))

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/Third Year Paper Results Outline/images/tax_by_party.png", plot = p, width = 6, height = 4)
