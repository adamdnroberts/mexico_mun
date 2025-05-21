library(dplyr)
library(rdd)
library(rdrobust)
library(ggplot2)
library(data.table)

load("~/mexico_mun/data/full_dataset_mexbudget.Rdata")
wide_budget_df$year <- wide_budget_df$ANIO

#rdrobust estimates

wide_budget_df <- wide_budget_df %>% arrange(year)

wide_budget_df$pct_imp <- wide_budget_df$Ingresos.Capítulo.Impuestos/wide_budget_df$Ingresos.Tema.Total.de.ingresos

bud_lead <- wide_budget_df %>%
  group_by(mun_id) %>%
  mutate(imp_lead1 = lead(pct_imp, n = 1), imp_lead2 = lead(pct_imp, n = 2), imp_lead3 = lead(pct_imp, n = 3),
         taxes1 = lead(Ingresos.Capítulo.Impuestos, n = 1), taxes2 = lead(Ingresos.Capítulo.Impuestos, n = 2), 
         taxes3 = lead(Ingresos.Capítulo.Impuestos, n = 3)) %>%
  ungroup()

bud_lead$pi_diff1 <- bud_lead$imp_lead1 - bud_lead$pct_imp
bud_lead$pi_diff2 <- bud_lead$imp_lead2 - bud_lead$pct_imp
bud_lead$pi_diff3 <- bud_lead$imp_lead3 - bud_lead$pct_imp

bud_lead$tax_increase1 <- (bud_lead$taxes1 - bud_lead$Ingresos.Capítulo.Impuestos)/bud_lead$Ingresos.Capítulo.Impuestos
bud_lead$tax_increase2 <- (bud_lead$taxes2 - bud_lead$Ingresos.Capítulo.Impuestos)/bud_lead$Ingresos.Capítulo.Impuestos
bud_lead$tax_increase3 <- (bud_lead$taxes3 - bud_lead$Ingresos.Capítulo.Impuestos)/bud_lead$Ingresos.Capítulo.Impuestos

#df <-  subset(big_df, year >= 1995 & year <= 1997)
#& estado != "Gunajuato" & estado != "Chihuahua" & estado != "Baja California" & estado != "Jalisco")

#df <- merge(big_df,bud_lead, by = c("mun_id", "year"))
#df <- subset(df, year>= 1995 & year <= 1997
#             #& estado!="Tlaxcala" 
#             & (p1_name == "PRI" | p2_name == "PRI") & (p1_name == "PRD" | p2_name == "PRD"#)
#)

load("~/mexico_mun/data/PRD_not_treated.Rdata")

df <- merge(main_mun_PRD_not_treated,bud_lead, by = c("mun_id", "year"))
df <- subset(df, year>= 1995 & year <= 1997)


robust_taxes0 <- rdrobust(y = df$pi_diff1, x = df$PRD_margin,  bwselect = "cerrd")
robust_taxes1 <- rdrobust(y = df$pi_diff1, x = df$PRD_margin,  bwselect = "cerrd")
summary(robust_taxes1)
robust_taxes2 <- rdrobust(y = df$pi_diff2, x = df$PRD_margin,  bwselect = "cerrd")
summary(robust_taxes2)
robust_taxes3 <- rdrobust(y = df$pi_diff3, x = df$PRD_margin,  bwselect = "cerrd")
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
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  #geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), 
                width = 0, linewidth = 0.5) +
  geom_errorbar(aes(ymin = CI_Lower90, ymax = CI_Upper90), 
                width = 0, linewidth = 3, alpha = 0.4) +
  theme_minimal() +
  labs(title = "Effect of PRD Win on Municipal Taxation",
       subtitle = "Robust RD Coefficients",
       x = "",
       y = "% Increase in Income from Taxes",
       caption = "Thick bars: 90% CI, Thin bars: 95% CI")

print(taxation)

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/TYP Final Tables and Figures Appendix/images/taxation.png", plot = taxation, width = 6, height = 4)

#TAX INCREASES
df2 <- subset(df, tax_increase1 < 200)

tax_increase0 <- rdrobust(y = df2$tax_increase1, x = df2$PRD_margin,  bwselect = "cerrd")
tax_increase1 <- rdrobust(y = df2$tax_increase1, x = df2$PRD_margin,  bwselect = "cerrd")
summary(tax_increase1)
tax_increase2 <- rdrobust(y = df2$tax_increase2, x = df2$PRD_margin,  bwselect = "cerrd")
summary(tax_increase2)
tax_increase3 <- rdrobust(y = df2$tax_increase3, x = df2$PRD_margin,  bwselect = "cerrd")
summary(tax_increase3)
