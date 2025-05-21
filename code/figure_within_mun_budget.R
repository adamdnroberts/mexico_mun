library(dplyr)
library(rdd)
library(rdrobust)
library(ggplot2)
library(data.table)

load("~/mexico_mun/data/full_dataset_mexbudget.Rdata")
wide_budget_df$year <- wide_budget_df$ANIO

#rdrobust estimates

wide_budget_df <- wide_budget_df %>% arrange(year)

wide_budget_df$pct_tax <- wide_budget_df$Ingresos.Capítulo.Impuestos/wide_budget_df$Egresos.Tema.Total.de.egresos
wide_budget_df$pct_debt <- (wide_budget_df$Egresos.Capítulo.Deuda.pública)/wide_budget_df$Egresos.Tema.Total.de.egresos
wide_budget_df$pct_ip <- (wide_budget_df$Egresos.Capítulo.Inversión.pública)/wide_budget_df$Egresos.Tema.Total.de.egresos

budget <- wide_budget_df %>%
  group_by(mun_id) %>%
  mutate(tax_lead1 = lead(pct_tax, n = 1), tax_lead2 = lead(pct_tax, n = 2), tax_lead3 = lead(pct_tax, n = 3), 
         debt_lead1 = lead(pct_debt, n = 1), debt_lead2 = lead(pct_debt, n = 2), debt_lead3 = lead(pct_debt, n = 3),
         ip_lead1 = lead(pct_ip, n = 1), ip_lead2 = lead(pct_ip, n = 2), ip_lead3 = lead(pct_ip, n = 3)) %>%
  ungroup()

budget$pi_diff1 <- budget$tax_lead1 - budget$pct_tax
budget$pi_diff2 <- budget$tax_lead2 - budget$pct_tax
budget$pi_diff3 <- budget$tax_lead3 - budget$pct_tax

budget$ps_diff1 <- budget$debt_lead1 - budget$pct_debt
budget$ps_diff2 <- budget$debt_lead2 - budget$pct_debt
budget$ps_diff3 <- budget$debt_lead3 - budget$pct_debt

budget$ip_diff1 <- budget$ip_lead1 - budget$pct_ip
budget$ip_diff2 <- budget$ip_lead2 - budget$pct_ip
budget$ip_diff3 <- budget$ip_lead3 - budget$pct_ip

load("~/mexico_mun/data/PRD_not_treated.Rdata")

df <- merge(main_mun_PRD_not_treated, budget, by = c("mun_id", "year"))
df <- subset(df, year>= 1995 & year <= 1997)


robust_taxes1 <- rdrobust(y = df$pi_diff1, x = df$PRD_margin,  bwselect = "cerrd")
robust_taxes2 <- rdrobust(y = df$pi_diff2, x = df$PRD_margin,  bwselect = "cerrd")
robust_taxes3 <- rdrobust(y = df$pi_diff3, x = df$PRD_margin,  bwselect = "cerrd")

robust_debt1 <- rdrobust(y = df$ps_diff1, x = df$PRD_margin,  bwselect = "cerrd")
robust_debt2 <- rdrobust(y = df$ps_diff2, x = df$PRD_margin,  bwselect = "cerrd")
robust_debt3 <- rdrobust(y = df$ps_diff3, x = df$PRD_margin,  bwselect = "cerrd")

robust_ip1 <- rdrobust(y = df$ip_diff1, x = df$PRD_margin,  bwselect = "cerrd")
robust_ip2 <- rdrobust(y = df$ip_diff2, x = df$PRD_margin,  bwselect = "cerrd")
robust_ip3 <- rdrobust(y = df$ip_diff3, x = df$PRD_margin,  bwselect = "cerrd")

#GRAPH RESULTS

# Extract coefficients and confidence intervals
extract_ci <- function(model, period, dv) {
  coef <- model$coef[3]
  ci_lower <- model$ci[3, 1]
  ci_upper <- model$ci[3, 2]
  ci_lower90 <- model$coef[3] - model$se[3]*1.645
  ci_upper90 <- model$coef[3] + model$se[3]*1.645
  data.frame(Outcome = dv, Period = period, Coefficient = coef, CI_Lower = ci_lower, CI_Upper = ci_upper, CI_Lower90 = ci_lower90, CI_Upper90 = ci_upper90)
}

plot_data <- bind_rows(
  extract_ci(robust_taxes1, "t+1", "Taxes"),
  extract_ci(robust_taxes2, "t+2", "Taxes"),
  extract_ci(robust_taxes3, "t+3", "Taxes"),
  extract_ci(robust_debt1, "t+1", "Debt"),
  extract_ci(robust_debt2, "t+2", "Debt"),
  extract_ci(robust_debt3, "t+3", "Debt"),
  extract_ci(robust_ip1, "t+1", "Public Investment"),
  extract_ci(robust_ip2, "t+2", "Public Investment"),
  extract_ci(robust_ip3, "t+3", "Public Investment")
)


# Create the plot
PRD_gov <- ggplot(plot_data, aes(x = Period, y = Coefficient, color = Outcome)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), 
                width = 0, linewidth = 0.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = CI_Lower90, ymax = CI_Upper90), 
                width = 0, linewidth = 3, alpha = 0.4, position = position_dodge(width = 0.5)) +
  theme_minimal() +
  labs(title = "",
       subtitle = "",
       x = "",
       y = "Change in Outcome as % of Expenditures",
       caption = "Thick bars: 90% CI, Thin bars: 95% CI") +
  theme(legend.position = c(0.1, 0.1),
        legend.justification = c(0, 0),
        legend.background = element_rect(fill = "white", color = NA))

print(PRD_gov)

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/TYP Final Tables and Figures Appendix/images/PRD_governance.png", plot = PRD_gov, width = 6, height = 4)
