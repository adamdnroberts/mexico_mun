library(dplyr)
library(rdd)
library(rdrobust)
library(ggplot2)
library(data.table)

load("~/mexico_mun/data/full_dataset_mexbudget.Rdata")
wide_budget_df$year <- wide_budget_df$ANIO

#rdrobust estimates

wide_budget_df <- wide_budget_df %>% arrange(year)

# Create a dataframe that counts the number of NAs in each column
na_counts <- data.frame(
  column_name = names(wide_budget_df),
  na_count = sapply(wide_budget_df, function(x) sum(is.na(x)))
)

# Print the resulting dataframe
print(na_counts)

wide_budget_df$pct_ip <- (wide_budget_df$Egresos.Capítulo.Inversión.pública)/wide_budget_df$Egresos.Tema.Total.de.egresos

ip <- wide_budget_df %>%
  group_by(mun_id) %>%
  mutate(ip_lead1 = lead(pct_ip, n = 1), ip_lead2 = lead(pct_ip, n = 2), ip_lead3 = lead(pct_ip, n = 3)) %>%
  ungroup()

ip$ps_diff1 <- ip$ip_lead1 - ip$pct_ip
ip$ps_diff2 <- ip$ip_lead2 - ip$pct_ip
ip$ps_diff3 <- ip$ip_lead3 - ip$pct_ip

#df <-  subset(big_df, year >= 1995 & year <= 1997)
#& estado != "Gunajuato" & estado != "Chihuahua" & estado != "Baja California" & estado != "Jalisco")

#df <- merge(big_df,ip, by = c("mun_id", "year"))
#df <- subset(df, year>= 1995 & year <= 1997
#             #& estado!="Tlaxcala" 
#             & (p1_name == "PRI" | p2_name == "PRI") & (p1_name == "PRD" | p2_name == "PRD"#)
#)

load("~/mexico_mun/data/PRD_not_treated.Rdata")

df <- merge(main_mun_PRD_not_treated,ip, by = c("mun_id", "year"))
df <- subset(df, year>= 1995 & year <= 1997)


robust_ip1 <- rdrobust(y = df$ps_diff1, x = df$PRD_margin,  bwselect = "cerrd")
robust_ip2 <- rdrobust(y = df$ps_diff2, x = df$PRD_margin,  bwselect = "cerrd")
robust_ip3 <- rdrobust(y = df$ps_diff3, x = df$PRD_margin,  bwselect = "cerrd")

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
  extract_ci(robust_ip1, "t"),
  extract_ci(robust_ip1, "t+1"),
  extract_ci(robust_ip2, "t+2"),
  extract_ci(robust_ip3, "t+3")
)

#t model is confusing, change to zero
ci_data[1,2] <- 0
ci_data[1,3] <- NA
ci_data[1,4] <- NA
ci_data[1,5] <- NA
ci_data[1,6] <- NA

# Create the plot
ip <- ggplot(ci_data, aes(x = Model, y = Coefficient)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), 
                width = 0, linewidth = 0.5) +
  geom_errorbar(aes(ymin = CI_Lower90, ymax = CI_Upper90), 
                width = 0, linewidth = 3, alpha = 0.4) +
  theme_minimal() +
  labs(title = "Effect of PRD Win on Municipal Public Investment",
       subtitle = "Robust RD Coefficients",
       x = "",
       y = "PI as % of Expenditures",
       caption = "Thick bars: 90% CI, Thin bars: 95% CI")

print(ip)

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/TYP Final Tables and Figures Appendix/images/public_investment.png", plot = ip, width = 6, height = 4)
