library(dplyr)
library(rdd)
library(rdrobust)
library(ggplot2)
library(data.table)

load("~/mexico_mun/data/full_dataset_mexbudget.Rdata")
wide_budget_df$year <- wide_budget_df$ANIO

# Assuming wide_budget_df is your data frame
na_counts <- as.data.frame(sapply(wide_budget_df, function(x) sum(is.na(x))))

# Print the result
print(na_counts)

#rdrobust estimates

wide_budget_df <- wide_budget_df %>% arrange(year)

wide_budget_df$pct_part <- (wide_budget_df$Ingresos.Capítulo.Participaciones.federales)/wide_budget_df$Ingresos.Tema.Total.de.ingresos

part <- wide_budget_df %>%
  group_by(mun_id) %>%
  mutate(part_lead1 = lead(pct_part, n = 1), part_lead2 = lead(pct_part, n = 2), part_lead3 = lead(pct_part, n = 3)) %>%
  ungroup()

part$ps_diff1 <- part$part_lead1 - part$pct_part
part$ps_diff2 <- part$part_lead2 - part$pct_part
part$ps_diff3 <- part$part_lead3 - part$pct_part

load("~/mexico_mun/data/PRD_not_treated.Rdata")

df <- merge(main_mun_PRD_not_treated,part, by = c("mun_id", "year"))
df <- subset(df, year>= 1995 & year <= 1997)


robust_part0 <- rdrobust(y = df$ps_diff1, x = df$PRD_margin,  bwselect = "cerrd")
robust_part1 <- rdrobust(y = df$ps_diff1, x = df$PRD_margin,  bwselect = "cerrd")
summary(robust_part1)
robust_part2 <- rdrobust(y = df$ps_diff2, x = df$PRD_margin,  bwselect = "cerrd")
summary(robust_part2)
robust_part3 <- rdrobust(y = df$ps_diff3, x = df$PRD_margin,  bwselect = "cerrd")
summary(robust_part3)

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
  extract_ci(robust_part0, "t"),
  extract_ci(robust_part1, "t+1"),
  extract_ci(robust_part2, "t+2"),
  extract_ci(robust_part3, "t+3")
)

#t model is confusing, change to zero
ci_data[1,2] <- 0
ci_data[1,3] <- NA
ci_data[1,4] <- NA
ci_data[1,5] <- NA
ci_data[1,6] <- NA

# Create the plot
part <- ggplot(ci_data, aes(x = Model, y = Coefficient)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), 
                width = 0, linewidth = 0.5) +
  geom_errorbar(aes(ymin = CI_Lower90, ymax = CI_Upper90), 
                width = 0, linewidth = 3, alpha = 0.4) +
  theme_minimal() +
  labs(title = "Effect of PRD Win on Federal Transfers",
       subtitle = "Robust RD Coefficients",
       x = "",
       y = "Transfers as % of Municipal Income",
       caption = "Thick bars: 90% CI, Thin bars: 95% CI")

print(part)

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/TYP Final Tables and Figures Appendix/images/transfers_within_mun.png", plot = part, width = 6, height = 4)
