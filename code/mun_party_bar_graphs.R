library(dplyr)
library(ggplot2)

load("~/mexico_mun/data/full_dataset_mexelec_pcts.Rdata")

load("~/mexico_mun/data/mexpop.Rdata")
pop_full <- mun_cen_final
pop <- subset(pop_full, select = c(mun_id, year, pop))
pop <- pop %>% mutate(pop = gsub(",", "", pop))
pop$pop <- as.numeric(pop$pop)
pop$year <- as.numeric(pop$year)

load("~/mexico_mun/data/full_dataset_mexbudget.Rdata")

bud <- subset(wide_budget_df, ANIO <= 1997)

budget <- subset(bud, select = c(mun_id, ANIO, Ingresos.Tema.Total.de.ingresos))
                 #, Ingresos.Capítulo.Participaciones.federales, Egresos.Capítulo.Deuda.pública,Ingresos.Concepto.Impuesto.sobre.los.Ingresos))
colnames(budget) <- c("mun_id","year", "income") #,"transfers","debt","taxes")

#THE MERGE#

merge1 <- merge(big_df, pop, by = c("mun_id","year"))
merge2 <- merge(merge1, budget, by = c("mun_id","year"), all.x = T)

merge2$party_win <- ifelse(merge2$PAN_pct > 0, "PAN", "PRI")
merge2$party_win <- ifelse(merge2$PRD_pct > 0, "PRD", merge2$party_win)

summary(merge2$party_win)

# Ensure the data is sorted by the relevant columns
merge2 <- merge2 %>%
  arrange(mun_id, year)

# Get the earliest value for each id
subset_merge2 <- merge2 %>%
  group_by(mun_id) %>%
  slice(1) %>%
  ungroup()

# MAKE POPULATION BAR GRAPHS

# Calculate means and confidence intervals
summary_data <- subset_merge2 %>%
  group_by(party_win) %>%
  summarise(
    mean_population = mean(pop, na.rm = T),
    sd_population = sd(pop, na.rm = T),
    n = n(),
    se = sd_population / sqrt(n),
    ci_lower = mean_population - qt(1 - (0.05 / 2), n - 1) * se,
    ci_upper = mean_population + qt(1 - (0.05 / 2), n - 1) * se
  )

mean_pop <- ggplot(subset(summary_data, !is.na(party_win)), aes(x = party_win, y = mean_population, fill = party_win)) +
  geom_bar(stat = "identity", color = "black") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  scale_fill_manual(values = c("PRI" = "gray", "PAN" = "lightblue", "PRD" = "gold")) +
  labs(title = "",
       x = "",
       y = "Mean Population") +
  theme_classic() +
  theme(legend.position = "none")
print(mean_pop)

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/TYP_draft/images/party_means_pop.png", plot = mean_pop, width = 6, height = 4)


# MAKE INCOME BAR GRAPHS
subset_merge2$income_m <- subset_merge2$income/1000000

summary_data <- subset_merge2 %>%
  group_by(party_win) %>%
  summarise(
    mean_income = mean(income_m, na.rm = T),
    sd_income = sd(income_m, na.rm = T),
    n = n(),
    se = sd_income / sqrt(n),
    ci_lower = mean_income - qt(1 - (0.05 / 2), n - 1) * se,
    ci_upper = mean_income + qt(1 - (0.05 / 2), n - 1) * se
  )

mean_income <- ggplot(subset(summary_data, !is.na(party_win)), aes(x = party_win, y = mean_income, fill = party_win)) +
  geom_bar(stat = "identity", color = "black") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  scale_fill_manual(values = c("PRI" = "gray", "PAN" = "lightblue", "PRD" = "gold")) +
  labs(title = "",
       x = "",
       y = "Mean Income (Millions Mex$)") +
  theme_classic() +
  theme(legend.position = "none")

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/TYP_draft/images/party_means_income.png", plot = mean_income, width =6, height = 4)
