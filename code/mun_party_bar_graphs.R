library(dplyr)
library(ggplot2)
library(xtable)

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

## Average Distance Bar Graph

load("~/mexico_mun/data/rdd_distance_PRD.Rdata")

df_rdd_PRD <- subset(df_rdd_PRD, ref_PRD_wins == 0 & main_estado == ref_estado)

avg_distances <- matrix(NA, nrow = 5, ncol = 6)

n_values <- 1:5

for (n in n_values) {
  print(n)
  df_n <- df_rdd_PRD %>%
    group_by(mun_id) %>%
    arrange(dH) %>%
    slice(n)
  
  se = sd(df_n$dH) / sqrt(n)
  ci_lower = mean(df_n$dH) - 1.645 * se
  ci_upper = mean(df_n$dH) + 1.645 * se
  
  avg_distances[n, ] <- c(n, min(df_n$dH), median(df_n$dH), max(df_n$dH), mean(df_n$dH), sd(df_n$dH)) 
  
}

test <- as.data.frame(avg_distances)
colnames(test) <- c("closest", "min", "median", "max", "mean", "sd")

xtable(test)

dist_graphs <- df_rdd_PRD %>%
  group_by(mun_id) %>%
  slice_head(n = 5) %>%
  mutate(rank = rank(dH, ties.method = "first")) %>%
  mutate(close = case_when(
    rank == 1 ~ 1,
    rank == 2 ~ 2,
    rank == 3 ~ 3,
    rank == 4 ~ 4,
    rank == 5 ~ 5,
    TRUE ~ 0
  )) %>%
  select(-rank) %>%
  ungroup()

PRD_dist <- ggplot(subset(dist_graphs, dH <= 100), aes(x = dH, fill = as.factor(close))) +
  geom_density(alpha = .5) +
  labs(title = "",
       x = "Distance (km)",
       y = "Density",
       fill = "Nearest") +  # Add a label for the legend
  theme_classic() +
  scale_fill_grey(start = 0.2, end = 0.8) +  # Apply grayscale colors
  theme(legend.position = c(0.8,0.8)) 

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/TYP_draft/images/PRD_dist_distribution.png", plot = PRD_dist, width =6, height = 4)



#Now, do the same for PAN

load("~/mexico_mun/data/rdd_distance_PAN.Rdata")

df_rdd_PAN <- subset(df_rdd_PAN, ref_PAN_wins == 0 & main_estado == ref_estado)

avg_distances <- matrix(NA, nrow = 5, ncol = 6)

n_values <- 1:5

for (n in n_values) {
  print(n)
  df_n <- df_rdd_PAN %>%
    group_by(mun_id) %>%
    arrange(dH) %>%
    slice(n)
  
  se = sd(df_n$dH) / sqrt(n)
  ci_lower = mean(df_n$dH) - 1.645 * se
  ci_upper = mean(df_n$dH) + 1.645 * se
  
  avg_distances[n, ] <- c(n, min(df_n$dH), median(df_n$dH), max(df_n$dH), mean(df_n$dH), sd(df_n$dH)) 
  
}

test <- as.data.frame(avg_distances)
colnames(test) <- c("closest", "min", "median", "max", "mean", "sd")

xtable(test)

dist_graphs <- df_rdd_PAN %>%
  group_by(mun_id) %>%
  slice_head(n = 5) %>%
  mutate(rank = rank(dH, ties.method = "first")) %>%
  mutate(close = case_when(
    rank == 1 ~ 1,
    rank == 2 ~ 2,
    rank == 3 ~ 3,
    rank == 4 ~ 4,
    rank == 5 ~ 5,
    TRUE ~ 0
  )) %>%
  select(-rank) %>%
  ungroup()

ggplot(subset(dist_graphs, dH <= 100), aes(x = dH, fill = as.factor(close))) +
  geom_density(alpha = 0.4) +
  labs(title = "",
       x = "Distance (km)",
       y = "Nearest nth Neighbor") + 
  theme_classic() +
  scale_fill_grey(start = 0.2, end = 0.8) +  # Apply grayscale colors
  theme(legend.position = c(0.8,0.8)) 

summary(dist_graphs$dH)
