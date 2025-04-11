library(tidyverse)
library(ggplot2)

load("~/mexico_mun/data/full_dataset_mexelec_pcts.Rdata")

big_df$mun_party <- "PRI"
big_df$mun_party[big_df$PRD_pct > 0] <- "PRD"
big_df$mun_party[big_df$PAN_pct > 0] <- "PAN"

mun_party <- subset(big_df, select = c(mun_id, year, mun_party))

# Define the range of years you want to fill in
all_years <- seq(min(mun_party$year), max(mun_party$year))

# Create a DataFrame with all combinations of mun_id and all_years
expanded_df <- expand_grid(mun_id = unique(mun_party$mun_id), year = all_years)

# Merge the original DataFrame with the expanded DataFrame
expanded_mun_party <- left_join(expanded_df, mun_party, by = c("mun_id", "year"))

# Fill missing values in the 'value' column if needed
expanded_mun_party <- expanded_mun_party %>%
  group_by(mun_id) %>%
  arrange(year) %>%
  fill(mun_party, .direction = "down")

percentage_df <- expanded_mun_party %>%
  filter(!is.na(mun_party)) %>%
  group_by(year, mun_party) %>%
  summarise(count = n()) %>%
  mutate(total = sum(count)) %>%
  mutate(percentage = (count / total) * 100) %>%
  select(year, mun_party, percentage) %>%
  pivot_wider(names_from = mun_party, values_from = percentage)

plot_df <- subset(percentage_df, year >= 1990 & year <= 2000)

ggplot(percentage_df, aes(x = year, y = percentage, color = mun_party)) +
  geom_line() +
  theme_classic()

