library(dplyr)
library(ggplot2)

load("~/mexico_mun/data/mexico_municipal_elections.Rdata")

df <- mexico_municipal_elections %>% filter(year < 1999 & year >= 1990)
df$election_period <- NA
df$election_period[df$year > 1987 & df$year <= 1990] <- 1990
df$election_period[df$year > 1990 & df$year <= 1993] <- 1993
df$election_period[df$year > 1993 & df$year <= 1996] <- 1996
df$election_period[df$year > 1996 & df$year <= 1999] <- 1999

mun_governed_party <- df %>%
  group_by(election_period) %>%
  mutate(total_muns = n()) %>%
  filter(p1_name %in% c("PRI", "PAN", "PRD")) %>%
  group_by(election_period, p1_name, total_muns) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(percent = n / total_muns)

p <- ggplot(
  mun_governed_party,
  aes(x = election_period, y = percent, color = p1_name)
) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = unique(mun_governed_party$election_period)) +
  scale_color_grey(start = 0.2, end = 0.8) +
  labs(color = "Party", x = "Year", y = "% Municipalities Governed by Party") +
  theme_classic()

ggsave(
  filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/TYP/images/MunPartyLineGraph.pdf",
  plot = p,
  width = 6,
  height = 4
)
