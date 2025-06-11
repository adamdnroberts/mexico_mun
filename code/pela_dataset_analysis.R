library(dplyr)
library(ggplot2)
library(tidyr)

pela_full <- read.delim("~/mexico_mun/raw/PELA_1997_from_hawkins.txt")

pela_mex <- subset(pela_full, Country == "MEXICO" 
                   & (Statistic == "Mean (excluding missing values)" | Statistic == "Standard deviation (excluding missing values)" 
                      | Statistic == "Number of non-missing cases")
                   )

pela_mex_mean <- subset(pela_full, Country == "MEXICO" 
                   & (Statistic == "Mean (excluding missing values)")
)

pela_mex[4:ncol(pela_mex)] <- lapply(pela_mex[4:ncol(pela_mex)], as.numeric)
pela_mex_mean[4:ncol(pela_mex_mean)] <- lapply(pela_mex_mean[4:ncol(pela_mex_mean)], as.numeric)

test <- pela_mex %>% pivot_wider(cols_id = 1:3, names_from = Statistic)

test <- pela_mex %>%
  group_by(Party.Abbreviation) %>%
  

ggplot(pela_mex_mean) +
  geom_bar(aes(x = reorder(Party.Abbreviation, Democracy.always.best), y = Democracy.always.best), stat = "identity")

# Reshape data from wide to long format
pela_mex_long <- pela_mex %>%
  pivot_longer(cols = 4:ncol(.), 
               names_to = "variable", 
               values_to = "value")

pela_mex_graph <- pela_mex_long %>%
  pivot_wider(names_from = 3, values_from = value)

pela_mex_graph$se <- pela_mex_graph$`Standard deviation (excluding missing values)`/sqrt(pela_mex_graph$`Number of non-missing cases`)
pela_mex_graph$upci <- pela_mex_graph$`Mean (excluding missing values)` + 1.645*pela_mex_graph$se
pela_mex_graph$lowci <- pela_mex_graph$`Mean (excluding missing values)` - 1.645*pela_mex_graph$se

pela_mex_graph$mean <- pela_mex_graph$`Mean (excluding missing values)`

# Create grid of plots
ggplot(pela_mex_graph) +
  geom_bar(aes(x = reorder(Party.Abbreviation, mean), 
               y = mean), 
           stat = "identity") +
  geom_errorbar(aes(x = reorder(Party.Abbreviation, mean),
                    ymin = lowci,
                    ymax = upci),
                width = 0.2) +
  facet_wrap(~ variable, scales = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

