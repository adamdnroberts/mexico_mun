library(ggplot2)
library(dplyr)
library(fixest)

#options(scipen = 999)

mex0 <- read.delim("C:/Users/adamd/Downloads/spending_irregularities.tab") #data: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/Y9CCEE

ggplot(data = mex0, aes(x = year, y= irregularities, color = as.factor(state))) +
  geom_point(alpha = 0.5) +
  #geom_smooth(aes(x = year, y = irregularities)) +
  facet_wrap(~state) + 
  labs(title = "Irregularities in Mexican Municipal Governments, by State") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data = mex0, aes(x = year, y= irregularities)) +
  geom_point(alpha = 0.5) +
  geom_smooth(aes(x = year, y = irregularities)) +
  facet_wrap(~party) + 
  labs(title = "Irregularities in Mexican Municipal Governments, by State") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data = test, aes(x = year, y= irregularities, color = as.factor(state))) +
  geom_point(alpha = 0.5) +
  #geom_smooth(aes(x = year, y = irregularities)) +
  facet_wrap(~state) + 
  labs(title = "Irregularities in Mexican Municipal Governments, by State") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

state_avg <- mex0 %>% group_by(state, year) %>%
  summarise(state = first(state), year = first(year), state_irreg = mean(irregularities))

mex <- merge(x = mex0, y = state_avg, by = c("state", "year"), all.x = TRUE)
mex$beta <- mex$irregularities - mex$state_irreg

m <- feols(log(margin) ~ irregularities + beta + lpop + incumbent + HDI + incumbent*irregularities| state+year+party, data=mex)
etable(m, tex=F)
summary(m)

plot(fixef(m))

conference_links <- read.csv("C:/Users/adamd/Downloads/conference_links.csv")
conference_links$time <- paste0(conference_links$month,conference_links$year)

new <- conference_links %>% group_by(time) %>% summarize(count = n(), month = as.integer(first(month)), year = as.integer(first(year)))
