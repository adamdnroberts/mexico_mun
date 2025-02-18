library(ggplot2)
library(dplyr)

news_full <- read.csv("~/mexico_mun/raw/0_impresos_directorio.csv", comment.char="#")

summary(news_full$impresos_fecha_fundacion)

#for some reason it's month/day/year, I think it converted to this because my excel is in that format?
news_full$date <- as.Date(news_full$impresos_fecha_fundacion, format = "%m/%d/%Y")

summary(news_full$date)
hist(news_full$date, breaks = 10)

news <- subset(news_full, date < as.Date("01/01/2000", format =  "%m/%d/%Y"))
summary(news$date)
hist(news$date, breaks = 20)

ggplot(data = news) +
  geom_histogram(aes(x = date)) +
  facet_wrap(~nom_ent)

news_summary <- news %>%
  group_by(nom_ent) %>%
  summarize(total_rows = n())
