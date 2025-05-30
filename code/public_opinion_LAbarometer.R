library(haven)
library(ggplot2)
library(dplyr)

lab95 <- read_dta("C:/Users/adamd/Downloads/F00004296-Latinobarometro_1995_dta/Latinobarometro_1995_data_english_v2014_06_27.dta")
lab96 <- read_dta("C:/Users/adamd/Downloads/F00004293-Latinobarometro_1996_dta/Latinobarometro_1996_datos_english_v2014_06_27.dta")
lab97 <- read_dta("C:/Users/adamd/Downloads/F00004299-Latinobarometro_1997_dta/Latinobarometro_1997_datos_english_v2014_06_27.dta")
lab97$pais <- lab97$idenpa
lab97$numero <- lab97$numinves

#lab <- bind_rows(lab95, lab96)
lab <- lab95

mexlab <- subset(lab, pais == 484)

hist(mexlab$p32)
hist(mexlab$p33[mexlab$p33>100])
hist(mexlab$p33b)

unique(mexlab$p33)
unique(mexlab$p33b)
unique(mexlab$p32)

#party codes
#PAN = 484001, PRI = 484002, PRD = 484003

mexlab$PAN <- ifelse(mexlab$p33 == 484001, 1, 0)
mexlab$PRI <- ifelse(mexlab$p33 == 484002, 1, 0)
mexlab$PRD <- ifelse(mexlab$p33 == 484003, 1, 0)

mexlab$party <- NA
mexlab$party[mexlab$PAN == 1] <- "PAN"
mexlab$party[mexlab$PRI == 1] <- "PRI"
mexlab$party[mexlab$PRD == 1] <- "PRD"

#left\right ideology
ggplot(subset(mexlab, p31 >=0 & !is.na(party))) +
  geom_density(aes(x = p31, color = party), alpha = 0.2, linewidth = 0.75) +
  labs(x = "Left/Right Ideology") +
  theme_classic()

ggplot(subset(mexlab, p31 >= 0 & !is.na(party))) +
  geom_boxplot(aes(x = party, y = p31), alpha = 0.2) +
  #scale_color_manual(values = c("PAN" = "black", "PRI" = "gray50", "PRD" = "gray80")) +
  labs(title = "Ideology of Voters by Party, 1995", x = "", y = "Left/Right Ideology") +
  theme_classic()

summary(mexlab$p31[mexlab$party == "PAN" & mexlab$p31 > 0])
summary(mexlab$p31[mexlab$party == "PRI" & mexlab$p31 > 0])
summary(mexlab$p31[mexlab$party == "PRD" & mexlab$p31 > 0])

var(mexlab$p31[mexlab$party == "PAN" & mexlab$p31 > 0], na.rm = T)
var(mexlab$p31[mexlab$party == "PRI" & mexlab$p31 > 0], na.rm = T)
var(mexlab$p31[mexlab$party == "PRD" & mexlab$p31 > 0], na.rm = T)

#democracy
ggplot(subset(mexlab, p20 >=0)) +
  geom_density(aes(x = p20, color = party), alpha = 0.2) +
  theme_classic()

ggplot(subset(mexlab, p31 >= 0)) +
  geom_point(aes(x = p31, y = p20), alpha = 0.01) +
  facet_wrap(~party)

mexlab$dem_preferred <- ifelse(mexlab$p20 == 1, 1, 0)
mexlab$dem_preferred[mexlab$p20 < 0] <- NA
summary(mexlab$dem_preferred)
summary(mexlab$dem_preferred[mexlab$party == "PRI"])

mexlab$dem_satisfied <- ifelse(mexlab$p21 > 0, mexlab$p21, NA)
summary(mexlab$dem_satisfied)
summary(mexlab$dem_satisfied[mexlab$party == "PRI"])

ggplot(subset(mexlab, party == "PRI")) +
  geom_bar(aes(x = p20))


#1998
lab98 <- read_dta("C:/Users/adamd/Downloads/F00004302-Latinobarometro_1998_dta/Latinobarometro_1998_datos_english_v2014_06_27.dta")

#lab <- bind_rows(lab95, lab96)

mexlab98 <- subset(lab98, idenpa == 484)

#party codes
#PAN = 484001, PRI = 484002, PRD = 484003

mexlab98$PAN <- ifelse(mexlab98$sp53 == 484001, 1, 0)
mexlab98$PRI <- ifelse(mexlab98$sp53 == 484002, 1, 0)
mexlab98$PRD <- ifelse(mexlab98$sp53 == 484003, 1, 0)

mexlab98$party <- NA
mexlab98$party[mexlab98$PAN == 1] <- "PAN"
mexlab98$party[mexlab98$PRI == 1] <- "PRI"
mexlab98$party[mexlab98$PRD == 1] <- "PRD"

summary(mexlab98$sp52[mexlab98$party == "PAN" & mexlab98$sp52 > 0])
summary(mexlab98$sp52[mexlab98$party == "PRI" & mexlab98$sp52 > 0])
summary(mexlab98$sp52[mexlab98$party == "PRD" & mexlab98$sp52 > 0])

#left\right ideology
ggplot(subset(mexlab98, sp52 >=0 & !is.na(party))) +
  geom_density(aes(x = sp52, color = party), alpha = 0.2, linewidth = 0.75) +
  labs(x = "Left/Right Ideology") +
  theme_classic()

ggplot(subset(mexlab98, sp52 >= 0 & !is.na(party))) +
  geom_boxplot(aes(x = party, y = sp52), alpha = 0.2) +
  #scale_color_manual(values = c("PAN" = "black", "PRI" = "gray50", "PRD" = "gray80")) +
  labs(title = "Ideology of Voters by Party, 1998", x = "", y = "Left/Right Ideology") +
  theme_classic()

#difference in means
mexlab98$left_right <- ifelse(mexlab98$sp52 >= 0, mexlab98$sp52, NA)

mexlab98$prd_voter_ideology <- mexlab98$left_right*mexlab98$PRD
mexlab98$pan_voter_ideology <- mexlab98$left_right*mexlab98$PAN
mexlab98$pri_voter_ideology <- mexlab98$left_right*mexlab98$PRI


t.test(mexlab98$prd_voter_ideology, mexlab98$pan_voter_ideology)


#no party info?
lab00 <- read_dta("C:/Users/adamd/Downloads/F00004305-Latinobarometro_2000_dta/Latinobarometro_2000_datos_eng_v2014_06_27.dta")

mexlab00 <- subset(lab00, idenpa == 484)

mexlab00$PAN <- ifelse(mexlab00$P54ST == 484001, 1, 0)
mexlab00$PRI <- ifelse(mexlab00$P54ST == 484002, 1, 0)
mexlab00$PRD <- ifelse(mexlab00$P54ST == 484003, 1, 0)

mexlab00$party <- ifelse(mexlab00$PRD == 0, ifelse(mexlab00$PAN == 1, "PAN", "PRI"), "PRD")

ggplot(subset(mexlab00, P52ST >= 0)) +
  geom_boxplot(aes(x = party, y = P52ST), alpha = 0.2) +
  #scale_color_manual(values = c("PAN" = "black", "PRI" = "gray50", "PRD" = "gray80")) +
  labs(title = "Ideology of Voters by Party, 1995", x = "", y = "Left/Right Ideology") +
  theme_classic()
