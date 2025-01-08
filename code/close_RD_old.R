library(dplyr)
library(ggplot2)
library(data.table)

puebla95 <- read.csv("C:/Users/adamd/Downloads/arc_ResultadosDefinitivos 1995 Aytos.csv", header=FALSE)

puebla95 <- puebla95[-c(1:7,9), ]

# Convert the first row to column names
colnames(puebla95) <- puebla95[1, ]

# Remove the first row
puebla95 <- puebla95[-1, ]

colnames(puebla95)[4] <- "p1_name"
colnames(puebla95)[15] <- "votos_valid"
colnames(puebla95)[19] <- "votos_total"
colnames(puebla95)[21] <- "ciudadanos"

puebla95 <- puebla95[, colnames(puebla95) != "NA"]

puebla95 <- puebla95[-1, ]

puebla95 <- puebla95 %>% filter(Dtto != "")

# Coerce to numeric
puebla95 <- puebla95 %>% mutate(across(4:15, ~ gsub(",", "", .)))
puebla95 <- puebla95 %>% mutate(across(4:15, ~ as.numeric(.)))

#make vector for puebla colnames

#find max vote winner
puebla95$p1 <- apply(puebla95[, 4:11], 1, max, na.rm = TRUE)

#find second max winner
# Step 1: Create a column for the second largest number in columns 4-11
puebla95$p2 <- apply(puebla95[, 4:11], 1, function(x) sort(x, decreasing = TRUE)[2])

# Step 2: Create a column for the column name of the second largest number
second_largest_colname <- function(row) {
  sorted_indices <- order(row, decreasing = TRUE)
  colnames(puebla95)[4:11][sorted_indices[2]]
}

puebla95$p2_name <- apply(puebla95[, 4:11], 1, second_largest_colname)

#if we remove where PRI is one of the parties
test <- subset(puebla95, p1_name == "PRI" | p2_name == "PRI")

test$PRI_pct <- test$PRI/(test$p1 + test$p2)

hist(test$PRI_pct, breaks = 50)
abline(v = 0.5, col = "red", lwd = 2)

#if we keep only when PRI and PAN are the parties
test2 <- subset(puebla95, (p1_name == "PRI" | p2_name == "PRI")&(p1_name == "PAN" | p2_name == "PAN"))

test2$PAN_pct <- test2$PAN/(test2$p1 + test2$p2)

hist(test2$PAN_pct, breaks = 30)
abline(v = 0.5, col = "red", lwd = 2)

#Puebla 98 city councils
p98 <- read.csv("C:/Users/adamd/Downloads/arc_ResultadosDefinitivos 1998 Aytos.csv", header=FALSE)

p98 <- puebla95[-c(1:7,9), ]

# Convert the first row to column names
colnames(puebla95) <- puebla95[1, ]

# Remove the first row
puebla95 <- puebla95[-1, ]

colnames(puebla95)[4] <- "p1_name"
colnames(puebla95)[15] <- "votos_valid"
colnames(puebla95)[19] <- "votos_total"
colnames(puebla95)[21] <- "ciudadanos"

puebla95 <- puebla95[, colnames(puebla95) != "NA"]

puebla95 <- puebla95[-1, ]

puebla95 <- puebla95 %>% filter(Dtto != "")

# Coerce to numeric
puebla95 <- puebla95 %>% mutate(across(4:15, ~ gsub(",", "", .)))
puebla95 <- puebla95 %>% mutate(across(4:15, ~ as.numeric(.)))

