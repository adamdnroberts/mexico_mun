library(ggplot2)
library(dplyr)
library(rdrobust)
library(xtable)

source("~/mexico_mun/code/function_create_rd_table.R") #for function create_model_table

load("C:/Users/adamd/Documents/mexico_mun/data/PRD_nn.Rdata")
load("C:/Users/adamd/Documents/mexico_mun/data/PAN_nn.Rdata")

#table PRD
nc_PRD <- rdrobust(y = PRD_nn$change_pp_PRD, x = PRD_nn$PRD_margin, p = 1, bwselect = "cerrd", level = 90)
summary(nc_PRD)

cerm_PRD <- rdrobust(y = PRD_nn$change_pp_PRD, x = PRD_nn$PRD_margin, p = 1, 
                     covs = cbind(PRD_nn$main_year, PRD_nn$main_estado, PRD_nn$dH), bwselect = "cerrd", level = 90)
summary(cerm_PRD)

PRD_nn$PRD_treated <- ifelse(PRD_nn$PRD_margin > 0, 1, 0)

test <- lm(change_pp_PRD ~ PRD_margin*PRD_treated*main_estado + main_year + dH, data = subset(PRD_nn, abs(PRD_margin) < 0.85))
summary(test)

coefs <- test$coefficients
filtered_coefs <- coefs[grepl("PRD_margin:PRD_treated:main_estado", names(coefs))]
test <- as.data.frame(filtered_coefs)
test$state <- gsub("PRD_margin:PRD_treated:main_estado", "", 
                   names(filtered_coefs)
                   )
test$coef <- test$filtered_coefs
test$`coefs[grepl("PRD_margin:PRD_treated:main_estado", names(coefs))]` <- NULL
rownames(test) <- NULL

test$michoacan <- as.factor(ifelse(test$state == "Michoacan", 1, 0))

# Assuming you have a data frame with columns 'state' and 'coef'
michoacan_graph <- ggplot(subset(test, !is.na(coef)), aes(x = reorder(state, coef), y = coef, fill = michoacan)) +
  geom_bar(stat = "identity") + #, fill = "steelblue") +
  theme_minimal() +
  labs(title = "Coefficients by State (Sorted)",
       x = "State",
       y = "Coefficient Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(michoacan_graph)

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/TYP_draft/images/Michoacan_bar_graph.png", plot = michoacan_graph, width = 6, height = 4)

