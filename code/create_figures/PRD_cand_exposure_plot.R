library(dplyr)
library(rdrobust)
library(fixest)
library(ggplot2)

load("~/mexico_mun/data/mexico_municipal_elections.Rdata")
load("C:/Users/adamd/Documents/mexico_mun/data/nearest_neighbor_PRD.Rdata")

mexico_municipal_elections$PRD_cand <- 0
mexico_municipal_elections$PRD_cand[mexico_municipal_elections$PRD > 0] <- 1

cand_index <- mexico_municipal_elections %>%
  filter(year <= 1997) %>%
  group_by(mun_id) %>%
  summarise(c_index = sum(PRD_cand))

nearest_neighbor_PRD <- merge(nearest_neighbor_PRD, cand_index, by.x = "neighbor", by.y = "mun_id")

cindex_PRD <- rdrobust(y = nearest_neighbor_PRD$change_pct_PRD, x = nearest_neighbor_PRD$PRD_margin, p = 1, 
                       covs = cbind(nearest_neighbor_PRD$main_year, nearest_neighbor_PRD$main_estado, 
                                    nearest_neighbor_PRD$dH, nearest_neighbor_PRD$treated_neighbors, 
                                    nearest_neighbor_PRD$c_index), 
                       bwselect = "cerrd", level = 90)
summary(cindex_PRD)


# Subset data and fit models
test1 <- subset(nearest_neighbor_PRD, c_index <= 1)
model1_90 <- rdrobust(y = test1$change_pct_PRD, x = test1$PRD_margin, p = 1, 
                   covs = cbind(test1$main_year, test1$main_estado, test1$dH, 
                                test1$treated_neighbors,  test1$c_index), 
                   bwselect = "cerrd", level = 90)
model1_95 <- rdrobust(y = test1$change_pct_PRD, x = test1$PRD_margin, p = 1, 
                      covs = cbind(test1$main_year, test1$main_estado, test1$dH, 
                                   test1$treated_neighbors,  test1$c_index), 
                      bwselect = "cerrd", level = 95)

test2 <- subset(nearest_neighbor_PRD, c_index > 1 & c_index <= 3)
model2_90 <- rdrobust(y = test2$change_pct_PRD, x = test2$PRD_margin, p = 1, 
                   covs = cbind(test2$main_year, test2$main_estado, 
                                test2$dH, test2$treated_neighbors,  test2$c_index), 
                   bwselect = "cerrd", level = 90)
model2_95 <- rdrobust(y = test2$change_pct_PRD, x = test2$PRD_margin, p = 1, 
                      covs = cbind(test2$main_year, test2$main_estado, 
                                   test2$dH, test2$treated_neighbors,  test2$c_index), 
                      bwselect = "cerrd", level = 95)

test3 <- subset(nearest_neighbor_PRD, c_index > 3)
model3_90 <- rdrobust(y = test3$change_pct_PRD, x = test3$PRD_margin, p = 1, 
                   covs = cbind(test3$main_year, test3$main_estado, 
                                test3$dH, test3$treated_neighbors,  test3$c_index), 
                   bwselect = "cerrd", level = 90)
model3_95 <- rdrobust(y = test3$change_pct_PRD, x = test3$PRD_margin, p = 1, 
                      covs = cbind(test3$main_year, test3$main_estado, 
                                   test3$dH, test3$treated_neighbors,  test3$c_index), 
                      bwselect = "cerrd", level = 95)

cindex_models <- matrix(NA, nrow = 3, ncol = 6)

# Extract coefficients
cindex_models[1, ] <- c(model1_90$coef[3], model1_90$ci[3, 1], model1_90$ci[3, 2], model1_95$ci[3,1], model1_95$ci[3,2], 1)
cindex_models[2, ] <- c(model2_90$coef[3], model2_90$ci[3, 1], model2_90$ci[3, 2], model2_95$ci[3,1], model2_95$ci[3,2], 2) 
cindex_models[3, ] <- c(model3_90$coef[3], model3_90$ci[3, 1], model3_90$ci[3, 2], model3_95$ci[3,1], model3_95$ci[3,2], 3) 


# Create a data frame for the plot
plot_data <- as.data.frame(cindex_models)
plot_data <- na.omit(plot_data)
colnames(plot_data) <- c("est", "ci_90low","ci_90high","ci_95low", "ci_95high", "n")

plot_data$n <- factor(plot_data$n, levels = c(1,2,3), labels = c("0-1", "2-3", "4-5"))

p <- ggplot(plot_data, aes(x = n, y = est)) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  # 95% CI - thinner error bars
  geom_errorbar(aes(ymin = ci_95low, ymax = ci_95high), 
                width = 0, linewidth = 0.5, position = position_dodge(width = 0.5)) +
  # 90% CI - fatter error bars  
  geom_errorbar(aes(ymin = ci_90low, ymax = ci_90high), 
                width = 0, linewidth = 3, alpha = 0.4, position = position_dodge(width = 0.5)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5)) +
  labs(x = "Number of Neighbors in Weighted Average", 
       y = "RD Estimate", 
       title = "", 
       subtitle = "",
       caption = "Thick bars: 90% CI, Thin bars: 95% CI") +
  theme_classic() +
  scale_color_viridis_d()

print(p)

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/TYP Final Tables and Figures Appendix/images/PRD_c_index_models.png", plot = p, width = 6, height = 4)

