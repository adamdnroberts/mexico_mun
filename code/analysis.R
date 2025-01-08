library(stargazer)
library(fixest)

#simple linear analysis
average_values$PAN_win <- ifelse(average_values$main_PAN_pct >= 0.5,1,0)

m <- lm(avg_next_PAN_pct ~ main_PAN_pct + estado.x, data = average_values)
m2 <- lm(avg_next_PAN_pct ~ PAN_win + estado.x, data = average_values)
stargazer(m,m2, type = "text")

fe <- feols(avg_next_PAN_pct ~ PAN_pct | estado.x, data = average_values)
fe2 <- feols(avg_next_PAN_pct ~ PAN_win | estado.x, data = average_values)
fe3 <- feols(avg_next_PAN_pct ~ PAN_pct*avg_d | estado.x, data = average_values)
fe4 <- feols(avg_next_PAN_pct ~ PAN_win*avg_d | estado.x, data = average_values)
etable(fe,fe2,fe3,fe4)

fe <- feols(avg_next_PAN_pct ~ main_PAN_pct + avg_vec_PAN_pct | estado.x, data = average_values)
fe2 <- feols(avg_next_PAN_pct ~ PAN_win + avg_vec_PAN_pct | estado.x, data = average_values)
fe3 <- feols((avg_next_PAN_pct - avg_vec_PAN_pct) ~ main_PAN_pct | estado.x, data = average_values)
fe4 <- feols((avg_next_PAN_pct - avg_vec_PAN_pct) ~ PAN_win | estado.x, data = average_values)
etable(fe,fe2,fe3,fe4)

library(rdd)

rd1 <- RDestimate(avg_next_PAN_pct ~ main_PAN_pct, cutpoint = 0.5, data = average_values)
summary(rd1)

DCdensity(average_values$main_PAN_pct, cutpoint = 0.50)
title(x = "PAN vote share")

placebo <- RDestimate(avg_next_PAN_pct ~ main_PAN_pct, cutpoint = 0.45, data = average_values)
summary(placebo)

placebo <- RDestimate(avg_next_PAN_pct ~ main_PAN_pct, cutpoint = 0.51, data = average_values)
summary(placebo)

plot(placebo)

DCdensity(average_values$main_PAN_pct, cutpoint = 0.55)
title(x = "PAN vote share")
