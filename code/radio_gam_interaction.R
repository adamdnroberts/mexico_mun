#PLOT

library(mgcv)

# De-mean the dependent variable and predictors by mun_id
new2 <- new %>%
  group_by(mun_id) %>%
  mutate(
    change_pct_PRD_demeaned = change_pct_PRD - mean(change_pct_PRD, na.rm = TRUE),
    PRD_treat_demeaned = PRD_treat - mean(PRD_treat, na.rm = TRUE),
    js_demeaned = js - mean(js, na.rm = TRUE),
    PRD_treat_js_demeaned = PRD_treat_times_js - mean(PRD_treat_times_js, na.rm = TRUE),
    dist_std_demeaned = dist_std - mean(dist_std, na.rm = TRUE),
    ref_PRD_pct_demeaned = ref_PRD_pct - mean(ref_PRD_pct, na.rm = TRUE),
    ref_PRI_pct_demeaned = ref_PRI_pct - mean(ref_PRI_pct, na.rm = TRUE),
    ref_PAN_pct_demeaned = ref_PAN_pct - mean(ref_PAN_pct, na.rm = TRUE)
  ) %>%
  ungroup()

# Fit the GAM model
t1 <- Sys.time()
g <- gam(change_pct_PRD_demeaned ~ s(PRD_treat_js_demeaned) +
           dist_std_demeaned + ref_PRD_pct_demeaned +
           ref_PRI_pct_demeaned + ref_PAN_pct_demeaned,
         data = new2, method = "REML")
t2 <- Sys.time()
t2-t1
# Summary of the model
summary(g)

t1 <- Sys.time()
js_sequence <- seq(min(new2$PRD_treat_js_demeaned, na.rm = TRUE), max(new2$PRD_treat_js_demeaned, na.rm = TRUE), length.out = 10)
g.yh1=predict(g, newdata = data.frame(PRD_treat_js_demeaned = js_sequence,
                                      dist_std_demeaned = rep(median(new2$dist_std_demeaned),10),
                                      ref_PRD_pct_demeaned = rep(median(new2$ref_PRD_pct_demeaned, na.rm = T),10),
                                      ref_PRI_pct_demeaned = rep(median(new2$ref_PRI_pct_demeaned, na.rm = T),10),
                                      ref_PAN_pct_demeaned = rep(median(new2$ref_PAN_pct_demeaned, na.rm = T),10)),
              type = "response")
g.yh0=predict(g, newdata = data.frame(PRD_treat_js_demeaned = rep(0,10),
                                      dist_std_demeaned = rep(median(new2$dist_std_demeaned),10),
                                      ref_PRD_pct_demeaned = rep(median(new2$ref_PRD_pct_demeaned, na.rm = T),10),
                                      ref_PRI_pct_demeaned = rep(median(new2$ref_PRI_pct_demeaned, na.rm = T),10),
                                      ref_PAN_pct_demeaned = rep(median(new2$ref_PAN_pct_demeaned, na.rm = T),10)),
              type = "response")
t2 <- Sys.time()
t2 - t1
g.dy=(g.yh1 - g.yh0)
summary(g.dy)

#linear marginal effect
ols <- feols(change_pct_PRD_demeaned ~ PRD_treat_js_demeaned +
           dist_std_demeaned + ref_PRD_pct_demeaned +
           ref_PRI_pct_demeaned + ref_PAN_pct_demeaned,
         data = new2)

# Extract coefficients
coef_interaction <- coef(ols)["PRD_treat_js_demeaned"]

# Calculate the marginal effect
marginal_effect <- coef_interaction * js_sequence

# Plot the marginal effect
col.gam='chartreuse4'
plot(js_sequence,g.dy,type='l',col=col.gam,lty=4,lwd=3, 
     xlab = "js", ylab = "Marginal Effect of PRD_treat",
     main = "Marginal Effect of PRD_treat as a Function of js")
points(js_sequence, marginal_effect, type = "l", col = "blue")
abline(h = 0, lty = 2, col = "red")  # Add a horizontal line at 0
