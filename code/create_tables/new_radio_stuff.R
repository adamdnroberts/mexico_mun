library(car)
library(dplyr)
library(ggplot2)
library(fixest)
library(data.table)
library(gratia)
library(ggplot2)
library(mgcv)

load("~/mexico_mun/data/jaccard_similarity_AM_FM.Rdata")
load("~/mexico_mun/data/mexico_municipal_elections.Rdata")
load("~/mexico_mun/data/rdd_PRD_subset.Rdata")
load("C:/Users/adamd/Documents/mexico_mun/data/nearest_neighbor_PRD.Rdata")

new1 <- rdd_PRD_subset %>%
  inner_join(js, by = join_by("mun_id" == "mun_id", "neighbor" == "ref_mun_id"))
new2 <- nearest_neighbor_PRD %>%
  inner_join(js, by = join_by("mun_id" == "mun_id", "neighbor" == "ref_mun_id"))

new <- new1 %>% filter(main_estado == ref_estado)

muns <- unique(nearest_neighbor_PRD$mun_id)

new$change_pct_PRD <- new$ref_PRD_pct - new$ref_next_PRD_pct
new$change_pct_PRI <- new$ref_PRI_pct - new$ref_next_PRI_pct
new$change_pct_PAN <- new$ref_PAN_pct - new$ref_next_PAN_pct
new$PRD_treat <- as.numeric(new$PRD_margin > 0)
new$above_median_js <- ifelse(new$js >= median(new$js), 1, 0)
new$PRD_treat_times_js <- new$PRD_treat * new$js
new$dist_std <- scale(new$dH)

new <- new %>% filter(mun_id %in% muns)

test <- new %>%
  group_by(neighbor) %>%
  summarise(
    dist = mean(dH),
    PRD_treat_sum = sum(PRD_treat),
    PRD_treat_times_js = sum(PRD_treat * js),
    PRD_treat_times_js_dist = sum(PRD_treat * js * weight),
    change_pct_PRD = first(change_pct_PRD),
    mean_radio = mean(js),
    sum_js = sum(js),
    n_treatment_mun = n(),
    estado = first(estado)
  )
summary(test)

test$log_treatment_intensity <- log(test$PRD_treat_times_js + 1)

m1_gam <- gam(
  change_pct_PRD ~
    s(log_treatment_intensity, bs = "cr") + s(dist) + factor(estado) - 1,
  data = test,
  method = "REML"
)
summary(m1_gam)

library(boot)
# Define bootstrap function
boot_gam <- function(data, indices) {
  boot_data <- data[indices, ]
  model <- bam(
    change_pct_PRD ~ s(log_treatment_intensity) + s(dist) + factor(estado) - 1,
    data = boot_data
  )
  return(coef(model))
}
# Perform bootstrapping
set.seed(456)
boot_results <- boot(test, boot_gam, R = 10)
# Compute bootstrap standard errors
boot_se <- apply(boot_results$t, 2, sd)
# Display results
print(boot_se)
# Get smooth estimates for dist
smooth_df <- smooth_estimates(m1_gam, smooth = "s(log(PRD_treat_times_js + 1))")

plot(m1_gam)
# Compute both 90% and 95% CI
smooth_df <- smooth_df %>%
  mutate(
    se = .se,
    estimate = .estimate,
    lower_95 = estimate - 1.96 * se,
    upper_95 = estimate + 1.96 * se,
    lower_90 = estimate - 1.645 * se,
    upper_90 = estimate + 1.645 * se
  )

ggplot(smooth_df, aes(x = `log(PRD_treat_times_js + 1)`, y = estimate)) +
  geom_line(size = 1.1, color = "black") +
  geom_line(aes(y = lower_95), color = "black", linetype = "dotted") +
  geom_line(aes(y = upper_95), color = "black", linetype = "dotted") +
  geom_line(aes(y = lower_90), color = "black", linetype = "dashed") +
  geom_line(aes(y = upper_90), color = "black", linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  geom_rug(
    aes(x = log(PRD_treat_times_js + 1), y = NULL),
    sides = "b",
    alpha = 0.3,
    data = test
  ) + # rug at bottom
  labs(
    title = "Marginal Effect of Treatment Intensity",
    x = "Weighted Average of Treated Neighbors by Radio Similarity",
    y = "Partial effect on change_pct_PRD"
  ) +
  theme_minimal()

plot(ggeffects::ggpredict(m1_gam), facets = TRUE)
gratia::draw(m1_gam)
