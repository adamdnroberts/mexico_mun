library(car)
library(dplyr)
library(ggplot2)
library(fixest)
library(data.table)

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

#base model
m1 <- feols(
  change_pct_PRD ~ PRD_treat * above_median_js + dist_std | mun_id,
  cluster = "neighbor",
  data = new
)

m2 <- feols(
  change_pct_PRD ~
    PRD_treat *
      above_median_js +
      dist_std +
      ref_PRD_pct +
      ref_PRI_pct +
      ref_PAN_pct |
      mun_id,
  cluster = "neighbor",
  data = new
)

m3 <- feols(
  change_pct_PRI ~ PRD_treat * above_median_js + dist_std | mun_id,
  cluster = "neighbor",
  data = new
)

m4 <- feols(
  change_pct_PRI ~
    PRD_treat *
      above_median_js +
      dist_std +
      ref_PRD_pct +
      ref_PRI_pct +
      ref_PAN_pct |
      mun_id,
  cluster = "neighbor",
  data = new
)

m5 <- feols(
  change_pct_PAN ~ PRD_treat * above_median_js + dist_std | mun_id,
  cluster = "neighbor",
  data = new
)

m6 <- feols(
  change_pct_PAN ~
    PRD_treat *
      above_median_js +
      dist_std +
      ref_PRD_pct +
      ref_PRI_pct +
      ref_PAN_pct |
      mun_id,
  cluster = "neighbor",
  data = new
)

etable(m1, m2, m3, m4, m5, m6, digits = "r3", digits.stats = "r3", tex = F)

#BOOTSTRAP
rm(list = ls()[ls() != "new"])
gc()

B <- 50
n <- length(unique(new$mun_id))
treatment_muns <- unique(new$mun_id)

coefs <- matrix(NA, nrow = B, ncol = 6)

set.seed(123)

start_time <- Sys.time()

for (i in 1:B) {
  if (i %% round(B / 50) == 0) {
    print(paste0(round((i / B) * 100), "%"))
    print(Sys.time() - start_time)
  }
  df_list <- vector("list", n)
  sampled_treatment_muns = sample(treatment_muns, size = n, replace = T)
  df_list = lapply(sampled_treatment_muns, function(x) {
    new[new$mun_id == x, ]
  })
  df = do.call(rbind, df_list)
  m = feols(
    change_pct_PRD ~
      PRD_treat *
        above_median_js +
        dist_std +
        ref_PRD_pct +
        ref_PRI_pct +
        ref_PAN_pct |
        mun_id,
    lean = TRUE,
    data = df,
    notes = F
  )
  coefs[i, ] = m$coefficients
}

end_time <- Sys.time()
end_time - start_time # Returns time difference

bootstrap_plot <- as.data.frame(m2$coefficients[c(1, 6)])

bootstrap_plot$ci_95high <- c(
  quantile(coefs[, 1], 0.975),
  quantile(coefs[, 6], 0.975)
)
bootstrap_plot$ci_90high <- c(
  quantile(coefs[, 1], 0.95),
  quantile(coefs[, 6], 0.95)
)

bootstrap_plot$ci_95low <- c(
  quantile(coefs[, 1], 0.025),
  quantile(coefs[, 6], 0.025)
)
bootstrap_plot$ci_90low <- c(
  quantile(coefs[, 1], 0.05),
  quantile(coefs[, 6], 0.05)
)

bootstrap_plot$coef <- rownames(bootstrap_plot)
bootstrap_plot$est <- bootstrap_plot$`m2$coefficients`

p <- ggplot(bootstrap_plot, aes(x = coef, y = est)) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  # 95% CI - thinner error bars
  geom_errorbar(
    aes(ymin = ci_95low, ymax = ci_95high),
    width = 0,
    linewidth = 0.5
  ) +
  # 90% CI - fatter error bars
  geom_errorbar(
    aes(ymin = ci_90low, ymax = ci_90high),
    width = 0,
    linewidth = 3,
    alpha = 0.4
  ) +
  geom_point(size = 2.5) +
  labs(
    x = "",
    y = "Coefficient Estimate",
    title = "",
    subtitle = "",
    caption = "Thick bars: 90% CI, Thin bars: 95% CI"
  ) +
  theme_classic()
print(p)

library(tidyr)

test <- as.data.frame(coefs)
colnames(test) <- c("Radio", "x1", "x2", "x3", "x4", "Radio X Treatment")

test2 <- test %>%
  select(1, 6) %>%
  pivot_longer(everything(), names_to = "var", values_to = "value")

ggplot(test2) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  geom_boxplot(aes(x = var, y = value)) +
  theme_classic()
