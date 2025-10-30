library(ggplot2)
library(dplyr)
library(viridis)
library(rdrobust)
library(patchwork)

data_dir <- "~/mexico_mun/data"
output_dir <- "C:/Users/adamd/Dropbox/Apps/Overleaf/TYP/images"

load(file.path(data_dir, "jaccard_similarity_AM_FM.Rdata"))
load(file.path(data_dir, "mexico_municipal_elections.Rdata"))
load(file.path(data_dir, "rdd_PRD_subset.Rdata"))
load(file.path(data_dir, "nearest_neighbor_PRD.Rdata"))

df_rdd_PRD2 <- rdd_PRD_subset %>%
  inner_join(
    js,
    by = join_by("mun_id" == "mun_id", "neighbor" == "ref_mun_id")
  ) %>%
  filter(
    main_estado == ref_estado,
    mun_id %in% unique(nearest_neighbor_PRD$mun_id)
  ) %>%
  mutate(
    change_pct_PRD = ref_PRD_pct - ref_next_PRD_pct,
    change_pct_PRI = ref_PRI_pct - ref_next_PRI_pct,
    change_pct_PAN = ref_PAN_pct - ref_next_PAN_pct,
    PRD_treat = as.numeric(PRD_margin > 0),
    above_median_js = as.numeric(js >= median(js)),
    PRD_treat_times_js = PRD_treat * js,
    dist_std = scale(dH)
  )

percentiles <- seq(0.5, 0.99, by = .01)
n_values <- 1:length(percentiles)

js_quantiles <- quantile(df_rdd_PRD$js, probs = seq(0.9, 0.99, 0.01))
cat("Jaccard Similarity for 90-99th percentiles:\n")
cat(sprintf("  %3d%%: %.4f\n", seq(90, 99, 1), js_quantiles))

robust_est_w_controls <- matrix(NA, nrow = length(n_values), ncol = 7)

# Loop through n values
for (n in n_values) {
  print(n)
  df_n <- df_rdd_PRD %>%
    filter(js > quantile(js, probs = percentiles[n])) %>%
    group_by(mun_id) %>%
    summarise(
      PRD_margin = mean(PRD_margin, na.rm = TRUE),
      weighted_avg_npp = sum(ref_next_PRD_pct * js) / sum(js),
      weighted_avg_pp = sum(ref_PRD_pct * js) / sum(js),
      main_year = first(main_year),
      main_estado = first(main_estado),
      avg_dH = mean(dH)
    )

  df_n <- df_n %>%
    mutate(change_pp_wt = weighted_avg_npp - weighted_avg_pp)

  df_n$main_estado <- as.factor(df_n$main_estado)
  df_n$main_year <- as.factor(df_n$main_year)

  m90 <- rdrobust(
    y = df_n$change_pp_wt,
    x = df_n$PRD_margin,
    p = 1,
    covs = cbind(df_n$main_year, df_n$main_estado, df_n$avg_dH),
    bwselect = "cerrd",
    level = 90
  )
  m95 <- rdrobust(
    y = df_n$change_pp_wt,
    x = df_n$PRD_margin,
    p = 1,
    covs = cbind(df_n$main_year, df_n$main_estado, df_n$avg_dH),
    bwselect = "cerrd",
    level = 95
  )

  robust_est_w_controls[n, ] <- c(
    m90$coef[3],
    m90$ci[3, 1],
    m90$ci[3, 2],
    m95$ci[3, 1],
    m95$ci[3, 2],
    percentiles[n],
    nrow(df_n)
  )
}

# Create a data frame for the plot
plot_data <- as.data.frame(robust_est_w_controls) %>%
  na.omit()

colnames(plot_data) <- c(
  "est",
  "ci_90low",
  "ci_90high",
  "ci_95low",
  "ci_95high",
  "percentile",
  "sample"
)

plot_data$percentile <- as.factor(plot_data$percentile)

# Find max values to rescale bar heights
max_est <- max(plot_data$est, na.rm = TRUE)
max_count <- max(plot_data$sample, na.rm = TRUE)

plot_for_paper <- ggplot(
  filter(plot_data, percentile %in% as.factor(seq(0.90, 0.99, .01))),
  aes(x = percentile)
) +
  # Bars for sample counts (rescaled to match est scale)
  geom_col(
    aes(y = sample / max_count * max_est),
    fill = "grey80",
    width = 0.8
  ) +

  # Error bars and points for estimates
  geom_errorbar(
    aes(ymin = ci_95low, ymax = ci_95high),
    width = 0,
    linewidth = 0.5,
    color = "black"
  ) +
  geom_errorbar(
    aes(ymin = ci_90low, ymax = ci_90high),
    width = 0,
    linewidth = 3,
    alpha = 0.4,
    color = "black"
  ) +
  geom_point(aes(y = est), size = 2.5, color = "black") +

  # Horizontal line
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +

  # Axes and labels
  scale_y_continuous(
    name = "Estimate",
    sec.axis = sec_axis(~ . / max_est * max_count, name = "Count")
  ) +
  labs(
    x = "Media Overlap Percentile",
    caption = "Thick bars: 90% CI, Thin bars: 95% CI"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90))
plot_for_paper

ggsave(
  filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/TYP/images/radio_estimate_plot.pdf",
  plot = plot_for_paper,
  width = 6,
  height = 4
)

plot_for_appendix <- ggplot(plot_data, aes(x = percentile)) +
  # Bars for sample counts (rescaled to match est scale)
  geom_col(
    aes(y = sample / max_count * max_est),
    fill = "grey80",
    width = 0.8
  ) +

  # Error bars and points for estimates
  geom_errorbar(
    aes(ymin = ci_95low, ymax = ci_95high),
    width = 0,
    linewidth = 0.5,
    color = "black"
  ) +
  geom_errorbar(
    aes(ymin = ci_90low, ymax = ci_90high),
    width = 0,
    linewidth = 3,
    alpha = 0.4,
    color = "black"
  ) +
  geom_point(aes(y = est), size = 2.5, color = "black") +

  # Horizontal line
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +

  # Axes and labels
  scale_y_continuous(
    name = "Estimate",
    sec.axis = sec_axis(~ . / max_est * max_count, name = "Count")
  ) +
  labs(
    x = "Media Overlap Percentile",
    caption = "Thick bars: 90% CI, Thin bars: 95% CI"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90))
plot_for_appendix

ggsave(
  filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/TYP Final Tables and Figures/images/radio_estimate_plot_50th.pdf",
  plot = plot_for_appendix,
  width = 6,
  height = 4
)
