library(car)
library(dplyr)
library(ggplot2)
library(fixest)
library(data.table)
library(rdrobust)

load("~/mexico_mun/data/jaccard_similarity_AM_FM.Rdata")
load("C:/Users/adamd/Documents/mexico_mun/data/nearest_neighbor_PRD.Rdata")

test <- nearest_neighbor_PRD %>%
  left_join(js, join_by(mun_id, neighbor == ref_mun_id))

test$PRD_treat <- as.numeric(test$PRD_margin > 0)
test$above_median_radio = as.numeric(test$js > median(test$js, na.rm = T))

# Bandwidth
h <- 0.089

# Filter data within the bandwidth
df <- test %>%
  filter(abs(PRD_margin) <= h) %>%
  mutate(
    weight_tri = 1 - abs(PRD_margin) / h
  )


# Run weighted regression
m <- lm(
  change_pct_PRD ~
    PRD_margin +
      PRD_treat * js +
      main_estado +
      main_year +
      dH +
      treated_neighbors,
  data = df,
  weights = weight_tri
)

summary(m)

test$interaction <- test$PRD_treat * test$js

#PRD models
prd_non_covariate_model <- rdrobust(
  y = test$change_pct_PRD,
  x = test$PRD_margin,
  p = 1,
  covs = cbind(test$js, test$interaction),
  bwselect = "cerrd"
)
summary(prd_non_covariate_model)
prd_non_covariate_model$beta_covs

prd_covariate_model <- rdrobust(
  y = test$change_pct_PRD,
  x = test$PRD_margin,
  p = 1,
  covs = cbind(
    test$main_year,
    test$main_estado,
    test$dH,
    test$treated_neighbors,
    test$js,
    test$interaction
  ),
  bwselect = "cerrd"
)
prd_covariate_model$beta_covs

test <- test %>%
  mutate(js_quartile = ntile(js, 4))

library(rdrobust)

# Create an empty list to store models
rd_models <- list()

# Loop over quartiles
for (q in 1:4) {
  print(q)
  df_q <- filter(test, js_quartile == q)

  rd_models[[q]] <- rdrobust(
    y = df_q$change_pct_PRD,
    x = df_q$PRD_margin,
    p = 1,
    kernel = "triangular",
    bwselect = "cerrd"
  )
}

# Inspect results for quartile 1
summary(rd_models[[1]])

# Quartile 2, 3, 4 similarly
summary(rd_models[[2]])
summary(rd_models[[3]])
summary(rd_models[[4]])

test <- test %>%
  mutate(
    js_bin = case_when(
      js <= -0.25 ~ "1",
      js > -0.25 & js <= 0.5 ~ "2",
      js > 0.5 & js <= 0.75 ~ "3",
      js > 0.75 ~ "4"
    )
  )

# Identify bins
bins <- unique(test$js_bin)
bins <- bins[!is.na(bins)] # drop NAs if any

rd_models <- list()

for (b in bins) {
  df_b <- filter(test, js_bin == b)

  rd_models[[b]] <- rdrobust(
    y = df_b$change_pct_PRD,
    x = df_b$PRD_margin,
    p = 1,
    kernel = "triangular",
    bwselect = "cerrd"
  )
}

# Example: look at the model for js < -0.25
summary(rd_models[["2"]])
summary(rd_models[["3"]])
summary(rd_models[["4"]])
