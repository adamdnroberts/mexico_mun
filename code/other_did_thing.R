library(car)
library(dplyr)
library(ggplot2)
library(fixest)
library(data.table)

load("~/mexico_mun/data/mexico_municipal_elections.Rdata")
load("~/mexico_mun/data/pairwise_km.Rdata")

setDT(mexico_municipal_elections)

df <- mexico_municipal_elections[year <= 2000 & 
                                   (p1_name == "PRI" | p2_name == "PRI")]

ref_muns <- mexico_municipal_elections[year <= 2000]

treated_muns <- mexico_municipal_elections[year <= 1997 & PRD_treat == 1, 
                                           unique(mun_id)]
treated_muns_before <- mexico_municipal_elections[year <= 1994 & PRD_treat == 1, 
                                                  unique(mun_id)]

# Define column selections once
ref_cols <- c("year", "mun_id", "PRD_pct", 
              "PAN_pct", "estado", "PRD_margin", 
              "PRI_pct")

main_cols <- c("year", "mun_id", "PRD_pct", "PRI_pct", "PAN_pct", "PRD_margin", "estado")

# Create filtered datasets with proper exclusions
ref_PRD_not_treated <- ref_muns[!mun_id %in% treated_muns, ..ref_cols]
main_mun_PRD_not_treated <- df[!mun_id %in% treated_muns_before, ..main_cols]

ref_merged <- ref_PRD_not_treated %>% 
  left_join(dH_df, 
            by = join_by("mun_id" == "neighbor")) %>%
  rename(ref_mun_id = mun_id)

# Rename columns in bulk using setnames for data.table
old_names <- c("PRD_pct", "PAN_pct", "estado", "PRD_margin", "PRI_pct")

new_names <- c("ref_PRD_pct", "ref_PAN_pct", "ref_estado", "ref_PRD_margin", "ref_PRI_pct")

setnames(ref_merged, old_names, new_names)

# Final merge and variable creation
did <- main_mun_PRD_not_treated %>% inner_join(ref_merged, 
                        by = join_by("mun_id" == "mun", "year")) %>%
  filter(estado == ref_estado & estado != "Oaxaca")

#rm(list = ls()[ls() != "did"])

did$t <- NA
did$t[did$year == 1985] <- -4
did$t[did$year >= 1986 & did$year <= 1988] <- -3
did$t[did$year >= 1989 & did$year <= 1991] <- -2
did$t[did$year >= 1992 & did$year <= 1994] <- -1
did$t[did$year >= 1995 & did$year <= 1997] <- 0
did$t[did$year >= 1998 & did$year <= 2000] <- 1

did$t_factor <- as.factor(did$t)
did$t_factor <- relevel(did$t_factor, ref = "0")

library(stringr)
load("~/mexico_mun/data/jaccard_similarity_AM_FM.Rdata")

js$estado_js <- str_sub(js$mun_id, 1, 2)
js$ref_estado_js <- str_sub(js$ref_mun_id, 1, 2)

js <- js %>% filter(estado_js == ref_estado_js 
                    & mun_id %in% unique(did$mun_id) 
                    & ref_mun_id %in% unique(did$ref_mun_id))

new <- did %>% left_join(js, by = join_by("mun_id", "ref_mun_id"))

treated <- unique(new$mun_id[new$PRD_margin > 0])
new$PRD_treat <- as.numeric(new$mun_id %in% treated)

new$dist_std <- scale(new$dH)
new$pair_id <- paste(new$mun_id, new$ref_mun_id)

#base model
m1 <- feols(ref_PRD_pct ~ PRD_treat*js*t_factor + dH + PRD_margin
            | pair_id + year,
            cluster = "ref_mun_id",
            data = new)

etable(m1, digits = "r3")

m2 <- feols(ref_PRD_pct ~ PRD_treat*js*t_factor + dH + PRD_margin
            | pair_id + year,
            cluster = "ref_mun_id",
            data = subset(new, t >= 0))

etable(m2, digits = "r3")

m2 <- feols(change_pct_PRD ~ PRD_treat*js + dist_std
            + ref_PRD_pct + ref_PRI_pct + ref_PAN_pct
            | mun_id,
            cluster = "neighbor",
            data = new)

m3 <- feols(change_pct_PRI ~ PRD_treat*js + dist_std
            | mun_id,
            cluster = "neighbor",
            data = new)

m4 <- feols(change_pct_PRI ~ PRD_treat*js + dist_std
            + ref_PRD_pct + ref_PRI_pct + ref_PAN_pct
            | mun_id,
            cluster = "neighbor",
            data = new)

m5 <- feols(change_pct_PAN ~ PRD_treat*js + dH 
            | mun_id,
            cluster = "neighbor",
            data = new)

m6 <- feols(change_pct_PAN ~ PRD_treat*js + dH 
            + ref_PRD_pct + ref_PRI_pct + ref_PAN_pct
            | mun_id,
            cluster = "neighbor",
            data = new)

etable(m1, m2, m3, m4, m5, m6, digits = "r3")

linearHypothesis(m2, "js = PRD_treat:js")

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
  if (i %% round(B/50) == 0) {
    print(paste0(round((i/B)*100),"%"))
    print(Sys.time() - start_time)
  }
  df_list <- vector("list", n)
  sampled_treatment_muns = sample(treatment_muns, size = n, replace = T)
  df_list = lapply(sampled_treatment_muns, function(x) {
    new[new$mun_id == x, ]
  })
  df = do.call(rbind, df_list)
  m = feols(change_pct_PRD ~ PRD_treat*js + dist_std 
            + ref_PRD_pct + ref_PRI_pct + ref_PAN_pct
            | mun_id,
            lean = TRUE,
            data = df,
            notes = F)
  coefs[i,] = m$coefficients
}

end_time <- Sys.time()
end_time - start_time  # Returns time difference

bootstrap_plot <- as.data.frame(m2$coefficients[c(1,6)])

bootstrap_plot$ci_95high <- c(quantile(coefs[,1], 0.975),quantile(coefs[,6], 0.975))
bootstrap_plot$ci_90high <- c(quantile(coefs[,1], 0.95),quantile(coefs[,6], 0.95))

bootstrap_plot$ci_95low <- c(quantile(coefs[,1], 0.025),quantile(coefs[,6], 0.025))
bootstrap_plot$ci_90low <- c(quantile(coefs[,1], 0.05),quantile(coefs[,6], 0.05))

bootstrap_plot$coef <- rownames(bootstrap_plot)
bootstrap_plot$est <- bootstrap_plot$`m2$coefficients`

p <- ggplot(bootstrap_plot, aes(x = coef, y = est)) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  # 95% CI - thinner error bars
  geom_errorbar(aes(ymin = ci_95low, ymax = ci_95high), 
                width = 0, linewidth = 0.5) +
  # 90% CI - fatter error bars  
  geom_errorbar(aes(ymin = ci_90low, ymax = ci_90high), 
                width = 0, linewidth = 3, alpha = 0.4) +
  geom_point(size = 2.5) +
  labs(x = "", 
       y = "Coefficient Estimate", 
       title = "", 
       subtitle = "",
       caption = "Thick bars: 90% CI, Thin bars: 95% CI") +
  theme_classic()
print(p)

library(tidyr)

test <- as.data.frame(coefs)
colnames(test) <- c("Radio", "x1", "x2", "x3", "x4", "Radio X Treatment")

test2 <- test %>%
  select(1,6) %>%
  pivot_longer(everything() ,names_to = "var", values_to = "value")

ggplot(test2) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  geom_boxplot(aes(x = var, y = value)) +
  theme_classic()
