library(ggplot2)
library(dplyr)
library(rdd)
library(viridis)
library(rdrobust)

load("~/mexico_mun/data/pairwise_km.Rdata")

load("~/mexico_RD/data/full_dataset_mexelec.Rdata")
big_df$mun_id <- gsub(" ", "", big_df$Municipio)

df <- subset(big_df, year>= 1995 & year <= 1997 
             #& estado!="Tlaxcala" 
             & (p1_name == "PRI" | p2_name == "PRI") & (p1_name == "PAN" | p2_name == "PAN")
)
#df$mun_id <- gsub(" ", "", df$Municipio)
df$PRD_pct <- df$PRD / (df$p1 + df$p2) # PRD percentage compared to top two
df$PRD_treat <- ifelse(df$PRD_pct > 0.5, 1, 0)

DCdensity(df$PAN_pct[df$year <= 1997], cutpoint = 0.5)

df_ref <- subset(big_df, year>= 1995 & year <= 1997 
                 #& estado!="Tlaxcala" 
                 #& (p1_name == "PRI" | p2_name == "PRI") & (p1_name == "PAN" | p2_name == "PAN")
)

#create smaller datasets for merge
ref_PAN <- subset(df_ref, select = c(year, mun_id, next_PAN_pct, PAN_pct, estado))
main_mun_PAN <- subset(df, select = c(year, mun_id, PAN_pct, estado))
DCdensity(ref_PAN$PAN_pct, cutpoint = 0.5)

#merge datasets using adjacent municipalities index
ref2 <- merge(dH_df,ref_PAN, by.x = c("neighbor"), by.y = c("mun_id"))
ref2 <- ref2 %>% rename(ref_PAN_pct = PAN_pct)

df_rdd <- merge(main_mun_PAN,ref2, by.x = c("mun_id"), by.y = c("mun"))
df_rdd <- df_rdd %>% 
  rename(main_year = year.x, ref_year = year.y, main_estado = estado.x, ref_estado = estado.y)

df_rdd$weight <- 1/df_rdd$dH

save(df_rdd, file = "C:/Users/adamd/Documents/mexico_mun/data/rdd_distance.Rdata")

## DISTANCES IN KM ##

# Pre-allocate the result matrix
robust_est <- matrix(NA, nrow = 100, ncol = 6)

# Vector of n values
km_values <- seq(5, 50, 5)

# Loop through n values
start_time <- Sys.time()
i <- 1
for (km in km_values) {
  df_km <- df_rdd %>%
    group_by(mun_id) %>%
    filter(dH <= km) %>%
    summarise(
      PAN_pct = mean(PAN_pct, na.rm = TRUE),
      weighted_avg_npp = sum(next_PAN_pct * weight) / sum(weight),
      weighted_avg_pp = sum(ref_PAN_pct * weight) / sum(weight)
      )
  
    df_km <- df_km %>%
      mutate(change_pp_wt = weighted_avg_npp - weighted_avg_pp)
  
    #md_rdr <- rdrobust(y = df_km$change_pp_wt, x = df_km$PAN_pct, c = 0.5, p = 1)
    md_rdr <- RDestimate(change_pp_wt ~ PAN_pct, cutpoint = 0.5, data = df_km)
    
    #bw1[i, ] <- c(md_rdr$coef[1], md_rdr$ci[1, 1], md_rdr$ci[1, 2], km)
    robust_est[i, ] <- c(md_rdr$est[3], md_rdr$ci[3, 1], md_rdr$ci[3, 2], md_rdr$est[3] - md_rdr$se[3]*1.65,  md_rdr$est[3] + md_rdr$se[3]*1.65, km)
    
    i = i+1
}
stop_time <- Sys.time()
print(stop_time - start_time)


# Create a data frame for the plot, 50 km = 96% of muns in df
robust_est <- as.data.frame(robust_est)
colnames(robust_est) <- c("est", "ci_lower","ci_upper","ci90low", "ci90high", "km")

ggplot(data = robust_est, aes(x = km, y = est)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.3, width = 0.2, color = "darkred") +
  geom_errorbar(aes(ymin = ci90low, ymax = ci90high), alpha = 0.9, width = 0.2, color = "gray") +
  #geom_hline(yintercept = 0, color = "darkgray") + 
  labs(x = "KM limit", y = "Estimate", title = "RD Robust Estimates, weighted average by km distance") +
  theme_minimal()

