library(ggplot2)
library(dplyr)
library(rdd)
library(fixest)

load("~/mexico_RD/near.Rdata")

load("~/mexico_RD/full_dataset_mexelec.Rdata")

df <- subset(big_df, year>= 1995 & year <= 1997 & estado!="Tlaxcala" & (p1_name == "PRI" | p2_name == "PRI") & (p1_name == "PAN" | p2_name == "PAN"))
#df$mun_id <- gsub(" ", "", df$Municipio)
df$PRD_pct <- df$PRD / (df$p1 + df$p2) # PAN percentage compared to top two
df$PRD_treat <- ifelse(df$PRD_pct > 0.5, 1, 0)

DCdensity(df$PAN_pct[df$year <= 1997], cutpoint = 0.5)

big_df$mun_id <- gsub(" ", "", big_df$Municipio)

#create smaller datasets for merge
neighbors_PAN <- subset(big_df, select = c(year, mun_id, next_PAN_pct, PAN_pct, estado))
main_mun_PAN <- subset(df, select = c(year, mun_id, PAN_pct, estado))
DCdensity(main_mun_PAN$PAN_pct, cutpoint = 0.5)

#merge datasets using adjacent municipalities index
neighbors2 <- merge(near,neighbors_PAN, by.x = c("neighbor"), by.y = c("mun_id"))
neighbors2 <- neighbors2 %>% rename(vec_PAN_pct = PAN_pct)

df_rdd <- merge(main_mun_PAN,neighbors2, by.x = c("mun_id"), by.y = c("mun"))
df_rdd <- df_rdd %>% 
  rename(main_year = year.x, vec_year = year.y, main_estado = estado.x, vec_estado = estado.y)

DCdensity(df_rdd$PAN_pct, cutpoint = 0.5)

test1 <- unique(avd$mun_id)
test2 <- unique(df$mun_id)

#average vecino, distance neighbors
avd <- df_rdd %>% 
  group_by(mun_id) %>%
  summarise(PAN_pct = first(PAN_pct), vec_npp = mean(next_PAN_pct, na.rm = T), main_year = first(main_year), main_estado = first(main_estado), vec_pp = mean(vec_PAN_pct, na.rm = T), d = mean(d, na.rm = T))

DCdensity(avd$PAN_pct, cutpoint = 0.5)
title(x = "PAN vote share")

rd1 <- RDestimate(vec_npp ~ PAN_pct, cutpoint = 0.5, data = avd)
summary(rd1)

plot(rd1, range = c(0.4,0.6))

avd$PAN_win <- ifelse(avd$PAN_pct>=0.5,1,0)

m <- feols(vec_npp ~ vec_pp + PAN_pct | main_estado + main_year, data = avd)
m2 <- feols(vec_npp ~ vec_pp + PAN_pct*d | main_estado + main_year, data = avd)
m3 <- feols(vec_npp ~ vec_pp + PAN_win | main_estado + main_year, data = avd)
m4 <- feols(vec_npp ~ vec_pp + PAN_win*d | main_estado + main_year, data = avd)

etable(m, m2, m3, m4)

me <- marginaleffects(m2)


m5 <- feols(vec_npp ~ i(main_estado,PAN_win) + vec_pp | main_year, data = avd)
etable(m5)

iplot(m5)

PAN_governors <- c("Guanajuato", "Chihuahua")
PAN_governors2 <- c(" Guanajuato", "Chihuahua", "Jalisco", "Queretaro", "Nuevo Leon", "Aguascalientes", "Yucatan", "Morelos")

avd$PAN_gov <- ifelse(avd$main_estado %in% PAN_governors,1,0)


dd_gov <- feols(vec_npp ~ i(PAN_gov,PAN_win) + vec_pp | main_year + main_estado, data = avd)
summary(dd_gov)

iplot(dd_gov)

dd_gov <- feols(vec_npp ~ PAN_gov*PAN_win + vec_pp, data = avd)
summary(dd_gov)
