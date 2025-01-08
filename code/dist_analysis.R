library(ggplot2)
library(dplyr)
library(rdd)
library(fixest)

load("~/mexico_RD/data/vec5.Rdata")


load("~/mexico_RD/data/full_dataset_mexelec.Rdata")
big_df$mun_id <- gsub(" ", "", big_df$Municipio)

df <- subset(big_df, year>= 1995 & year <= 1997 
             #& estado!="Tlaxcala" 
             & (p1_name == "PRI" | p2_name == "PRI") & (p1_name == "PAN" | p2_name == "PAN"))
#df$mun_id <- gsub(" ", "", df$Municipio)
df$PRD_pct <- df$PRD / (df$p1 + df$p2) # PRD percentage compared to top two
df$PRD_treat <- ifelse(df$PRD_pct > 0.5, 1, 0)

DCdensity(df$PAN_pct[df$year <= 1997], cutpoint = 0.5)

#create smaller datasets for merge
ref_PAN <- subset(df, select = c(year, mun_id, next_PAN_pct, PAN_pct, estado))
main_mun_PAN <- subset(df, select = c(year, mun_id, PAN_pct, estado))
DCdensity(main_mun_PAN$PAN_pct, cutpoint = 0.5)

#merge datasets using adjacent municipalities index
ref2 <- merge(vec5,ref_PAN, by.x = c("neighbor"), by.y = c("mun_id"))
ref2 <- ref2 %>% rename(ref_PAN_pct = PAN_pct)

df_rdd <- merge(main_mun_PAN,ref2, by.x = c("mun_id"), by.y = c("mun"))
df_rdd <- df_rdd %>% 
  rename(main_year = year.x, ref_year = year.y, main_estado = estado.x, ref_estado = estado.y)

DCdensity(df_rdd$PAN_pct, cutpoint = 0.5)

#average of references (distance only)
avg_ref <- df_rdd %>% 
  group_by(mun_id) %>%
  summarise(PAN_pct = first(PAN_pct), ref_npp = mean(next_PAN_pct, na.rm = T), main_year = first(main_year), main_estado = first(main_estado), ref_pp = mean(ref_PAN_pct, na.rm = T), d = mean(d, na.rm = T))

DCdensity(avg_ref$PAN_pct, cutpoint = 0.5)
title(x = "PAN vote share")

avg_ref$change_pp <- avg_ref$ref_npp - avg_ref$ref_pp

rd5.1 <- RDestimate(ref_npp ~ PAN_pct + ref_pp, cutpoint = 0.5, data = avg_ref)
summary(rd5.1)

plot(rd5.1, range = c(0.5-rd5.1$bw[1],0.5+rd5.1$bw[1]))

rd5.2 <- RDestimate(change_pp ~ PAN_pct, cutpoint = 0.5, data = avg_ref)
summary(rd5.2)

plot(rd5.2, range = c(0.5-rd5.2$bw[1],0.5+rd5.2$bw[1]))
title(main = "Effect of PAN win on AVG. nearby PAN vote share", x = "PAN vote share", y = "Change in Avg. PAN vote share, t+1")

#only 4 closest

# Create a subset of df_rdd by removing rows with the highest value of d for each mun_id
df_rdd_4 <- df_rdd %>%
  group_by(mun_id) %>%
  filter(d != max(d)) %>%
  ungroup()

avg_ref4 <- df_rdd_4 %>% 
  group_by(mun_id) %>%
  summarise(PAN_pct = first(PAN_pct), ref_npp = mean(next_PAN_pct, na.rm = T), main_year = first(main_year), main_estado = first(main_estado), ref_pp = mean(ref_PAN_pct, na.rm = T), d = mean(d, na.rm = T))

DCdensity(avg_ref4$PAN_pct, cutpoint = 0.5)
title(x = "PAN vote share")

avg_ref4$change_pp <- avg_ref4$ref_npp - avg_ref4$ref_pp

rd4.1 <- RDestimate(ref_npp ~ PAN_pct, cutpoint = 0.5, data = avg_ref4)
summary(rd4.1)

rd4.2 <- RDestimate(change_pp ~ PAN_pct, cutpoint = 0.5, data = avg_ref4)
summary(rd4.2)

plot(rd4.2, range = c(0.5-rd4.2$bw[1],0.5+rd4.2$bw[1]))

#now only top 3
# Create a subset of df_rdd by removing the top two rows with the highest value of d for each mun_id
df_rdd_3 <- df_rdd %>%
  group_by(mun_id) %>%
  arrange(desc(d)) %>%
  slice(-1:-2) %>%
  ungroup()

avg_ref3 <- df_rdd_3 %>% 
  group_by(mun_id) %>%
  summarise(PAN_pct = first(PAN_pct), ref_npp = mean(next_PAN_pct, na.rm = T), main_year = first(main_year), main_estado = first(main_estado), ref_pp = mean(ref_PAN_pct, na.rm = T), d = mean(d, na.rm = T))

DCdensity(avg_ref3$PAN_pct, cutpoint = 0.5)
title(x = "PAN vote share")

avg_ref3$change_pp <- avg_ref3$ref_npp - avg_ref3$ref_pp

rd3 <- RDestimate(ref_npp ~ PAN_pct, cutpoint = 0.5, data = avg_ref3)
summary(rd3)

plot(rd1, range = c(0.5-rd1$bw[1],0.5+rd1$bw[1]))

rd2.2 <- RDestimate(change_pp ~ PAN_pct, cutpoint = 0.5, data = avg_ref3)
summary(rd2.2)

plot(rd2.2, range = c(0.5-rd2.2$bw[1],0.5+rd2.2$bw[1]))

#balance test

below <- subset(avg_ref, PAN_pct >= 0.5-rd5.1$bw[1] & PAN_pct < 0.5)
above <- subset(avg_ref, PAN_pct <= 0.5+rd5.1$bw[1] & PAN_pct > 0.5)

# Using t.tset()
result = t.test(below$ref_pp, above$ref_pp, 
                var.equal = TRUE) 

# Print the result
print(result)

# Using t.tset()
result = t.test(below$ref_npp, above$ref_npp, 
                var.equal = TRUE) 

# Print the result
print(result)

# Using t.tset()
result = t.test(below$main_year, above$main_year, 
                var.equal = TRUE) 

# Print the result
print(result)
#no difference in years!!

