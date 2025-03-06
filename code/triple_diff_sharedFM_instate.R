library(sf)
library(dplyr)
library(ggplot2)
library(fixest)

audits <- read.csv("~/mexico_mun/raw/ueaa046_replication_package/audits_sample.csv")

audits$audit_id <- sprintf("%05d", audits$uniqueid)

asum <- audits %>%
  group_by(audit_id, year) %>%
  summarise(audit = mean(audit), corrupt = mean(spent_unauthorized_sub), not_poor = mean(spent_not_poor_sub), PRI_inc = mean(PRI_inc), PAN_inc = mean(PAN_inc), PRD_inc = mean(PRD_inc), year = mean(year) )

load("~/mexico_mun/data/jaccard_similarity_FM.Rdata")

audit_js <- merge(asum, js, by.x = "audit_id", by.y = "mun_id")

load("~/mexico_RD/full_dataset_mexelec.Rdata")

# Find coalitions that contain parties
prd_columns <- grep("PRD", names(big_df), value = TRUE)
pan_columns <- grep("PAN", names(big_df), value = TRUE)
pri_columns <- grep("PRI", names(big_df), value = TRUE)

# Sum the values of these columns row-wise
big_df$PRD_sum <- rowSums(big_df[, prd_columns], na.rm = TRUE)
big_df$PAN_sum <- rowSums(big_df[, pan_columns], na.rm = TRUE)
big_df$PRI_sum <- rowSums(big_df[, pri_columns], na.rm = TRUE)

big_df$PRD_pct <- big_df$PRD_sum/big_df$TOTAL
big_df$PAN_pct <- big_df$PAN_sum/big_df$TOTAL
big_df$PRI_pct <- big_df$PRI_sum/big_df$TOTAL

big_df <- big_df %>%
  arrange(year) %>%
  mutate(PRI_pct_lag = lag(PRI_pct, n = 1), PAN_pct_lag = lag(PAN_pct, n = 1), PRD_pct_lag = lag(PRD_pct, n = 1))

big_df$PRI_change <- big_df$PRI_pct_lag - big_df$PRI_pct
big_df$PAN_change <- big_df$PAN_pct_lag - big_df$PAN_pct
big_df$PRD_change <- big_df$PRD_pct_lag - big_df$PRD_pct

big_df_merge <- subset(big_df, select = c(mun_id,year,PRI_change,PAN_change,PRD_change))

test <- merge(audit_js, big_df_merge, by.x = c("ref_mun_id","year"), by.y = c("mun_id","year"), all.x = T)
test$audit_state <- substr(test$audit_id, 1, 2)
test$elec_state <- substr(test$ref_mun_id, 1, 2)

test <- subset(test, audit_state == elec_state)

test$corruptm <- ifelse(test$corrupt > summary(asum$corrupt)[3],1,0)
test$corrupt4 <- ifelse(test$corrupt >= summary(asum$corrupt)[5],1,0)
test$corrupt0 <- ifelse(test$corrupt == 0,1,0)

test$pvs <- ifelse(test$PRI_inc ==1, test$PRI_change, ifelse(test$PAN_inc==1,test$PAN_change,test$PRI_change))
test$pvs[is.infinite(test$pvs)] <- NA

m <- feols(pvs ~ corruptm*js*audit | audit_id + ref_mun_id, data = test)
summary(m)

test$a_e_id <- paste0(test$audit_id, test$ref_mun_id)

test$js_new <- ifelse(test$js>0,1,0)
test$ay <- paste0(test$audit_id,test$year)
test$ey <- paste0(test$ref_mun_id,test$year)

#above median corruption
m0m <- feols(pvs ~ corruptm*js_new*audit, data = test)
m1m <- feols(pvs ~ corruptm*js_new*audit | audit_id, data = test)
m2m <- feols(pvs ~ corruptm*js_new*audit | audit_id + year, data = test)
m3m <- feols(pvs ~ corruptm*js_new*audit | audit_id + ref_mun_id, data = test)
m4m <- feols(pvs ~ corruptm*js_new*audit | audit_id + ref_mun_id + year, data = test)
m5m <- feols(pvs ~ corruptm*js_new*audit | a_e_id, data = test)

etable(m0m,m1m,m2m,m3m,m4m,m5m, tex = T)

#fourth quartile unauthorized spending
m0f <- feols(pvs ~ corrupt4*js_new*audit, data = test)
m1f <- feols(pvs ~ corrupt4*js_new*audit | audit_id, data = test)
m2f <- feols(pvs ~ corrupt4*js_new*audit | audit_id + year, data = test)
m3f <- feols(pvs ~ corrupt4*js_new*audit | audit_id + ref_mun_id, data = test)
m4f <- feols(pvs ~ corrupt4*js_new*audit | audit_id + ref_mun_id + year, data = test)
m5f <- feols(pvs ~ corrupt4*js_new*audit | a_e_id, data = test)

etable(m0f,m1f,m2f,m3f,m4f,m5f, tex = T)

#no unauthorized spending
m0no <- feols(pvs ~ corrupt0*js_new*audit, data = test)
m1no <- feols(pvs ~ corrupt0*js_new*audit | audit_id, data = test)
m2no <- feols(pvs ~ corrupt0*js_new*audit | audit_id + year, data = test)
m3no <- feols(pvs ~ corrupt0*js_new*audit | audit_id + ref_mun_id, data = test)
m4no <- feols(pvs ~ corrupt0*js_new*audit | audit_id + ref_mun_id + year, data = test)
m5no <- feols(pvs ~ corrupt0*js_new*audit | a_e_id, data = test)

etable(m0no,m1no,m2no,m3no,m4no,m5no)

#intensive margin
newthing <- subset(test, js > 0)

m0f <- feols(pvs ~ corrupt4*js*audit, data = newthing)
m1f <- feols(pvs ~ corrupt4*js*audit | audit_id, data = newthing)
m2f <- feols(pvs ~ corrupt4*js*audit | audit_id + year, data = newthing)
m3f <- feols(pvs ~ corrupt4*js*audit | audit_id + ref_mun_id, data = newthing)
m4f <- feols(pvs ~ corrupt4*js*audit | audit_id + ref_mun_id + year, data = newthing)
m5f <- feols(pvs ~ corrupt4*js*audit | a_e_id, data = newthing)

etable(m0f,m1f,m2f,m3f,m4f,m5f, tex = T)

#positive non poor spending

test$not_poor_pos <- ifelse(test$not_poor >0,1,0)
test$not_poor4 <- ifelse(test$not_poor >= summary(audits$spent_not_poor_sub)[5],1,0)

m0 <- feols(pvs ~ not_poor_pos*js_new*audit, data = test)
m1 <- feols(pvs ~ not_poor_pos*js_new*audit | audit_id, data = test)
m2 <- feols(pvs ~ not_poor_pos*js_new*audit | audit_id + year, data = test)
m3 <- feols(pvs ~ not_poor_pos*js_new*audit | audit_id + ref_mun_id, data = test)
m4 <- feols(pvs ~ not_poor_pos*js_new*audit | audit_id + ref_mun_id + year, data = test)
m5 <- feols(pvs ~ not_poor_pos*js_new*audit | a_e_id, data = test)

etable(m0,m1,m2,m3,m4,m5)

#4th quartile
m0 <- feols(pvs ~ not_poor4*js_new*audit, data = test)
m1 <- feols(pvs ~ not_poor4*js_new*audit | audit_id, data = test)
m2 <- feols(pvs ~ not_poor4*js_new*audit | audit_id + year, data = test)
m3 <- feols(pvs ~ not_poor4*js_new*audit | audit_id + ref_mun_id, data = test)
m4 <- feols(pvs ~ not_poor4*js_new*audit | audit_id + ref_mun_id + year, data = test)
m5 <- feols(pvs ~ not_poor4*js_new*audit | a_e_id, data = test)

etable(m0,m1,m2,m3,m4,m5)

#what about both?


