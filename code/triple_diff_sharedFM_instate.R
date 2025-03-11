library(sf)
library(dplyr)
library(ggplot2)
library(fixest)

audits <- read.csv("~/mexico_mun/raw/ueaa046_replication_package/audits_sample.csv")

audits$audit_id <- sprintf("%05d", audits$uniqueid)

asum <- audits %>%
  group_by(audit_id, year) %>%
  dplyr::summarise(audit = mean(audit), corrupt = mean(spent_unauthorized_sub), not_poor = mean(spent_not_poor_sub), PRI_inc = mean(PRI_inc), PAN_inc = mean(PAN_inc), PRD_inc = mean(PRD_inc), year = mean(year) )

load("~/mexico_mun/data/jaccard_similarity_FM.Rdata")

audit_js <- merge(asum, js, by.x = "audit_id", by.y = "mun_id")

load("~/mexico_RD/full_dataset_mexelec.Rdata")

# Find coalitions that contain parties
prd_columns_full <- grep("PRD", names(big_df), value = TRUE)
prd_columns <- setdiff(prd_columns_full,c("PRD_pct","PRD_treat"))
pan_columns <- c("PAN","PAN-PRD","PAN-ADC","PAN-CONV","PAN-PANAL","PAN-PANAL-PCP","PAN-PANAL-PES","PAN-PAC","PAN-PRD-CONV","PAN-PRD-CONV-PANAL","PAN-PRD-PT","PAN-PRD-PT-CONV","PAN-PRD-PVEM-PT","PAN-PRD-PT-PVEM-PCP-MC-PANAL","PAN-PRD-PUDC","PAN-PRD-PVEM","PAN-PRI","PAN-PRI-PANAL","PAN-PRI-PVEM","PAN-PRI-PVEM-PANAL","PAN-PRS","PAN-PSD","PAN-PT","PAN-PT-PANAL","PAN-PT-PANAL-MC","PAN-PT-PCDT-PJS","PAN-PVEM","PAN-PVEM-PANAL")
pri_columns <- grep("PRI", names(big_df), value = TRUE)

# Sum the values of these columns row-wise
big_df$PRD_sum <- rowSums(big_df[, prd_columns], na.rm = TRUE)
big_df$PAN_sum <- rowSums(big_df[, pan_columns], na.rm = TRUE)
big_df$PRI_sum <- rowSums(big_df[, pri_columns], na.rm = TRUE)

big_df$PRD_pct <- big_df$PRD_sum/big_df$TOTAL
big_df$PAN_pct <- big_df$PAN_sum/big_df$TOTAL
big_df$PRI_pct <- big_df$PRI_sum/big_df$TOTAL

big_df <- big_df %>%
  arrange(mun_id, year) %>%
  mutate(PRI_pct_lag = lag(PRI_pct, n = 1), PAN_pct_lag = lag(PAN_pct, n = 1), PRD_pct_lag = lag(PRD_pct, n = 1))

big_df$PRI_change <- big_df$PRI_pct - big_df$PRI_pct_lag
big_df$PAN_change <- big_df$PAN_pct - big_df$PAN_pct_lag
big_df$PRD_change <- big_df$PRD_pct - big_df$PRD_pct_lag

hmm <- subset(big_df, mun_id == "01001")
hmm <- subset(hmm, select = c(Municipio, year, PRI_pct_lag,PRI_pct,PAN_pct_lag,PAN_pct,PRD_pct_lag,PRD_pct,PRI_change,PAN_change,PRD_change))

big_df_merge <- subset(big_df, select = c(mun_id,year,PRI_change,PAN_change,PRD_change))

test_old <- merge(audit_js, big_df_merge, by.x = c("ref_mun_id","year"), by.y = c("mun_id","year"), all = T)

ref_audits <- subset(asum, select = -c(corrupt, not_poor))

test <- merge(test_old, ref_audits, by.x = c("ref_mun_id","year"), by.y = c("audit_id","year"), all = T)

test$simult_audit <- ifelse(test$audit.x == test$audit.y,1,0)
test$audit_state <- substr(test$audit_id, 1, 2)
test$elec_state <- substr(test$ref_mun_id, 1, 2)

test <- subset(test, audit_state == elec_state)

test$corruptm <- ifelse(test$corrupt > summary(asum$corrupt)[3],1,0)
test$corrupt3 <- ifelse(test$corrupt > summary(asum$corrupt)[3] & test$corrupt < summary(asum$corrupt)[5],1,0)
test$corrupt4 <- ifelse(test$corrupt >= summary(asum$corrupt)[5],1,0)
test$corrupt0 <- ifelse(test$corrupt == 0,1,0)

test$pvs <- ifelse(test$PRI_inc.x ==1, test$PRI_change, ifelse(test$PAN_inc.x==1,test$PAN_change,test$PRD_change))

test$pvs[is.infinite(test$pvs)] <- NA

test$a_e_id <- paste0(test$audit_id, test$ref_mun_id)

test$js_new <- ifelse(test$js>0,1,0)
test$ay <- paste0(test$audit_id,test$year)
test$ey <- paste0(test$ref_mun_id,test$year)

test$same_inc <- 0
test$same_inc[test$PRI_inc.x == test$PRI_inc.y] <- 1
test$same_inc[test$PAN_inc.x == test$PAN_inc.y] <- 1
test$same_inc[test$PRD_inc.x == test$PRD_inc.y] <- 1

test$audit_inc <- ifelse(test$audit.x ==1 & test$audit.y == 1 & test$same_inc ==1,1,0)
test$simult_audit <- ifelse(test$audit.x ==1 & test$audit.y == 1,1,0)

test$audit <- test$audit.x

#what about both?
# test$med_both <- ifelse(test$corrupt >= summary(audits$spent_unauthorized_sub)[3] | test$not_poor >= summary(audits$spent_not_poor_sub)[3],1,0)
# 
# m0 <- feols(pvs ~ med_both*js_new*audit + audit_inc, data = test)
# m1 <- feols(pvs ~ med_both*js_new*audit + audit_inc | audit_id, data = test)
# m2 <- feols(pvs ~ med_both*js_new*audit + audit_inc | audit_id + year, data = test)
# m3 <- feols(pvs ~ med_both*js_new*audit + audit_inc | audit_id + ref_mun_id + year, data = test)
# m4 <- feols(pvs ~ med_both*js_new*audit + audit_inc | a_e_id + year, data = test)
# m5 <- feols(pvs ~ med_both*js_new*audit + audit_inc | a_e_id + year, data = test)
# 
# etable(m0,m1,m2,m3,m4,m5) #can't estimate most of these because of colinearity


test$four_both <- ifelse(test$corrupt >= summary(audits$spent_unauthorized_sub)[5] | test$not_poor >= summary(audits$spent_not_poor_sub)[5],1,0)

m0 <- feols(pvs ~ four_both*js_new*audit + audit_inc, data = test)
m1 <- feols(pvs ~ four_both*js_new*audit + audit_inc | audit_id, data = test)
m2 <- feols(pvs ~ four_both*js_new*audit + audit_inc | audit_id + ref_mun_id, data = test)
m3 <- feols(pvs ~ four_both*js_new*audit + audit_inc | audit_id + ref_mun_id + year, data = test)
m4 <- feols(pvs ~ four_both*js_new*audit + audit_inc | a_e_id, data = test)
m5 <- feols(pvs ~ four_both*js_new*audit + audit_inc | a_e_id + year, data = test)

etable(m0,m1,m2,m3,m4,m5, digits = 3, tex = F)

#intensive margin
newthing <- subset(test, js > 0)

m0i <- feols(pvs ~ four_both*js*audit + audit_inc, data = newthing)
m1i <- feols(pvs ~ four_both*js*audit + audit_inc | audit_id, data = newthing)
m2i <- feols(pvs ~ four_both*js*audit + audit_inc | audit_id + year, data = newthing)
m3i <- feols(pvs ~ four_both*js*audit + audit_inc | audit_id + ref_mun_id + year, data = newthing)
m4i <- feols(pvs ~ four_both*js*audit + audit_inc | a_e_id, data = newthing)
m5i <- feols(pvs ~ four_both*js*audit + audit_inc | a_e_id + year, data = newthing)

etable(m0i,m1i,m2i,m3i,m4i,m5i, tex = F)

#above median corruption
m0m <- feols(pvs ~ corruptm*js_new*audit + audit_inc, data = test)
m1m <- feols(pvs ~ corruptm*js_new*audit + audit_inc | audit_id, data = test)
m2m <- feols(pvs ~ corruptm*js_new*audit + audit_inc | audit_id + ref_mun_id, data = test)
m3m <- feols(pvs ~ corruptm*js_new*audit + audit_inc | audit_id + ref_mun_id, cluster = c("a_e_id","year"), data = test)
m4m <- feols(pvs ~ corruptm*js_new*audit + audit_inc | a_e_id, data = test)
m5m <- feols(pvs ~ corruptm*js_new*audit + audit_inc | a_e_id, cluster = c("a_e_id","year"), data = test)

etable(m0m,m1m,m2m,m3m,m5m, digits = 3, tex = F)

summary(m5m)

#check if js_new is messing things up in m5m
# test$js_new_corruptm <- test$js_new*test$corruptm
# test$js_new_audit <- test$js_new*test$audit
# test$js_new_corruptm_audit <- test$js_new*test$corruptm*test$audit
# 
# 
# check <- feols(pvs ~ corruptm*audit + js_new_corruptm + js_new_audit + js_new_corruptm_audit + audit_inc | a_e_id, cluster = c("a_e_id","year"), data = test)
# 
# etable(m5m,check, digits = 3, tex = F) #it's the same!

#full js
m0mi <- feols(pvs ~ corruptm*js*audit + audit_inc, cluster = c("a_e_id", "year"), data = test)
m2mi <- feols(pvs ~ corruptm*js*audit + audit_inc | audit_id + ref_mun_id, cluster = c("a_e_id"), data = test)
m3mi <- feols(pvs ~ corruptm*js*audit + audit_inc | audit_id + ref_mun_id, cluster = c("a_e_id","year"), data = test)
m4mi <- feols(pvs ~ corruptm*js*audit + audit_inc | a_e_id, data = test)
m5mi <- feols(pvs ~ corruptm*js*audit + audit_inc | a_e_id, cluster = c("a_e_id","year"), data = test)

etable(m0mi,m2mi,m3mi,m4mi,m5mi, digits = 3, tex = F)

coefplot(m3mi, ci_level = .90)
coefplot(m5mi, ci_level = .95)

summary(m0mi)

#check if js is messing things up in m5m
# test$js_corruptm <- test$js*test$corruptm
# test$js_audit <- test$js*test$audit
# test$js_corruptm_audit <- test$js*test$corruptm*test$audit
# 
# 
# check <- feols(pvs ~ corruptm*audit + js_corruptm + js_audit + js_corruptm_audit + audit_inc | a_e_id, cluster = c("a_e_id","year"), data = test)
# etable(m5mi,check, digits = 3, tex = F) #it's the same!

#try with only audit.y NAs?
no_ay <- subset(test, is.na(audit.y))

m0mi <- feols(pvs ~ corruptm*js*audit + audit_inc, data = no_ay)
m1mi <- feols(pvs ~ corruptm*js*audit + audit_inc | audit_id + ref_mun_id, cluster = c("a_e_id", "year"), data = no_ay)
m2mi <- feols(pvs ~ corruptm*js*audit + audit_inc | audit_id + ref_mun_id, cluster = c("a_e_id"), data = no_ay)
m3mi <- feols(pvs ~ corruptm*js*audit + audit_inc | audit_id + ref_mun_id, cluster = c("a_e_id","year"), data = no_ay)
m4mi <- feols(pvs ~ corruptm*js*audit + audit_inc | a_e_id, data = no_ay)
m5mi <- feols(pvs ~ corruptm*js*audit + audit_inc | a_e_id, cluster = c("a_e_id","year"), data = no_ay)

etable(m0mi,m1mi,m2mi,m3mi,m4mi,m5mi, digits = 3, tex = F)

summary(m5mi)

#try with excluding audit.y == 1
zero_ay <- subset(test, audit.y == 0 | is.na(audit.y))

m0mi <- feols(pvs ~ corruptm*js*audit, data = zero_ay)
m1mi <- feols(pvs ~ corruptm*js*audit | audit_id + ref_mun_id, cluster = c("a_e_id", "year"), data = zero_ay)
m2mi <- feols(pvs ~ corruptm*js*audit | audit_id + ref_mun_id, cluster = c("a_e_id"), data = zero_ay)
m3mi <- feols(pvs ~ corruptm*js*audit | audit_id + ref_mun_id, cluster = c("a_e_id","year"), data = zero_ay)
m4mi <- feols(pvs ~ corruptm*js*audit | a_e_id, data = zero_ay)
m5mi <- feols(pvs ~ corruptm*js*audit | a_e_id, cluster = c("a_e_id","year"), data = zero_ay)

etable(m1mi,m5mi, digits = 3, tex = F)

coefplot(m5mi, ci_level = .90)

summary(m5mi)

#try with excluding audit_inc == 1
zero_ayinc <- subset(test, audit_inc != 1)

m0mi <- feols(pvs ~ corruptm*js*audit, data = zero_ayinc)
m1mi <- feols(pvs ~ corruptm*js*audit | audit_id + ref_mun_id, cluster = c("a_e_id", "year"), data = zero_ayinc)
m2mi <- feols(pvs ~ corruptm*js*audit | audit_id + ref_mun_id, cluster = c("a_e_id"), data = zero_ayinc)
m3mi <- feols(pvs ~ corruptm*js*audit | audit_id + ref_mun_id, cluster = c("a_e_id","year"), data = zero_ayinc)
m4mi <- feols(pvs ~ corruptm*js*audit | a_e_id, data = zero_ayinc)
m5mi <- feols(pvs ~ corruptm*js*audit | a_e_id, cluster = c("a_e_id","year"), data = zero_ayinc)

etable(m1mi,m5mi, digits = 3, tex = F)

coefplot(m1mi, ci_level = .90)
coefplot(m5mi, ci_level = .90)

summary(m5mi)

#intensive margin
new_test <- subset(test, js > 0)

m0mi <- feols(pvs ~ corruptm*js*audit + audit_inc, data = new_test)
m1mi <- feols(pvs ~ corruptm*js*audit + audit_inc | audit_id, data = new_test)
m2mi <- feols(pvs ~ corruptm*js*audit + audit_inc | audit_id + ref_mun_id, data = new_test)
m3mi <- feols(pvs ~ corruptm*js*audit + audit_inc | audit_id + ref_mun_id, cluster = c("a_e_id","year"), data = new_test)
m4mi <- feols(pvs ~ corruptm*js*audit + audit_inc | a_e_id, data = new_test)
m5mi <- feols(pvs ~ corruptm*js*audit + audit_inc | a_e_id, cluster = c("a_e_id","year"), data = new_test)

etable(m0mi,m1mi,m2mi,m3mi,m5mi, digits = 3, tex = F)

#fourth quartile unauthorized spending
m0f <- feols(pvs ~ corrupt4*js_new*audit + audit_inc, data = test)
m1f <- feols(pvs ~ corrupt4*js_new*audit + audit_inc | audit_id, data = test)
m2f <- feols(pvs ~ corrupt4*js_new*audit + audit_inc | audit_id + ref_mun_id, data = test)
m3f <- feols(pvs ~ corrupt4*js_new*audit + audit_inc | audit_id + ref_mun_id, cluster = c("a_e_id","year"), data = test)
m4f <- feols(pvs ~ corrupt4*js_new*audit + audit_inc | a_e_id, data = test)
m5f <- feols(pvs ~ corrupt4*js_new*audit + audit_inc | a_e_id, cluster = c("a_e_id","year"), data = test)

etable(m0f,m1f,m2f,m3f,m4f,m5f, tex = F)

#larreguy spec unauthorized spending
m0f <- feols(pvs ~ corrupt3*js_new*audit + corrupt4*js_new*audit + audit_inc, data = test)
m1f <- feols(pvs ~ corrupt3*js_new*audit + corrupt4*js_new*audit + audit_inc | audit_id, data = test)
m2f <- feols(pvs ~ corrupt3*js_new*audit + corrupt4*js_new*audit + audit_inc | audit_id + year, data = test)
m3f <- feols(pvs ~ corrupt3*js_new*audit + corrupt4*js_new*audit + audit_inc | audit_id + ref_mun_id + year, data = test)
m4f <- feols(pvs ~ corrupt3*js_new*audit + corrupt4*js_new*audit + audit_inc | a_e_id, data = test)
m5f <- feols(pvs ~ corrupt3*js_new*audit + corrupt4*js_new*audit + audit_inc | a_e_id + year, data = test)

etable(m0f,m1f,m2f,m3f,m4f,m5f, tex = F)

#intensive margin
m0f <- feols(pvs ~ corrupt4*js*audit + audit_inc, data = test)
m1f <- feols(pvs ~ corrupt4*js*audit + audit_inc | audit_id, data = test)
m2f <- feols(pvs ~ corrupt4*js*audit + audit_inc | audit_id + year, data = test)
m3f <- feols(pvs ~ corrupt4*js*audit + audit_inc | audit_id + ref_mun_id + year, data = test)
m4f <- feols(pvs ~ corrupt4*js*audit + audit_inc | a_e_id, data = test)
m5f <- feols(pvs ~ corrupt4*js*audit + audit_inc | a_e_id + year, data = test)

etable(m0f,m1f,m2f,m3f,m4f,m5f, tex = F)

#no unauthorized spending
m0no <- feols(pvs ~ corrupt0*js_new*audit + audit_inc, data = test)
m1no <- feols(pvs ~ corrupt0*js_new*audit + audit_inc | audit_id, data = test)
m2no <- feols(pvs ~ corrupt0*js_new*audit + audit_inc | audit_id + year, data = test)
m3no <- feols(pvs ~ corrupt0*js_new*audit + audit_inc | audit_id + ref_mun_id, data = test)
m4no <- feols(pvs ~ corrupt0*js_new*audit + audit_inc | audit_id + ref_mun_id + year, data = test)
m5no <- feols(pvs ~ corrupt0*js_new*audit + audit_inc | a_e_id, data = test)

etable(m0no,m1no,m2no,m3no,m4no,m5no)

#positive non poor spending

test$not_poor_pos <- ifelse(test$not_poor >0,1,0)
test$not_poor4 <- ifelse(test$not_poor >= summary(audits$spent_not_poor_sub)[5],1,0)

m0 <- feols(pvs ~ not_poor_pos*js_new*audit + simult_audit, data = test)
m1 <- feols(pvs ~ not_poor_pos*js_new*audit + simult_audit | audit_id, data = test)
m2 <- feols(pvs ~ not_poor_pos*js_new*audit + simult_audit | audit_id + year, data = test)
m3 <- feols(pvs ~ not_poor_pos*js_new*audit + simult_audit | audit_id + ref_mun_id + year, data = test)
m4 <- feols(pvs ~ not_poor_pos*js_new*audit + simult_audit | a_e_id, data = test)
m5 <- feols(pvs ~ not_poor_pos*js_new*audit + simult_audit | a_e_id + year, data = test)

etable(m0,m1,m2,m3,m4,m5)

#4th quartile
m0 <- feols(pvs ~ not_poor4*js_new*audit + simult_audit, data = test)
m1 <- feols(pvs ~ not_poor4*js_new*audit + simult_audit | audit_id, data = test)
m2 <- feols(pvs ~ not_poor4*js_new*audit + simult_audit | audit_id + year, data = test)
m3 <- feols(pvs ~ not_poor4*js_new*audit + simult_audit | audit_id + ref_mun_id + year, data = test)
m4 <- feols(pvs ~ not_poor4*js_new*audit + simult_audit | a_e_id, data = test)
m5 <- feols(pvs ~ not_poor4*js_new*audit + simult_audit | a_e_id + year, data = test)

etable(m0,m1,m2,m3,m4,m5, tex = F)
