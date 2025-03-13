library(sf)
library(dplyr)
library(ggplot2)
library(fixest)

audits <- read.csv("~/mexico_mun/raw/ueaa046_replication_package/audits_sample.csv")

audits$audit_id <- sprintf("%05d", audits$uniqueid)

asum <- audits %>%
  group_by(audit_id, year) %>%
  dplyr::summarise(audit = mean(audit), corrupt = mean(spent_unauthorized_sub), not_poor = mean(spent_not_poor_sub), PRI_inc = mean(PRI_inc), PAN_inc = mean(PAN_inc), PRD_inc = mean(PRD_inc), year = mean(year), coalition_partners = mean(coalition_partners) )

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

radio <- merge(test_old, ref_audits, by.x = c("ref_mun_id","year"), by.y = c("audit_id","year"), all = T)

radio$simult_audit <- ifelse(radio$audit.x == radio$audit.y,1,0)
radio$audit_state <- substr(radio$audit_id, 1, 2)
radio$elec_state <- substr(radio$ref_mun_id, 1, 2)

radio <- subset(radio, audit_state == elec_state)

radio$corruptm <- ifelse(radio$corrupt > summary(asum$corrupt)[3],1,0)
radio$corrupt3 <- ifelse(radio$corrupt > summary(asum$corrupt)[3] & radio$corrupt < summary(asum$corrupt)[5],1,0)
radio$corrupt4 <- ifelse(radio$corrupt >= summary(asum$corrupt)[5],1,0)
radio$corrupt0 <- ifelse(radio$corrupt == 0,1,0)

radio$pvs <- ifelse(radio$PRI_inc.x ==1, radio$PRI_change, ifelse(radio$PAN_inc.x==1,radio$PAN_change,radio$PRD_change))

radio$pvs[is.infinite(radio$pvs)] <- NA

radio$a_e_id <- paste0(radio$audit_id, radio$ref_mun_id)

radio$js_new <- ifelse(radio$js>0,1,0)
radio$ay <- paste0(radio$audit_id,radio$year)
radio$ey <- paste0(radio$ref_mun_id,radio$year)

radio$same_inc <- 0
radio$same_inc[radio$PRI_inc.x == radio$PRI_inc.y] <- 1
radio$same_inc[radio$PAN_inc.x == radio$PAN_inc.y] <- 1
radio$same_inc[radio$PRD_inc.x == radio$PRD_inc.y] <- 1

radio$audit_inc <- ifelse(radio$audit.x ==1 & radio$audit.y == 1 & radio$same_inc ==1,1,0)
radio$simult_audit <- ifelse(radio$audit.x ==1 & radio$audit.y == 1,1,0)

radio$audit <- radio$audit.x

save(radio, file = "~/mexico_mun/data/radio_elec.Rdata")
