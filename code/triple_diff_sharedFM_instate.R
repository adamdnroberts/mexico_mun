library(sf)
library(dplyr)
library(ggplot2)
library(fixest)

load("~/mexico_mun/data/radio_elec.Rdata")

zero_ay <- subset(radio, audit.y == 0 | is.na(audit.y))
zero_ayinc <- subset(radio, audit_inc != 1)

#no outcome audit
noa <- feols(pvs ~ corruptm*js*audit + coalition_partners.x, cluster = c("a_e_id", "year"), data = zero_ay)
noa_fe <- feols(pvs ~ corruptm*js*audit + coalition_partners.x | audit_id + ref_mun_id, cluster = c("a_e_id","year"), data = zero_ay)
noa_pair_fe <- feols(pvs ~ corruptm*js*audit + coalition_partners.x | a_e_id, cluster = c("a_e_id","year"), data = zero_ay)

#no same incumbent audit
nia <- feols(pvs ~ corruptm*js*audit + coalition_partners.x, cluster = c("a_e_id", "year"), data = zero_ayinc)
nia_fe <- feols(pvs ~ corruptm*js*audit + coalition_partners.x | audit_id + ref_mun_id, cluster = c("a_e_id","year"), data = zero_ayinc)
nia_pair_fe <- feols(pvs ~ corruptm*js*audit + coalition_partners.x | a_e_id, cluster = c("a_e_id","year"), data = zero_ayinc)

#control for same party simultaneous audits
csa <- feols(pvs ~ corruptm*js*audit + coalition_partners.x + audit_inc, cluster = c("a_e_id", "year"), data = radio)
csa_fe <- feols(pvs ~ corruptm*js*audit + coalition_partners.x + audit_inc | audit_id + ref_mun_id, cluster = c("a_e_id","year"), data = radio)
csa_pair_fe <- feols(pvs ~ corruptm*js*audit + coalition_partners.x + audit_inc | a_e_id, cluster = c("a_e_id","year"), data = radio)

#for appendix
etable(noa,noa_pair_fe,nia,nia_pair_fe,csa,csa_pair_fe, digits = 3, tex = T)

#for paper
etable(noa,noa_pair_fe, digits = 3, tex = F)

coefplot(noa_pair_fe, drop = "coalition_partners.x")

#plot
plot_data <- as.data.frame(noa_pair_fe$coefficients)
colnames(plot_data) <- "est"
plot_data$vars <- row.names(plot_data)
plot_data$cilow <- test$`2.5 %`
plot_data$cihigh <- test$`97.5 %`

# Filter the data frame
plot_data <- plot_data %>%
  filter(vars %in% c("corruptm:js", "corruptm:audit", "js:audit", "corruptm:js:audit"))

plot_data$placebo <- as.factor(ifelse(plot_data$vars == "corruptm:js" | plot_data$vars == "corruptm:audit",1,0))

p <- ggplot(plot_data, aes(x = vars, y = est, color = placebo)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_errorbar(aes(ymin = cilow, ymax = cihigh), width = 0.2, position = position_dodge(width = -0.5)) +
  geom_point(position = position_dodge(width = -0.5)) +
  labs(x = "", y = "Coefficient and 95% CI", title = "Model Coefficients") +
  theme_minimal()

print(p)

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/TYP_radio/images/model_coefs.png", plot = p, width = 6, height = 4)


# ALTERNATIVE SPECIFICATIONS #

#fourth quartile unauthorized spending
m0f <- feols(pvs ~ corrupt4*js_new*audit + coalition_partners.x, cluster = c("a_e_id","year"), data = zero_ay)
m3f <- feols(pvs ~ corrupt4*js_new*audit + coalition_partners.x | audit_id + ref_mun_id, cluster = c("a_e_id","year"), data = zero_ay)
m5f <- feols(pvs ~ corrupt4*js_new*audit + coalition_partners.x | a_e_id, cluster = c("a_e_id","year"), data = zero_ay)

etable(m0f,m3f,m5f, digits = 3, tex = F)

#intensive margin
radio_int_zero_ay <- subset(radio, js > 0 & (audit.y == 0 | is.na(audit.y)))

noa <- feols(pvs ~ corruptm*js*audit + coalition_partners.x, cluster = c("a_e_id", "year"), data = radio_int_zero_ay)
noa_pair_fe <- feols(pvs ~ corruptm*js*audit + coalition_partners.x | a_e_id, cluster = c("a_e_id","year"), data = radio_int_zero_ay)

etable(noa,noa_pair_fe, digits = 3, tex = F)

#extensive margin
noa <- feols(pvs ~ corruptm*js_new*audit + coalition_partners.x, cluster = c("a_e_id", "year"), data = zero_ay)
noa_pair_fe <- feols(pvs ~ corruptm*js_new*audit + coalition_partners.x | a_e_id, cluster = c("a_e_id","year"), data = zero_ay)

etable(noa,noa_pair_fe, digits = 3, tex = F)

#non-poor spending
radio$not_poor_pos <- ifelse(radio$not_poor >0,1,0)
radio$not_poor4 <- ifelse(radio$not_poor >= summary(audits$spent_not_poor_sub)[5],1,0)

zero_ay <- subset(radio, audit.y == 0 | is.na(audit.y))

noa <- feols(pvs ~ not_poor_pos*js*audit + coalition_partners.x, cluster = c("a_e_id", "year"), data = zero_ay)
noa_pair_fe <- feols(pvs ~ not_poor_pos*js*audit + coalition_partners.x | a_e_id, cluster = c("a_e_id","year"), data = zero_ay)

etable(noa,noa_pair_fe, digits = 3, tex = F)

noa <- feols(pvs ~ not_poor4*js*audit + coalition_partners.x, cluster = c("a_e_id", "year"), data = zero_ay)
noa_pair_fe <- feols(pvs ~ not_poor4*js*audit + coalition_partners.x | a_e_id, cluster = c("a_e_id","year"), data = zero_ay)

etable(noa,noa_pair_fe, digits = 3, tex = F)

#OLD CODE#

#what about both?
# test$med_both <- ifelse(test$corrupt >= summary(audits$spent_unauthorized_sub)[3] | test$not_poor >= summary(audits$spent_not_poor_sub)[3],1,0)
# 
# m0 <- feols(pvs ~ med_both*js_new*audit + audit_inc, data = radio)
# m1 <- feols(pvs ~ med_both*js_new*audit + audit_inc | audit_id, data = radio)
# m2 <- feols(pvs ~ med_both*js_new*audit + audit_inc | audit_id + year, data = radio)
# m3 <- feols(pvs ~ med_both*js_new*audit + audit_inc | audit_id + ref_mun_id + year, data = radio)
# m4 <- feols(pvs ~ med_both*js_new*audit + audit_inc | a_e_id + year, data = radio)
# m5 <- feols(pvs ~ med_both*js_new*audit + audit_inc | a_e_id + year, data = radio)
# 
# etable(m0,m1,m2,m3,m4,m5) #can't estimate most of these because of colinearity


radio$four_both <- ifelse(radio$corrupt >= summary(audits$spent_unauthorized_sub)[5] | radio$not_poor >= summary(audits$spent_not_poor_sub)[5],1,0)

m0 <- feols(pvs ~ four_both*js_new*audit + audit_inc + coalition_partners.x, data = radio)
m1 <- feols(pvs ~ four_both*js_new*audit + audit_inc + coalition_partners.x | audit_id, data = radio)
m2 <- feols(pvs ~ four_both*js_new*audit + audit_inc + coalition_partners.x | audit_id + ref_mun_id, data = radio)
m3 <- feols(pvs ~ four_both*js_new*audit + audit_inc + coalition_partners.x | audit_id + ref_mun_id + year, data = radio)
m4 <- feols(pvs ~ four_both*js_new*audit + audit_inc + coalition_partners.x | a_e_id, cluster = c("a_e_id","year"), data = radio)
m5 <- feols(pvs ~ four_both*js_new*audit + audit_inc + coalition_partners.x | a_e_id + year, data = radio)

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
m0m <- feols(pvs ~ corruptm*js_new*audit + audit_inc, data = radio)
m1m <- feols(pvs ~ corruptm*js_new*audit + audit_inc | audit_id, data = radio)
m2m <- feols(pvs ~ corruptm*js_new*audit + audit_inc | audit_id + ref_mun_id, data = radio)
m3m <- feols(pvs ~ corruptm*js_new*audit + audit_inc | audit_id + ref_mun_id, cluster = c("a_e_id","year"), data = radio)
m4m <- feols(pvs ~ corruptm*js_new*audit + audit_inc | a_e_id, data = radio)
m5m <- feols(pvs ~ corruptm*js_new*audit + audit_inc | a_e_id, cluster = c("a_e_id","year"), data = radio)

etable(m0m,m1m,m2m,m3m,m5m, digits = 3, tex = F)

summary(m5m)

#check if js_new is messing things up in m5m
# test$js_new_corruptm <- test$js_new*test$corruptm
# test$js_new_audit <- test$js_new*test$audit
# test$js_new_corruptm_audit <- test$js_new*test$corruptm*test$audit
# 
# 
# check <- feols(pvs ~ corruptm*audit + js_new_corruptm + js_new_audit + js_new_corruptm_audit + audit_inc | a_e_id, cluster = c("a_e_id","year"), data = radio)
# 
# etable(m5m,check, digits = 3, tex = F) #it's the same!

#full js
m0mi <- feols(pvs ~ corruptm*js*audit + audit_inc, cluster = c("a_e_id", "year"), data = radio)
m2mi <- feols(pvs ~ corruptm*js*audit + audit_inc | audit_id + ref_mun_id, cluster = c("a_e_id"), data = radio)
m3mi <- feols(pvs ~ corruptm*js*audit + audit_inc | audit_id + ref_mun_id, cluster = c("a_e_id","year"), data = radio)
m4mi <- feols(pvs ~ corruptm*js*audit + audit_inc | a_e_id, data = radio)
m5mi <- feols(pvs ~ corruptm*js*audit + audit_inc | a_e_id, cluster = c("a_e_id","year"), data = radio)

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
# check <- feols(pvs ~ corruptm*audit + js_corruptm + js_audit + js_corruptm_audit + audit_inc | a_e_id, cluster = c("a_e_id","year"), data = radio)
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
m0f <- feols(pvs ~ corrupt4*js_new*audit + audit_inc + coalition_partners.x, data = radio)
m1f <- feols(pvs ~ corrupt4*js_new*audit + audit_inc + coalition_partners.x | audit_id, data = radio)
m2f <- feols(pvs ~ corrupt4*js_new*audit + audit_inc + coalition_partners.x | audit_id + ref_mun_id, data = radio)
m3f <- feols(pvs ~ corrupt4*js_new*audit + audit_inc + coalition_partners.x | audit_id + ref_mun_id, cluster = c("a_e_id","year"), data = radio)
m4f <- feols(pvs ~ corrupt4*js_new*audit + audit_inc + coalition_partners.x | a_e_id, data = radio)
m5f <- feols(pvs ~ corrupt4*js_new*audit + audit_inc + coalition_partners.x | a_e_id, cluster = c("a_e_id","year"), data = radio)

etable(m0f,m1f,m2f,m3f,m4f,m5f, tex = F)

#larreguy spec unauthorized spending
m0f <- feols(pvs ~ corrupt3*js_new*audit + corrupt4*js_new*audit + audit_inc, data = radio)
m1f <- feols(pvs ~ corrupt3*js_new*audit + corrupt4*js_new*audit + audit_inc | audit_id, data = radio)
m2f <- feols(pvs ~ corrupt3*js_new*audit + corrupt4*js_new*audit + audit_inc | audit_id + year, data = radio)
m3f <- feols(pvs ~ corrupt3*js_new*audit + corrupt4*js_new*audit + audit_inc | audit_id + ref_mun_id + year, data = radio)
m4f <- feols(pvs ~ corrupt3*js_new*audit + corrupt4*js_new*audit + audit_inc | a_e_id, data = radio)
m5f <- feols(pvs ~ corrupt3*js_new*audit + corrupt4*js_new*audit + audit_inc | a_e_id + year, data = radio)

etable(m0f,m1f,m2f,m3f,m4f,m5f, tex = F)

#intensive margin
m0f <- feols(pvs ~ corrupt4*js*audit + audit_inc, data = radio)
m1f <- feols(pvs ~ corrupt4*js*audit + audit_inc | audit_id, data = radio)
m2f <- feols(pvs ~ corrupt4*js*audit + audit_inc | audit_id + year, data = radio)
m3f <- feols(pvs ~ corrupt4*js*audit + audit_inc | audit_id + ref_mun_id + year, data = radio)
m4f <- feols(pvs ~ corrupt4*js*audit + audit_inc | a_e_id, data = radio)
m5f <- feols(pvs ~ corrupt4*js*audit + audit_inc | a_e_id + year, data = radio)

etable(m0f,m1f,m2f,m3f,m4f,m5f, tex = F)

#no unauthorized spending
m0no <- feols(pvs ~ corrupt0*js_new*audit + audit_inc, data = radio)
m1no <- feols(pvs ~ corrupt0*js_new*audit + audit_inc | audit_id, data = radio)
m2no <- feols(pvs ~ corrupt0*js_new*audit + audit_inc | audit_id + year, data = radio)
m3no <- feols(pvs ~ corrupt0*js_new*audit + audit_inc | audit_id + ref_mun_id, data = radio)
m4no <- feols(pvs ~ corrupt0*js_new*audit + audit_inc | audit_id + ref_mun_id + year, data = radio)
m5no <- feols(pvs ~ corrupt0*js_new*audit + audit_inc | a_e_id, data = radio)

etable(m0no,m1no,m2no,m3no,m4no,m5no)

#positive non poor spending

test$not_poor_pos <- ifelse(test$not_poor >0,1,0)
test$not_poor4 <- ifelse(test$not_poor >= summary(audits$spent_not_poor_sub)[5],1,0)

m0 <- feols(pvs ~ not_poor_pos*js_new*audit + simult_audit, data = radio)
m1 <- feols(pvs ~ not_poor_pos*js_new*audit + simult_audit | audit_id, data = radio)
m2 <- feols(pvs ~ not_poor_pos*js_new*audit + simult_audit | audit_id + year, data = radio)
m3 <- feols(pvs ~ not_poor_pos*js_new*audit + simult_audit | audit_id + ref_mun_id + year, data = radio)
m4 <- feols(pvs ~ not_poor_pos*js_new*audit + simult_audit | a_e_id, data = radio)
m5 <- feols(pvs ~ not_poor_pos*js_new*audit + simult_audit | a_e_id + year, data = radio)

etable(m0,m1,m2,m3,m4,m5)

#4th quartile
m0 <- feols(pvs ~ not_poor4*js_new*audit + simult_audit, data = radio)
m1 <- feols(pvs ~ not_poor4*js_new*audit + simult_audit | audit_id, data = radio)
m2 <- feols(pvs ~ not_poor4*js_new*audit + simult_audit | audit_id + year, data = radio)
m3 <- feols(pvs ~ not_poor4*js_new*audit + simult_audit | audit_id + ref_mun_id + year, data = radio)
m4 <- feols(pvs ~ not_poor4*js_new*audit + simult_audit | a_e_id, data = radio)
m5 <- feols(pvs ~ not_poor4*js_new*audit + simult_audit | a_e_id + year, data = radio)

etable(m0,m1,m2,m3,m4,m5, tex = F)
