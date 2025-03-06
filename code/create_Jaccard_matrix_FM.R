library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)

fm_mun_intersection <- read.csv("~/mexico_mun/data/fm_mun_intersection.csv")
fm_mun_intersection$INEGI_CODE <- sprintf("%05d", fm_mun_intersection$INEGI_CODE)

test <- subset(fm_mun_intersection, select = c(INEGI_CODE,Distintivo))
test$values <- 1

test_wide <- test %>%
  pivot_wider(names_from = Distintivo, values_from = values)

test_wide[is.na(test_wide)] <- 0

library(vegan)  # For the vegdist function

# Calculate the Jaccard similarity matrix
jaccard_sim <- 1 - as.matrix(vegdist(test_wide[,-1], method = "jaccard"))

row.names(jaccard_sim) <- test_wide$INEGI_CODE

# Adding "md" in front of each element
columns <- paste0("js_", test_wide$INEGI_CODE)
colnames(jaccard_sim) <- columns

js_mat <- as.data.frame(jaccard_sim)
js_mat$mun_id <- rownames(js_mat)

# Pivot longer 
js_mat_long <- js_mat %>% 
  pivot_longer( cols = -mun_id, names_to = "ref_mun_id", values_to = "js" )

js_mat_long$ref_mun_id <- gsub("js_", "", js_mat_long$ref_mun_id)

js <- subset(js_mat_long, ref_mun_id != mun_id)

save(js, file = "~/mexico_mun/data/jaccard_similarity_FM.Rdata")
