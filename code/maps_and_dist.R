# if (!require(devtools)) {
#   install.packages("devtools")
# }
# devtools::install_github('diegovalle/mxmaps')

#library(mxmaps)
library(dplyr)
library(ggplot2)
library(sf)

mex_sf <- read_sf("~/mexico_mun/raw/mun1995shp/Municipios_1995.shp")
load("~/mexico_mun/data/full_dataset_mexelec_pcts.Rdata")

mex_sf$Municipio <- paste(mex_sf$CVE_ENT,mex_sf$CVE_MUN)

df <- subset(big_df, (year>=1995 & year<=1997))
                      #& estado!="Tlaxcala")|(year==1994 & estado=="Tlaxcala"))

df_geom <- merge(df,mex_sf, by = "Municipio", all = TRUE)

ggplot(df_geom) +
  geom_sf(color = "black", aes(geometry = geometry, fill = PAN_pct)) +
  #coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"])) +
  theme_void()

df_geom$win <- ifelse(df_geom$p1_name == "PRI" | df_geom$p1_name == "PRD" | df_geom$p1_name == "PAN", df_geom$p1_name, ifelse(!is.na(df_geom$p1_name),"Other",NA))

df_geom$PANwin <- ifelse(!is.na(df_geom$PAN_pct),ifelse(df_geom$PAN_margin >= 0,1,0),NA)




df_geom_simp <- st_as_sf(df_geom)
df_geom_simp <- rmapshaper::ms_simplify(df_geom_simp)

mun_wins <- ggplot(df_geom_simp) +
  #geom_sf(fill = NA, linewidth = 0.1, color = "white") +
  geom_sf(alpha = 0.6, linewidth = 0.1, aes(fill = as.factor(win), color = as.factor(win))) +
  labs(title = "Municipal Elections, 1995-1997", fill = "", color = "") +
  scale_color_manual( values = c("PAN" = "darkblue", "PRD" = "goldenrod", "PRI" = "darkgreen", "Other" = "black", "NA" = "gray")) +
  scale_fill_manual( values = c("PAN" = "darkblue", "PRD" = "goldenrod", "PRI" = "darkgreen", "Other" = "black", "NA" = "gray")) +
  theme_void() +
  theme(legend.position = "bottom")
print(mun_wins)

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/TYP Final Tables and Figures Appendix/images/PAN_win_map.png", plot = mun_wins, width = 6, height = 4)

# Assuming df is your dataframe
test <- df %>%
  filter(PAN_pct >= 0.5 & !is.na(PAN_pct)) %>%  # Filter rows where PAN_pct is greater than or equal to 0.5
  group_by(estado) %>%        # Group by estado
  summarise(count = ifelse(n()>0,n(),0))      # Count the number of rows in each group


# Identify duplicate rows
duplicates <- df[duplicated(df$region), ]

df1 <- big_df[big_df$year==1997]
df2 <- big_df[big_df$year==1998]
df3 <- big_df[big_df$year==1999]

a <- unique(df1$estado)
b <- unique(df2$estado)
c <- unique(df3$estado)

a
b
c
duplicated(c(a,b,c))

#NOT MANY CHANGES IN MUNICIPALITIES BETWEEN 2000 - 2024!
# mun <- read.delim("C:/Users/adamd/Downloads/AGEEML_202410211926385.txt")
# 
# mun2 <- read.csv("C:/Users/adamd/Downloads/AGEEML_202410261514596.csv")
# 
# test <- subset(mun, select = c(NOM_ENT, NOM_MUN, CVE_MUN, NOM_CAB, CVE_CAB))
# test2 <- subset(mun2, select = c(NOM_ENT, NOM_MUN, CVE_MUN, NOM_CAB, CVE_CAB))
# test2 <- test2 %>% rename(NOM_CAB2 = NOM_CAB, CVE_CAB2 = CVE_CAB, CVE_MUN2 = CVE_MUN)
# 
# test3 <- merge(test,test2, by = c("NOM_ENT","NOM_MUN"), all = T)
# hi <- subset(test3, is.na(CVE_CAB) | is.na(CVE_CAB2))

#Municipios and Cabeceras from ENE 2000
mun <- read.csv("~/mexico_RD/AGEEML_202410261514596.csv")

mun$es_id <- as.character(sapply(mun$CVE_ENT, function(x) sprintf("%02d", x)))
mun$mun_id <- as.character(sapply(mun$CVE_MUN, function(x) sprintf("%03d", x)))
mun$mun_full <- paste0(mun$es_id,mun$mun_id)

#dataset of all locations for cabeceras: https://www.inegi.org.mx/app/ageeml/#
cab <- read.csv("~/mexico_RD/AGEEML_2024102615876.csv")

cab$es_id <- as.character(sapply(cab$CVE_ENT, function(x) sprintf("%02d", x)))
cab$mun_id <- as.character(sapply(cab$CVE_MUN, function(x) sprintf("%03d", x)))
cab$mun_full <- paste0(cab$es_id,cab$mun_id)
cab$CVE_CAB <- as.character(sapply(cab$CVE_LOC, function(x) sprintf("%04d", x)))

small_cab <- subset(cab, select = c(CVE_CAB,mun_full,LATITUD,LONGITUD,LAT_DECIMAL,LON_DECIMAL))

test <- subset(cab, NOM_ENT == "Coahuila de Zaragoza")

mun_ll_full <- merge(mun, small_cab, by = c("CVE_CAB","mun_full"), all.x = T)
summary(mun_ll_full$LAT_DECIMAL)
summary(mun_ll_full$LON_DECIMAL)

mun_ll <- subset(mun_ll_full, !is.na(LAT_DECIMAL) | !is.na(LON_DECIMAL)) #need to find out how to get long/lat of the mexico city municipalities

save(mun_ll, file = "~/mexico_RD/mun_ll.Rdata")

