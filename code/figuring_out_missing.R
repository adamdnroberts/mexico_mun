#load elections data
load("~/mexico_RD/full_dataset_mexelec.Rdata")
mex_sf <- read_sf("~/mexico_RD/mun1995shp/Municipios_1995.shp")
mex_sf$mun_id <- gsub(" ", "", paste(mex_sf$CVE_ENT,mex_sf$CVE_MUN))

df <- subset(big_df, year>=1995 & year <= 1997)
df$mun_id <- gsub(" ", "", df$Municipio)

length(unique(df$mun_id))

unique_pairs <- df %>% distinct(estado, year)

#figure out missing municipalities
one <- df$mun_id[!is.na(df$PAN_pct)]
two <- df$mun_id[!is.na(df$next_PAN_pct)]

three <- setdiff(one,two)
estados <- substr(three, 1, 2)

subset_df <- big_df[substr(big_df$mun_id, 1, 2) == "07", ]
test <- subset(subset_df, is.na(next_PAN_pct))

#why missing from shape file?
test <- subset(df, mun_id %in% missing_muns2)
summary(test$PAN_pct) #all NAs, so we're good

#which municipios missing next_PAN_pct?
mex_sf$missing <- as.factor(ifelse(mex_sf$mun_id %in% three,1,0))

ggplot(mex_sf) +
  geom_sf(color = "black", aes(geometry = geometry, fill=missing)) +
  theme_void()

#which missing PAN_pct?
mex_sf$missing <- as.factor(ifelse(mex_sf$mun_id %in% one,0,1))

ggplot(mex_sf) +
  geom_sf(color = "black", aes(geometry = geometry, fill=missing)) +
  theme_void()

hi <- subset(big_df, year == 1995 & estado == "Puebla") #looks like this is the same as the municipal government numbers from puebla's website (I only checked like 5, but also tweeted at CIDAC so hopefully they'll reply)

#what years are elections?
variable.names(big_df)

# Load the dplyr package
library(dplyr)

# Use distinct() to get unique pairs
unique_pairs <- df %>% distinct(estado, year)

subset(unique_pairs, estado == "Puebla")

# Group by estado and count the number of unique years
estado_counts <- unique_pairs %>%
  group_by(estado) %>%
  summarise(year_count = n_distinct(year))

# Filter to find estados with exactly two unique years
estados_with_two_years <- estado_counts$estado[estado_counts$year_count == 2]

df <- subset(big_df, year>=1994 & year <= 1999 & estado %in% estados_with_two_years)
df$mun_id <- gsub(" ", "", df$Municipio)

length(unique(df$mun_id))


