library(sf)
library(ggplot2)
library(dplyr)

road4 <- read_sf("C:/Users/adamd/Downloads/cdv_cvu_ed_1_nl/red_de_carreteras_19_nl.shp") #Dec 2011 NL Roads

ggplot(road4) +
  geom_sf(color = "black", aes(geometry = geometry)) +
  theme_void()

longest_object <- road4 %>%
  mutate(length = st_length(geometry)) %>%
  filter(length == max(length))

longest_object$length <- NULL

ggplot(road4) +
  geom_sf(color = "black", aes(geometry = geometry)) +
  geom_sf(color = "blue", aes(geometry = geometry), data = longest_object) +
  theme_void()
