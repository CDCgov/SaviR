library(tidyverse)
library(devtools)
load_all()
#Get One Table
onetable.1 <- onetable %>%
  st_as_sf() %>%
  st_cast("MULTIPOLYGON")
str(onetable.1)

#Pull out Russia
onetable.russia  <- onetable.1 %>%
  filter(id=="RUS")
#Pull out Ukraine
onetable.ukraine <- onetable.1 %>%
  filter(id=="UKR")
  
#Split Russia from multi-part into multiple single-part features
onetable.russia.multi <- onetable.russia %>%
  st_cast("POLYGON") %>%
  mutate(id_new = paste0('RUS-',row_number()))
#PLot Russia to visually identify which part is Crimea
ggplot(onetable.russia.multi) +
  geom_sf(aes(fill=id_new)) +
  coord_sf(xlim = c(1000000, 5000000), ylim = c(4000000, 8000000)) +
  scale_fill_brewer(palette="Set1")
#Pull out Crimea (id_new=='RUS-14')
crimea <- onetable.russia.multi %>%
  filter(id_new == "RUS-14") %>%
  select(id, geometry) %>%
  mutate(id = "UKR") %>%
  #Pull in Ukraine's Attributes
  left_join(onetable.ukraine %>%
              as_tibble() %>%
              select(-geometry), by="id")
  
  
#Convert Russia back to a single multi-part feature
onetable.russia.noCrimea <- onetable.russia.multi %>%
  filter(id_new != "RUS-14") %>%
  select(id, geometry) %>%
  st_union() %>%
  st_as_sf() %>%
  rename(geometry = x) %>%
  mutate(id = "RUS") %>%
  #Pull in Russia's Attributes
  left_join(onetable.russia %>% 
              as_tibble() %>%
              select(-geometry), by="id")
#Merge Crimea with Ukraine
onetable.ukraine.withCrimea <- onetable.ukraine %>%
  bind_rows(crimea) %>%
  st_union() %>%
  st_as_sf() %>%
  rename(geometry = x) %>%
  mutate(id = "UKR") %>%
  #PUll in Ukraine's Attributes
  left_join(onetable.ukraine %>%
              as_tibble() %>%
              select(-geometry), by="id")
  

#Create new version of onetable
onetable.new <- onetable %>%
  filter(!(id %in% c("UKR","RUS"))) %>%
  bind_rows(onetable.russia.noCrimea) %>%
  bind_rows(onetable.ukraine.withCrimea) %>%
  arrange(id)

ggplot(onetable.new) +
  geom_sf(aes(fill=id, geometry=geometry)) +
  coord_sf(xlim = c(1000000, 5000000), ylim = c(4000000, 8000000)) +
  theme_void() +
  theme(legend.position = "none")

# Visual check against old
ggplot(onetable) +
  geom_sf(aes(fill=id, geometry=geometry)) +
  coord_sf(xlim = c(1000000, 5000000), ylim = c(4000000, 8000000)) +
  theme_void() +
  theme(legend.position = "none")

# Update
onetable <- onetable.new
usethis::use_data(onetable, overwrite = TRUE)

# --- Handle country coords ---------------------
country_coords_new <- country_coords %>%
  as_tibble() %>%
  select(-geometry) %>%
  left_join(select(onetable, id, geometry), by = "id") %>%
  st_as_sf()

country_coords <- country_coords_new
usethis::use_data(country_coords, overwrite = TRUE)
