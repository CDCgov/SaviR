library(dplyr)
library(sf)
library(ggplot2)
library(devtools)

load_all()

# Explode France geometry
ot_france <- onetable %>%
  filter(id == "FRA") %>%
  st_as_sf() %>%
  st_cast("POLYGON") %>%
  mutate(id_new = paste0('FRA-',row_number()))

# Visual inspection
ggplot(ot_france) +
  geom_sf(aes(fill = id_new)) +
  theme_void()

# Pull out French Guiana
# add to attributes we already store
ot_guf <- ot_france %>%
  filter(id_new == "FRA-1") %>%
  distinct(id = "GUF", geometry) %>%
  left_join(select(onetable, -geometry), by = "id")

# Drop French Guiana from France
# Coerce to multipolygon again
ot_france <- ot_france %>%
  filter(id_new != "FRA-1") %>%
  select(-id_new) %>%
  st_union() %>%
  st_as_sf() %>%
  rename(geometry = x) %>%
  mutate(id = "FRA") %>%
  left_join(
    select(onetable, -geometry),
    by = "id"
  )

# Join new separate features back to onetable
ot_new <- onetable %>%
  filter(!id %in% c("FRA", "GUF")) %>%
  bind_rows(ot_guf) %>%
  bind_rows(ot_france) %>%
  arrange(id)

# Visual inspection
ot_new %>%
  filter(id %in% c("GUF", "FRA")) %>%
  st_as_sf() %>%
  ggplot() +
    geom_sf(aes(fill = id)) +
    theme_void()

# Update
onetable <- ot_new
usethis::use_data(onetable, overwrite = TRUE)

# --- Handle Country Coords ------------------
country_coords_new <- country_coords %>%
  as_tibble() %>%
  select(-geometry) %>%
  add_row(
    TYPE = NA_character_,
    ADMIN = "French Guiana",
    ISO_A3 = "GUF",
    iso3code = "GUF"
  ) %>%
  left_join(select(onetable, id, geometry), by = c(iso3code = "id")) %>%

  st_as_sf()

country_coords <- country_coords_new
usethis::use_data(country_coords, overwrite = TRUE)


