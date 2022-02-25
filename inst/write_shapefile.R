# =============================================================================== #
# Write-out shapefile for plotting in PowerBI or other sources                    #
# =============================================================================== #
library(dplyr)
library(sf)
devtools::load_all()

onetable %>%
  select(id, geometry) %>%
  st_as_sf() %>%
  st_transform(4326) %>%
  st_write("inst/extdata/onetable.geojson")