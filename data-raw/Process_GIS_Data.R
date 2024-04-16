# Prepare data for use in the package
#
# Ben.Block@tetratech.com
# 2024-04-15
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save GIS shapefiles as RDA
# saves space and should load quicker
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Prep ####
(wd <- getwd()) # assume is package directory
library(sf)
library(dplyr)
library(rmapshaper)
# Get data and process ####
fn_shp <- file.path(wd, "data-raw", "GIS_Data")

## streams ####
streams_shp <- sf::st_read(dsn = fn_shp, layer = "ATTAINS_Lines_MTWY") %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

## streams simp ####
streams_simp_shp <- sf::st_read(dsn = fn_shp, layer = "ATTAINS_Lines_MTWY_Simplify15m") %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

## lakes ####
lakes_shp <- sf::st_read(dsn = fn_shp, layer = "ATTAINS_Areas_MTWY") %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

# Save as RDA for use in app ####
## streams ####
save(streams_shp, file = file.path(wd, "data", "GISlayer_streams.rda"))
save(streams_simp_shp, file = file.path(wd, "data", "GISlayer_streams_simp.rda"))

## lakes ####
save(lakes_shp, file = file.path(wd, "data", "GISlayer_lakes.rda"))

# Helpful links
# https://www.r-bloggers.com/2021/03/simplifying-geospatial-features-in-r-with-sf-and-rmapshaper/
# https://stackoverflow.com/questions/54734771/sf-write-lat-long-from-geometry-into-separate-column-and-keep-id-column
# https://r-spatial.github.io/sf/articles/sf1.html#crs
# https://spatialreference.org/ref/?search=nad+83+massachusetts&srtext=Search