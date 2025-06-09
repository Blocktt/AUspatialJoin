# Shiny Global File

# Version ----
pkg_version <- "v0.1.2"

# Packages----
library(shiny)
library(shinydashboard)
library(shinydashboardPlus) # only using for footer
library(shinyjs)
library(shinyBS)
library(DT)
library(dplyr)
library(readxl)
library(zip)
library(httr)
library(sf)
library(leaflet)
library(purrr)
library(EPATADA) #https://github.com/USEPA/EPATADA
library(tictoc)

# Source ----
path_results <- "Results"

## tabs ----
db_main_sb                     <- source("external/db_main_sb.R"
                                         , local = TRUE)$value
db_main_body                   <- source("external/db_main_body.R"
                                        , local = TRUE)$value
tab_code_import                <- source("external/tab_import.R"
                                         , local = TRUE)$value
tab_code_tab_AUjoin            <- source("external/tab_AUjoin.R"
                                         , local = TRUE)$value

# Timeout ----
options(timeout = 600)

# Console Message ----
message(paste0("Interactive: ", interactive()))

# File Size ----
# By default, the file size limit is 5MB.
mb_limit <- 200
options(shiny.maxRequestSize = mb_limit * 1024^2)

##  AU Table ----
# df_ML2AU_orig <- as.data.frame(readr::read_csv(file.path("data/MonLoc_to_AU_Crosswalk_20240415.csv")))
# 
# df_ML2AU <- df_ML2AU_orig %>% 
#   select(MonitoringLocationIdentifier, AU_ID, AU_NAME)

url_au_table <- "https://github.com/Blocktt/ShinyAppDocuments/raw/main/AUSpatialJoin"
url_au_table2 <- file.path(url_au_table, "MonLoc_to_AU_Crosswalk_20250407.xlsx")
temp_au_table <- tempfile(fileext = ".xlsx")
httr::GET(url_au_table2, httr::write_disk(temp_au_table))

df_ML2AU_orig <- as.data.frame(readxl::read_excel(temp_au_table))

df_ML2AU <- df_ML2AU_orig %>% 
  select(MonitoringLocationIdentifier, AU_ID, AU_NAME)

## AU Shapefiles ####
load(file = "data/GISlayer_streams.rda")
GISlayer_streams_transformed <- streams_shp %>% 
  sf::st_transform(2818) %>%
  select(AU_ID, AU_NAME, drinkingwa, ecological, fishconsum, recreation
         , other_use) %>%  # trim unneccessary columns
  rename(DrinkingWater_Use = drinkingwa
         , Ecological_Use = ecological
         , FishConsumption_Use = fishconsum
         , Recreational_Use = recreation
         , Other_Use = other_use)
  

load(file = "data/GISlayer_streams_simp.rda")

load(file = "data/GISlayer_lakes.rda")
GISlayer_lakes_transformed <- lakes_shp %>% 
  sf::st_transform(2818) %>%
  select(AU_ID, AU_NAME, drinkingwa, ecological, fishconsum, recreation
         , other_use) %>%  # trim unneccessary columns
  rename(DrinkingWater_Use = drinkingwa
         , Ecological_Use = ecological
         , FishConsumption_Use = fishconsum
         , Recreational_Use = recreation
         , Other_Use = other_use)

## Waterbody types ####
Lake_types <- c("Lake, Reservoir, Impoundment", "Reservoir", "Lake")

Stream_types <- c("Stream", "Stream: Canal", "River/Stream", "Channelized Stream"
                  , "River/Stream Perennial", "River/Stream Intermittent"
                  , "Canal Irrigation", "Canal Drainage")
