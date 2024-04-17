# Shiny Global File

# Version ----
pkg_version <- "v0.1.1"

# Packages----
# nolint start
library(BCGcalc)
library(BioMonTools)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus) # only using for footer
library(shinyjs)
library(shinyBS)
library(DT)
library(dplyr)
library(readxl)
library(httr)
library(reshape2)
library(knitr)
library(zip)
library(ComplexUpset)
library(shinyalert)#ok
library(readr)#ok
library(rmarkdown) #ok
library(tools)#ok
library(openxlsx) #ok
# nolint end

# Source ----

# Helper Functions ----
source(file.path("scripts", "helper_functions.R"))

## tabs ----
# sourced in global.R
# ref in db_main_body.R
# menu in db_main_sb.R
db_main_sb                     <- source("external/db_main_sb.R"
                                         , local = TRUE)$value
db_main_body                   <- source("external/db_main_body.R"
                                        , local = TRUE)$value
tab_code_about                 <- source("external/tab_about.R"
                                         , local = TRUE)$value
tab_code_import                <- source("external/tab_import.R"
                                         , local = TRUE)$value
tab_code_filebuilder           <- source("external/tab_filebuilder.R"
                                         , local = TRUE)$value
tab_code_filebuilder_intro           <- source("external/tab_filebuilder_intro.R"
                                         , local = TRUE)$value
tab_code_filebuilder_outsideapp           <- source("external/tab_filebuilder_outsideapp.R"
                                         , local = TRUE)$value
tab_code_filebuilder_taxatrans <- source("external/tab_filebuilder_taxatrans.R"
                                         , local = TRUE)$value
tab_code_calc_bcg              <- source("external/tab_calc_bcg.R"
                                         , local = TRUE)$value
tab_code_resources             <- source("external/tab_resources.R"
                                         , local = TRUE)$value

# Console Message ----
message(paste0("Interactive: ", interactive()))

# File Size ----
# By default, the file size limit is 5MB.
mb_limit <- 200
options(shiny.maxRequestSize = mb_limit * 1024^2)

# Folders----
path_data <- file.path("data")
path_results <- file.path("results")

# ensure results folder exists
if (dir.exists(path_results) == FALSE) {
  dir.create(path_results)
} else {
  message(paste0("Directory already exists; ", path_data))
}## IF ~ dir.exists

# File and Folder Names ----
abr_filebuilder <- "FB"
abr_taxatrans   <- "TaxaTranslator"
abr_bcg         <- "BCG"
abr_results     <- "results"

dn_files_input  <- "_user_input"
dn_files_ref    <- "reference"
dn_files_fb     <- paste(abr_results, abr_filebuilder, sep = "_")
dn_files_bcg    <- paste(abr_results, abr_bcg, sep = "_")

# Selection Choices----
sel_community <- c("CREMP_KEYS", "NOT_CREMP_KEYS")

##  BCG Models ----
url_bcg_base <- "https://github.com/leppott/BCGcalc/raw/main/inst/extdata"

url_bcg_models <- file.path(url_bcg_base, "Rules.xlsx")
temp_bcg_models <- tempfile(fileext = ".xlsx")
httr::GET(url_bcg_models, httr::write_disk(temp_bcg_models))

df_bcg_models <- as.data.frame(readxl::read_excel(temp_bcg_models
                                                  , guess_max = 10^3
                                                  , sheet = "Rules"))
#sel_bcg_models <- sort(unique(df_bcg_models$Index_Name))
sel_bcg_models <- "FL_Coral_BCG"

## URL BioMonTools
url_bmt_base <- "https://github.com/leppott/BioMonTools_SupportFiles/raw/main/data"

# BMT, Flags ----
url_bcg_checks <- file.path(url_bcg_base, "MetricFlags.xlsx")
temp_bcg_checks <- tempfile(fileext = ".xlsx")
httr::GET(url_bcg_checks, httr::write_disk(temp_bcg_checks))

df_checks <- as.data.frame(readxl::read_excel(temp_bcg_checks, sheet = "Flags"))

# BMT, Taxa Official Pick----
url_taxa_official_pick <- file.path(url_bmt_base
                                    , "taxa_official"
                                    , "FLCoral_BCG_Pick_Files.csv")
temp_taxa_official_pick <- tempfile(fileext = ".csv")
httr::GET(url_taxa_official_pick, httr::write_disk(temp_taxa_official_pick))

df_pick_taxoff <- read.csv(temp_taxa_official_pick)

# BMT, Metric Names ----
url_bmt_pkg <- "https://github.com/leppott/BioMonTools/raw/main/inst/extdata"
url_metricnames <- file.path(url_bmt_pkg, "MetricNames.xlsx")
temp_metricnames <- tempfile(fileext = ".xlsx")
httr::GET(url_metricnames, httr::write_disk(temp_metricnames))

df_metricnames <- readxl::read_excel(temp_metricnames
                                     , sheet = "MetricMetadata"
                                     , skip = 4)

# BMT, Metric Scoring ----
url_bmt_pkg <- "https://github.com/leppott/BioMonTools/raw/main/inst/extdata"
url_metricscoring <- file.path(url_bmt_pkg, "MetricScoring.xlsx")
temp_metricscoring <- tempfile(fileext = ".xlsx")
httr::GET(url_metricscoring, httr::write_disk(temp_metricscoring))
