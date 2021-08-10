#!/usr/bin/Rscript --vanilla

# ------------------------------------------------------------------------------
# nolint start
# Project: GBADS: Global Burden of Animal Disease
#
# Author: Gabriel Dennis
#
# Position: Research Technician, CSIRO
#
# Email: gabriel.dennis@csiro.au
#
# CSIRO ID: den173
#
# GitHub ID: denn173
#
# Maps FAOSTAT crop codes to CPC codes and in particular
# the crop groupings that were used initially in this analysis.
#
# The only groupings which are included in the crop codes
# are the CPC groupings from Division 01
#
# "Products of Agriculture Horticulture and market Gardening"
#
# In particular, the groupings
# type, code_group
# "cereals", "011",
# "vegetables", "012",
# "fruits_nuts", "013",
# "oilseed_oil_fruits", "014",
# "roots_tubers", "015",
# "coffee_spice_crops", "016",
# "pulses", "017",
# "sugar_crops", "018"
#
# The CPC to FAO Groupings are available at the following URL
#
# - https://www.fao.org/fileadmin/templates/ess/classifications/Correspondence_CPCtoFCL.xlsx
#
# Which is also located at
#
# - data/codes/faostat/CPCtoFCL_codes.xlsx
#
# Which can be re-downloaded using a Makefile recipe and target
# nolint end 
# ------------------------------------------------------------------------------


# Project -----------------------------------------------------------------

renv::activate(".")


# Libraries ---------------------------------------------------------------

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tidyr)
  library(janitor)
  library(LivestockValueGBADS)
})



# Config ------------------------------------------------------------------

config <- config::get()




# The Food groups don't match perfectly
#  - USE the CPC groupings given by
# nolint start
# https://www.fao.org/fileadmin/templates/ess/classifications/Correspondence_CPCtoFCL.xlsx
# nolint end

food_groups <- tribble(
  ~group, ~code,
  "cereals", "011",
  "vegetables", "012",
  "fruits_nuts", "013",
  "oilseed_oil_fruits", "014",
  "roots_tubers", "015",
  "coffee_spice_crops", "016",
  "pulses", "017",
  "sugar_crops", "018"
)

# FAOSTAT Item codes
faostat_item_codes <- config$data$codes$faostat$items$item_codes |>
  readRDS() |>
  clean_names() |>
  sanitize_columns(exclude = c("domain_code"))



# Need to check further how to avoid double counting
#  - Keep as is for now as it seems that the separate codes
#    remove the issues of double counting as these are not aggregated
#    products

crop_groups <- faostat_item_codes |>
  dplyr::filter(domain_code == "QCL") |>
  select(item, item_code, cpc_code) |>
  mutate(
    item_group = substr(cpc_code, start = 1, stop = 3)
  ) |>
  filter(item_group %in% food_groups$code) |>
  left_join(food_groups, by = c("item_group" = "code")) |>
  drop_na(item)


# Write to output codes directories ---------------------------------------

saveRDS(
  crop_groups,
  file.path(
    config$data$codes$faostat$dir,
    "FAOSTAT_CPC_cropItemCodes.rds"
  )
)

saveRDS(
  food_groups,
  file.path(
    config$data$codes$faostat$dir,
    "CPC_cropGroups.rds"
  )
)
