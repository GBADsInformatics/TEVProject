#! /usr/bin/env Rscript --vanilla

## --------------------------------------------
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
# Date Created:  20220204
#
# Description:
# This script downloads, translates
# and extracts values from the FAOSTATs
# technical conversion factors for agricultural
# commodities.
#
# Url of PDF:
#  -https://www.fao.org/fileadmin/templates/ess/documents/methodology/tcf.pdf
#
# This script uses several command line tools to parse and translate the file
# these include
# pdftotext version 0.86.1
# Copyright 2005-2020 The Poppler Developers - http://poppler.freedesktop.org
# Copyright 1996-2011 Glyph & Cog, LLC
# and
#
# Translate Shell       0.9.6.12-release
#
# platform              Linux
# terminal type         screen
# bi-di emulator        [N/A]
# gawk (GNU Awk)        5.0.1
# fribidi (GNU FriBidi) [NOT INSTALLED]
# audio player          [NOT INSTALLED]
# terminal pager        less
# web browser           xdg-open
# user locale           C.UTF-8 (Emoji)
# home language         emj
# source language       auto
# target language       emj
# translation engine    google
# proxy                 [NONE]
# user-agent            Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/81.0.4044.138 Safari/537.36
# ip version            [DEFAULT]
# theme                 default
# init file             [NONE]
#
# Report bugs to:       https://github.com/soimort/translate-shell/issues
# nolint end 
## --------------------------------------------

renv::activate(project = ".")

library(dplyr)
library(countrycode)
library(glue)

# Config ------------------------------------------------------------------

tcf_file <- file.path("data", "processed", "faostat",
  "faostat_technical_conversion_factors",
  "faostat_technical_conversion_factors_translated.txt")



# Read In the Translated Text ---------------------------------------------

data <- read.delim(tcf_file) |>
  janitor::clean_names() |>
  dplyr::rename(
    tcf = technical_conversion_factors
  ) |>
  dplyr::mutate(
    tcf = tolower(tcf)
  ) |>
  dplyr::filter(
    !stringr::str_detect(tcf, "page \\d"),
    tcf != "technical conversion factors"
  )

# Get the country names ---------------------------------------------------

# Should be directly after the country name
countries_match <- stringr::str_which(data$tcf, "^crops") - 1





# Get all the matches for
# single lines which say
# "livestock products"
#
# The next line will always then say "meat"

livestock_products_matches <- which(
  stringr::str_detect(data$tcf, "^livestock products$")
)

# The ending of the livestock products matching will say "$edible"
edible_matches <- which(
  stringr::str_detect(data$tcf, "^edible")
)

df_list <- vector("list", length = length(livestock_products_matches))
k <- 1
for (i in livestock_products_matches) {

  # End of the Live Weight Table
  next_edible <- min(edible_matches[edible_matches > i]) - 1

  # Name of the country
  country_name <- data$tcf[max(countries_match[countries_match < i])]

  # First line containing data
  first_num_line <- i + stringr::str_which(data$tcf[(i + 1):next_edible], "\\d")

  # Save the data
  tmp_df <- data[first_num_line:next_edible, ]

  # Split the data to get the animal
  tmp_df <- stringr::str_split_fixed(tmp_df, "\\s+", 4)

  # Turn into a data frame
  tmp_df <- as.data.frame(tmp_df)

  # Set names
  names(tmp_df) <- c(
    "animal",
    "average_live_weight",
    "carcass_weight_kg_gr",
    "carcass_pct"
  )

  # Convert to numerics
  tmp_df <- dplyr::mutate_at(
    tmp_df, vars(average_live_weight:carcass_pct),
    ~ as.numeric(.x)
  )

  tmp_df$country <- country_name

  df_list[[country_name]] <- tmp_df
  k <- k + 1
}

df <- bind_rows(df_list)

# Make sure that all are parsed correctly
#
# If over > 850 and only a single non NA value
# then it is the carcass_weight
# Otherwise it is the live weight
na_counts <- which(rowSums(is.na(df)) == 2)
small_animals <- c("ducks", "rabbits", "geese", "turkeys", "chicken")
for (na_row in na_counts) {
  if (!is.na(df$average_live_weight[na_row]) &&
    df$animal[na_row] %in% small_animals) {
    val <- df$average_live_weight[na_row]
    df$average_live_weight[na_row] <- NA
    df$carcass_weight_kg_gr[na_row] <- val
  } else if (is.na(df$average_live_weight[na_row]) &&
    !is.na(df$carcass_weight_kg_gr[na_row])) {
    val <- df$carcass_weight_kg_gr[na_row]
    df$average_live_weight[na_row] <- val
    df$carcass_weight_kg_gr[na_row] <- NA
  }
}


# Assert that the carcass_pct is between 0 and 1
# Non poultry is between  1 and 1000
# poultry is between 1000 and 10000
# Guam at 115 for goats is not necassarily an error
# But it will be removed
aggregate(
  cbind(
    average_live_weight,
    carcass_weight_kg_gr,
    carcass_pct
  ) ~ animal,
  data = df,
  FUN = summary
)

df <- dplyr::filter(df, carcass_pct <= 100)


# Attach ISO3 codes -------------------------------------------------------

df$iso3_code <- countrycode::countrycode(df$country, "country.name.en", "iso3c")
# nolint start
# Warning message:
# In countrycode_convert(sourcevar = sourcevar, origin = origin, destination = dest,  :
#    Some values were not matched unambiguously: belgium-luxembourg,
#    benign, cap-vert, meeting, netherlands antille, rep. central african,
#    suisse, the savior, yugoslavia,fed.rep.
# nolint end 


# Fix these countries that were translated incorrectly
df$country <- dplyr::recode(df$country,
  "the savior" = "el salvador",
  "benign" = "benin",
  "suisse" = "switzerland",
  "meeting" = "reunion",
  "cap-vert" = "cape verde",
  "rep. central african" = "central african republic"
)

# Rematch
df$iso3_code <- countrycode::countrycode(df$country, "country.name.en", "iso3c")

# drop the rest
df <- tidyr::drop_na(df, iso3_code)

# Convert incorrectly translated name "horsepower" to "horses"
df$animal <- dplyr::recode(
  df$animal,
  "horsepower" = "horses"
)



# Standardise animal names ------------------------------------------------
df$animal <- dplyr::recode(df$animal,
  "buffaloes" = "buffalo",
  "camels" = "camel",
  "hens" = "chicken",
  "chickens" = "chicken",
  "goats" = "goat",
  "ducks" = "duck",
  "horses" = "horse",
  "pigs" = "pig",
  "turkeys" = "turkey",
  "rabbits" = "rabbit",
  "geese" = "goose"
)



# Standardise the animal measurement units to KG --------------------------

# Animals which have both their live weights and carcass weights in grams
# These are mostly small animals/poultry where this difference in weight
# can have a marginal difference
gram_animals <- c("chicken", "duck", "goose", "rabbit", "turkey")


# Divide the results by 1e3 = 1000 to convert to kg
df <- df %>%
  dplyr::mutate(
    live_weight_kg = case_when(
      animal %in% gram_animals ~ average_live_weight / 1e3,
      TRUE ~ average_live_weight
    ),
    carcass_weight_kg = if_else(
      animal %in% gram_animals,
      carcass_weight_kg_gr / 1e3,
      carcass_weight_kg_gr
    )
  ) |>
  dplyr::select(
    iso3_code,
    country,
    animal,
    live_weight_kg,
    carcass_weight_kg,
    carcass_pct
  )




# Impute regional medians for any missing values---------------------------

# Add regions
df <- df |>
  dplyr::mutate(
    region_23 = countrycode(iso3_code, "iso3c", "region23"),
    continent = countrycode(iso3_code, "iso3c", "continent")
  )

# Get different dataframes for each column of interest
live_weight <- df |>
  dplyr::select(
    iso3_code,
    country,
    animal,
    live_weight_kg,
    region_23,
    continent) |>
  tidyr::spread(animal, live_weight_kg) |>
  tidyr::gather(
    key = "animal", value = "live_weight_kg",
    -iso3_code, -country, -region_23, -continent
  )

carcass_weight <- df |>
  dplyr::select(iso3_code,
    country,
    animal,
    carcass_weight_kg,
    region_23,
    continent) |>
  tidyr::spread(animal, carcass_weight_kg) |>
  tidyr::gather(
    key = "animal", value = "carcass_weight_kg",
    -iso3_code, -country, -region_23, -continent
  )

carcass_pct <- df |>
  dplyr::select(iso3_code,
    country,
    animal,
    carcass_pct,
    region_23,
    continent) |>
  tidyr::spread(animal, carcass_pct) |>
  tidyr::gather(
    key = "animal", value = "carcass_pct",
    -iso3_code, -country, -region_23, -continent
  )

# Function to add the imputed median columns
add_imputed_median <- function(df, agg_region, var_name, world = FALSE) {
  if (!world) {
    df %>%
      dplyr::group_by(animal, {{ agg_region }}) %>%
      dplyr::mutate(
        "{{ var_name }}_{{ agg_region }}_median" := median({{ var_name }}, na.rm = TRUE)
      ) |>
      dplyr::ungroup()
  } else {
    df %>%
      dplyr::group_by(animal) %>%
      dplyr::mutate(
        "{{ var_name }}_global_median" := median({{ var_name }}, na.rm = TRUE)
      ) |>
      dplyr::ungroup()
  }
}


carcass_pct_imp <- carcass_pct |>
  add_imputed_median(region_23, carcass_pct) |>
  add_imputed_median(continent, carcass_pct) |>
  add_imputed_median(continent, carcass_pct, world = TRUE) |>
  dplyr::mutate(
    carcass_pct_imp = case_when(
      !is.na(carcass_pct) ~ carcass_pct,
      !is.na(carcass_pct_region_23_median) ~
        carcass_pct_region_23_median,
      !is.na(carcass_pct_continent_median) ~
        carcass_pct_continent_median,
      TRUE ~ carcass_pct_global_median
    ),
    carcass_pct_notes = case_when(
      !is.na(carcass_pct) ~ "",
      !is.na(carcass_pct_region_23_median) ~
        sprintf("Imputed region 23 median %.2f", carcass_pct_region_23_median),
      !is.na(carcass_pct_continent_median) ~
        sprintf("Imputed continental median %.2f",
        carcass_pct_continent_median),
      TRUE ~ sprintf("Imputed global median %.2f", carcass_pct_global_median)
    )
  )


live_weight_imp <- live_weight |>
  add_imputed_median(region_23, live_weight_kg) |>
  add_imputed_median(continent, live_weight_kg) |>
  add_imputed_median(continent, live_weight_kg, world = TRUE) |>
  dplyr::mutate(
    live_weight_kg_imp = case_when(
      !is.na(live_weight_kg) ~ live_weight_kg,
      !is.na(live_weight_kg_region_23_median) ~
        live_weight_kg_region_23_median,
      !is.na(live_weight_kg_continent_median) ~
        live_weight_kg_continent_median,
      TRUE ~ live_weight_kg_global_median
    ),
    live_weight_kg_notes = case_when(
      !is.na(live_weight_kg) ~ "",
      !is.na(live_weight_kg_region_23_median) ~
        sprintf("Imputed region 23 median %.2f",
        live_weight_kg_region_23_median),
      !is.na(live_weight_kg_continent_median) ~
        sprintf("Imputed continental median %.2f",
        live_weight_kg_continent_median),
      TRUE ~ sprintf("Imputed global median %.2f", live_weight_kg_global_median)
    )
  )



carcass_weight_imp <- carcass_weight |>
  add_imputed_median(region_23, carcass_weight_kg) |>
  add_imputed_median(continent, carcass_weight_kg) |>
  add_imputed_median(continent, carcass_weight_kg, world = TRUE) |>
  dplyr::mutate(
    carcass_weight_kg_imp = case_when(
      !is.na(carcass_weight_kg) ~ carcass_weight_kg,
      !is.na(carcass_weight_kg_region_23_median) ~
        carcass_weight_kg_region_23_median,
      !is.na(carcass_weight_kg_continent_median) ~
        carcass_weight_kg_continent_median,
      TRUE ~ carcass_weight_kg_global_median
    ),
    carcass_weight_kg_notes = case_when(
      !is.na(carcass_weight_kg) ~ "",
      !is.na(carcass_weight_kg_region_23_median) ~
        sprintf("Imputed region 23 median %.2f",
        carcass_weight_kg_region_23_median),
      !is.na(carcass_weight_kg_continent_median) ~
        sprintf("Imputed continental median %.2f",
        carcass_weight_kg_continent_median),
      TRUE ~ sprintf("Imputed global median %.2f",
        carcass_weight_kg_global_median)
    )
  )


# Bind together
df_imp <- purrr::reduce(
  list(live_weight_imp, carcass_weight_imp, carcass_pct_imp),
  full_join
)

# Save example summary statistics -----------------------------------------
library(ggplot2)
# Carcass pct per animal per continent

p <- ggplot2::ggplot(
  tidyr::drop_na(carcass_pct, carcass_pct),
  aes(x = continent, y = carcass_pct)
) +
  ggplot2::geom_boxplot() +
  facet_wrap(~animal) +
  labs(
    title = "FAOSTAT Carcass Percentage per Animal and continent"
  )
ggsave(p, file = "output/figures/tcf/tcf_carcass_pct_per_continent.png")

p <- ggplot2::ggplot(
  tidyr::drop_na(live_weight, live_weight_kg),
  aes(x = continent, y = live_weight_kg)
) +
  ggplot2::geom_boxplot() +
  facet_wrap(~animal) +
  labs(
    title = "FAOSTAT Carcass Live Weight per Animal and continent"
  )
ggsave(p, file = "output/figures/tcf/tcf_live_weight_per_continent.png")

# Save the outputs --------------------------------------------------------

# raw data
arrow::write_parquet(
  df,
  "data/output/faostat/faostat_technical_conversion_factors.parquet"
)

# Imputed data
arrow::write_parquet(
  df_imp,
  "data/output/FAOSTAT/faostat_technical_conversion_factors_imputed.parquet"
)
