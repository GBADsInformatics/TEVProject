#!/usr/bin/Rscript --vanilla

###################################################
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
# Date Created:  20220307
#
# Description:  This script downloads faostat and fao livestock and aquaculture
# datasets as bulk zip files, and extracts them into the data directory  as
# parquet files for easier storage.
#
# This uses the URLs and locations specified for both the input and output
# data are in the project configuration file: config.yml
####################################


# Load Libraries ----------------------------------------------------------
renv::activate(project = ".")


# Logging -----------------------------------------------------------------
logging::basicConfig()


# Project Configurations --------------------------------------------------
config <- config::get()


# Parse Command Line Arguments for different data sources -----------------
parser <- argparse::ArgumentParser(
    description = "Downloads required datasets"
)

parser$add_argument("-d", "--data",
                    help = "Name of which dataset to download.",
                    choices = names(config$data$source$tables),
                    required = TRUE)

logging::loginfo("Parsing command line arguments ...")
args <- parser$parse_args()
logging::loginfo(args)


# Download the data -------------------------------------------------------
table_config <- config$data$source$tables[[args$data]]
data_dir <- table_config$dir

destdir <- here::here(data_dir)

if (!dir.exists(destdir)) {
    dir.create(destdir, recursive = TRUE)
}

destfile <-  here::here(destdir,
                        paste0(config$date, "_",
                               args$data, ".",
                               table_config$type))

## Download files
tryCatch({
    logging::loginfo("Downloading: %s", table_config$url)
    download.file(
        table_config$url,
        destfile = destfile,
        quit  = TRUE
    )
},
error = function(err) {
    logging::logerror(err)
}
)




# Unzip and save to the data directory ------------------------------------
if (table_config$type == "zip") {
    unzip(destfile,
          exdir = tools::file_path_sans_ext(destfile),
          junkpaths = TRUE
    )
}




# If Faostat File - Read in and place in a Parquet File -------------------
if (args$data != "global_aquaculture_production") {

    # Data file should have normalized in the file path when from FAO
    # WDI has ^API at the start of the file name
    csv_file <- grep("Normalized|API", list.files(
        tools::file_path_sans_ext(destfile),
        full.names = TRUE), value = TRUE, perl = TRUE)[1]

    # If from the WDI skip the 4 top empty rows
    wb_row_skip <- 4
    data <- read.csv(csv_file,
                     skip = ifelse(grepl("API", csv_file),
                                   wb_row_skip,  0)) |>
        janitor::clean_names()
    output_file <- here::here(
        paste0(gsub("source", "processed", table_config$dir), ".parquet"))
    if (!dir.exists(dirname(output_file))) {
        dir.create(dirname(output_file))
    }
    arrow::write_parquet(data, output_file)
}




# Exit --------------------------------------------------------------------
logging::loginfo("Data Download Complete")
quit(status = 0)
