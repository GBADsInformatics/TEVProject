# Makefile to make all analysis targets in this project

document:
	R -q -e 'devtools::document()'

test:
	R -q -e 'devtools::test()'

format:
	R -q -e 'styler::style_pkg()' 1>/dev/null &


.PHONY: figures tables README

# Variables
R_DIR=R/
SCRIPT_DIR=inst/
SOURCE_DATA_DIR=data/source/
METADATA_DIR=data/metadata
PROCESSED_DATA_DIR=data/processed/
OUTPUT_DATA_DIR=data/output/
FIGURE_DIR=output/figures/
TABLE_DIR=output/tables/

#-----------------------------------------
# Data
# Downloads the latest datasets
# ---------------------------------------

# Downloads the FAOSTAT VOP Table
$(PROCESSED_DATA_DIR)faostat/value_of_production.parquet:
	Rscript $(SCRIPT_DIR)/data/data-download.R --data value_of_production

# Downloads the FAOSTAT Producer Prices table
$(PROCESSED_DATA_DIR)faostat/producer_prices.parquet:
	Rscript --vanilla $(SCRIPT_DIR)/data/data-download.R --data producer_prices

# Downloads the FAOSTAT Crops and Livestock Products Table
$(PROCESSED_DATA_DIR)faostat/crops_and_livestock_products.parquet:
	Rscript --vanilla $(SCRIPT_DIR)/data/data-download.R --data crops_and_livestock_products

# Downloads the FAOSTAT GLobal Aquaculture production database
$(SOURCE_DATA_DIR)/faostat/global_aquaculture_production:
	Rscript --vanilla $(SCRIPT_DIR)/data/data-download.R --data global_aquaculture_production

# World bank LCU to PPP conversion
$(OUTPUT_DATA_DIR)world_bank/ppp_conversion.parquet:
	Rscript --vanilla $(SCRIPT_DIR)/data/data-download.R --data ppp_conversion
	Rscript --vanilla $(SCRIPT_DIR)/values/get-world-bank-ppp-conversion.R

# World Bank Population Indicator
$(PROCESSED_DATA_DIR)world_bank/population.parquet:
	Rscript --vanilla $(SCRIPT_DIR)/data/data-download.R --data population


# World Bank GDP Per Capita PPP Indicator
$(PROCESSED_DATA_DIR)world_bank/gdp_per_capita_ppp.parquet:
	Rscript --vanilla $(SCRIPT_DIR)/data/data-download.R --data gdp_per_capita_ppp

# IMF/IFS LCU to USD ($) Exchange Rates (area-weighted)
# Missing values inputted using the IMF/IFC LCU to USD ($)
# official period average exchanged rate
$(OUTPUT_DATA_DIR)world_bank/lcu_conversion.parquet:
	# Area weighed
	Rscript --vanilla $(SCRIPT_DIR)/data/data-download.R --data lcu_conversion
	# Average Official Exchange Rate
	Rscript --vanilla $(SCRIPT_DIR)/data/data-download.R --data lcu_conversion_official
	# Inpute missing values
	Rscript --vanilla $(SCRIPT_DIR)/values/get-world-bank-lcu-usd-conversion.R


FAOSTATConversionFactorData:
	# TODO

#----------------------------------------
# Codes
#----------------------------------------

# Downloads the correspondence file between FAO and CPC codes
data/codes/FAOSTAT/CPCtoFCL_codes.xlsx:
	wget -O data/codes/FAOSTAT/CPCtoFCL_codes.xlsx 'https://www.fao.org/fileadmin/templates/ess/classifications/Correspondence_CPCtoFCL.xlsx'

data/codes/FAOSTAT/FAOSTAT-CPC_cropItemCodes.rds:
	Rscript --vanilla $(SCRIPT_DIR)/codes/get-faostat-crop-codes.R


# Subset Country Codes which are used throughout
data/output/codes/faostat_iso3_country_codes.parquet:
	Rscript --vanilla $(SCRIPT_DIR)/codes/get-project-country-codes.R

#-----------------------------------------
# Analysis
# ---------------------------------------

# Reproduces the livestock value table
$(OUTPUT_DATA_DIR)faostat/faostat_livestock_values.parquet:
	Rscript --vanilla $(SCRIPT_DIR)/values/get-fao-livestock-values.R


# Reproduces the aquaculture value table
# Requires
#  - Global Aquaculture Production database to be downloaded
#  - LCU Conversion
#  - Conversion to 2014-2016 Dollars
$(OUTPUT_DATA_DIR)fao/fao_aquaculture_values.parquet: $(SOURCE_DATA_DIR)/faostat/global_aquaculture_production \
	$(OUTPUT_DATA_DIR)/world_bank/lcu_conversion.parquet
	Rscript --vanilla $(SCRIPT_DIR)/values/get-fao-aquaculture-values.R

# Reproduces the crop values tables
$(OUTPUT_DATA_DIR)faostat/faostat_crop_values.parquet:
	Rscript --vanilla $(SCRIPT_DIR)/values/get-fao-crop-values.R

# Initial Data for the Informatics Dashboard
$(OUTPUT_DATA_DIR)informatics/20220603_informatics_tev_data.parquet:
	Rscript --vanilla $(SCRIPT_DIR)/values/get-info-data.R


# Generate metadata in the GBADS format for all files
$(METADATA_DIR):
	Rscript  $(SCRIPT_DIR)/metadata/get-gbads-metadata.R


#--------------------------------------
# Figures and tables
# -------------------------------------
$(FIGURE_DIR)figure_2.png: $(SCRIPT_DIR)figures/figure_2.R 
	Rscript $(SCRIPT_DIR)figures/figure_2.R 1>/dev/null &

$(FIGURE_DIR)figure_3.png: $(SCRIPT_DIR)figures/figure_3.R
	Rscript $(SCRIPT_DIR)figures/figure_3.R 1>/dev/null &

$(FIGURE_DIR)figure_4.png: $(SCRIPT_DIR)figures/figure_4.R
	Rscript $(SCRIPT_DIR)figures/figure_4.R 1>/dev/null &

$(FIGURE_DIR)figure_5.png: $(SCRIPT_DIR)figures/figure_5.R
	Rscript  $(SCRIPT_DIR)figures/figure_5.R 1>/dev/null &

$(FIGURE_DIR)figure_6.png:
	Rscript $(SCRIPT_DIR)figures/figure_6.R 1>/dev/null &

$(FIGURE_DIR)figure_7.png:
	Rscript $(SCRIPT_DIR)figures/figure_7.R 1>/dev/null &

$(FIGURE_DIR)figure_A1.png:
	Rscript $(SCRIPT_DIR)figures/figure_A1.R 1>/dev/null &

$(FIGURE_DIR)figure_A2.png:
	Rscript $(SCRIPT_DIR)figures/figure_A2.R 1>/dev/null &


# Make all figure
figures:
	$(FIGURE_DIR)figure_2.png  \
		$(FIGURE_DIR)figure_3.png     \
		$(FIGURE_DIR)figure_6.png     \
		$(FIGURE_DIR)figure_7.png     \
		$(FIGURE_DIR)figure_4.png     \
		$(FIGURE_DIR)figure_A2.png    \
		$(FIGURE_DIR)figure_A1.png    \
		$(FIGURE_DIR)figure_5.png     

.PHONY: export_figures
export_figures: 
	@echo "Exporting figures to png and tiff files --------------"
	@find output/figures -maxdepth 1 -name "figure*[png|tiff]" | xargs zip -vj exported_figures.zip
	@echo "Finished Exporting ------------------------------------"



# Make tables
tables:
	Rscript --vanilla $(SCRIPT_DIR)tables/generate-tables.R


# Any RMD files in the output folder
README:
	R -q -e 'rmarkdown::render("output/tables/README.rmd")' 1>/dev/null &


#------------------------------------
# Tests
#------------------------------------
tests:
	# Make tests
