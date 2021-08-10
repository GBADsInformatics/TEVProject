#!/bin/bash

#############################################
# Author: Gabriel Dennis
# Email: Gabriel.Dennis@csiro.au
#
# This script downloads the FAOSTAT Technical
# conversion factors and translates to english
# must be run from project root
############################################


############################################
# Creates and returns locations to place
# each set of source, processed and output
# data
############################################
data_locations() {

	directory=$"data/${1}/faostat/technical_conversion_factors/"

	if [[ -d $directory ]]
	then
		mkdir -p $directory
	fi

	echo $directory
}

url='https://www.fao.org/fileadmin/templates/ess/documents/methodology/tcf.pdf'
cwd=$(pwd)
# Create and save the correct directory structure
source_dir=$(data_locations source)
processed_dir=$(data_locations processed)
output_dir=$(data_locations output)
file_name='faostat_technical_conversion_factors'

############################################
# Download the pdf file
echo "Downloading the pdf file"
wget  ${url}
mkdir -p ${source_dir}
mv tcf.pdf "${cwd}/${source_dir}${file_name}.pdf"
############################################



############################################
# Convert to a text file which is stored in the
# processed location
############################################
echo "Converting PDF to text"
pdftotext -layout $"${cwd}/${source_dir}${file_name}.pdf" tmp.txt
cp tmp.txt ${cwd}/${processed_dir}${file_name}.txt

############################################
# Translate the PDF to English
############################################
trans file://tmp.txt >> translated.txt

mv translated.txt data/processed/faostat/faostat_technical_conversion_factors/faostat_technical_conversion_factors_translated.txt

wait

echo "Exiting"

exit 0
