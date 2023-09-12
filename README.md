# Citizen Science Image Dataset of European Moths 
## Description
This repository contains scripts and resources for collecting and formatting a dataset of expert-curated moth species with citizen science images from GBIF. The dataset is intended for pretraining of models for automatic monitoring of moths on AMI traps in connection with Biodiversa+.

## Limitations
This expert-curated species-list which is used by this repository does not contain all species, and certainly not all species-location pairings.

## Usage
An RMarkdown file for creating a reproducibility reprot can be found in the "Reproducibility" folder.

### Step-by-step guide

1. Follow the guide for downloading and filtering the Global Administrative Boundaries (**GADM**) dataset and save the resulting file should be saved in a subdirectory of this repository called "GADM" and be named "GADM/GADM_EuropeRussia.shp". This can be altered by modifying the "Preprocessing/fix_countries_for_GBIF.R" script.
2. Run the "Preprocessing/fix_countries_for_GBIF.R" script. This script provides geographic name resolution for the expert-curated species-list area names. 
