# Citizen Science Image Dataset of European Moths 
## Description
This repository contains scripts and resources for collecting and formatting a dataset of expert-curated moth species with citizen science images from GBIF. The dataset is intended for pretraining of models for automatic monitoring of moths on AMI traps in connection with Biodiversa+.

## Limitations
This expert-curated species-list which is used by this repository does not contain all species, and certainly not all species-location pairings.

## Usage
An RMarkdown file for creating a reproducibility reprot can be found in the `Reproducibility` folder.

### Step-by-step guide
**ALL** scripts should be run with the root directory (of the repository) as the working directory. Step 1 and 2 may be skipped, the results are saved as part of the repository for your sanity.
1. Follow the guide for downloading and filtering the Global Administrative Boundaries (**GADM**) dataset and save the resulting file should be saved in a subdirectory of this repository called `GADM` and be named `GADM/GADM_EuropeRussia.shp`. This can be altered by modifying the `Preprocessing/fix_countries_for_GBIF.R` script.
2. Run the `Preprocessing/fix_countries_for_GBIF.R` script. This script provides geographic name resolution for the expert-curated species-list area names.
3. Run the `Preprocessing/GBIF_api_handling.R` script. This script handles the construction of API requests for the GBIF Download API, for each of the areas found in the expert-curated list, as well as posting, waiting for the response to be processed and fetching and saving the result. The script also makes sure to log errors that may happen, for example if your machine loses internet connection or turns off. Simply re-run the script in that case, the script will automatically skip the already completed requests, to avoid spamming the GBIF API, and save time. OBS: This script contains many comments that are meant to be read, for example it needs user input otherwise it will not work.
4. Run the `Preprocessing/SummarizeGBIFData.R` script. This scripts combines all of the results from the GBIF API with the GADM features for easy data visualization purposes and further use. The resulting files are *quite* large.
5. If you want to produce a quality annotated dataset, then the script `manualQualityReview.R` provides a super-efficient RStudio keyboard-only annotation tool. The tool can be easily be modified to use a different class-structure than I have used, simply change the variables that are defined under the comment `Setup` in the beginning of the script. **OBS**: the script uses the `"future"` package for multiprocessing, which is requires OS-specific setup, by default it is set up for Windows. If you use another OS, you must change the `future_context` variable appropriately! (Requires knowledge of the `"future"` package.

### Result use without running preprocessing.
The zip-file `MothEU_barebones.zip` contains a csv file with the resulting list of images, a single image from each observation have been chosen arbitrarily.
