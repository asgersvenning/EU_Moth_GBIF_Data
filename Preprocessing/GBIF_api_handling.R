

                                  #############################
                                  ########### OBS #############
                                  # This script only requests #
                                  # images from January 2000  #
                                  # up until       July 2023! #
                                  #############################
                                  #############################


# Load the required packages
library(tidyverse)
library(magrittr)
library(httr)
library(jsonlite)

# Load helper functions
source("utils/GBIF_api_helpers.R")

# Helper functions are defined first, then the GBIF download requests are constructed and sent 
# starting from line ~330.

# Insert your GBIF credentials here
user <- "INSERT_YOUR_GBIF_USERNAME_HERE"
email <- "INSERT_YOUR_GBIF_EMAIL_HERE"

# Your GBIF account password. To make this more safe to use with git, instead of putting your
# password here, this should be the name of a text-file which contains "YOUR_GBIF_PASSWORD" and ends
# with a empty new line. 
# !!IMPORTANT!! Default file name is "pwd" and is already added to the .gitignore !!IMPORTANT!!
password <- "pwd" 

 ##### Robust function for creating a valid GBIF API query for species and GADM_1 area lists #####
## Description
# Includes all* (hopefully) the necessary type-checks to ensure that run-time errors occur before 
# calling the API, and with sensible error messages.
# 
# Also includes checks for keeping the API guidelines
# (https://www.gbif.org/developer/occurrence#download_limits)

## Docs:
# GADM_1: A character vector, limits the API results to occurrences within the supplied GADM code 
#         boundaries for *G*lobal *A*D*M*inistrative Areas - level 1. See https://gadm.org/.

# taxons: A character or integer-equivalent vector, limits the API results to occurrences of the 
#         species with the GBIF taxon keys in the vector. Scientific names can be matches to taxon 
#         keys using the GBIF matching API.

# parameters: A named list of further arguments to the API call. Use with caution! 
#             See https://www.gbif.org/developer/occurrence#parameters for documentation.

construct_api_call <- function(GADM_1=character(), 
                               taxons=character(), 
                               parameters=list(),
                               item_separator="&") {
  # Helpfully unlist the list of GADM areas or taxons if they are supplied in a nested list
  unlist_FLAG <- F
  if (is.list(GADM_1)) {
    GADM_1 <- unlist(GADM_1)
    unlist_FLAG <- T
  }
  if (is.list(taxons)) {
    taxons <- unlist(taxons)
    unlist_Flag <- T
  }
  if (unlist_FLAG) warning("Some arguments have been automatically unnested.")
  if (!is.character(GADM_1)) stop("Argument 'GADM_1' must be a character (vector).")
  if (!(is.character(taxons) | all.equal(taxons, as.integer(taxons)))) stop("Argument taxons must be a character or integer (vector).")
  
  GADM_1 <- GADM_1[!is.na(GADM_1)]
  taxons <- taxons[!is.na(taxons)]
  if (length(GADM_1) == 0 & length(taxons) == 0) {
    warning("Empty query requested, returned an empty tibble.")
    return(tibble())
  }
  if (length(c(GADM_1, taxons)) > 100000) stop("Too many taxons and GADM_1 areas supplied, query violates the GBIF occurence API limits.")
  
  # Create search predicates (user controlled)
  GADM_predicate <- create_predicate_multiple(
    type = "in",
    key = "GADM_LEVEL_1_GID",
    values = GADM_1
  )
  
  taxons_predicate <- create_predicate_multiple(
    type = "in",
    key  = "TAXON_KEY",
    values = as.character(taxons)
  )
  
  # Create quality predicates (static)
  standard_predicate <- c( # key = issue
    "TAXON_MATCH_FUZZY",
    "TAXON_MATCH_AGGREGATE",
    "BASIS_OF_RECORD_INVALID",
    "RECORDED_DATE_INVALID",
    "RECORDED_DATE_MISMATCH",
    "RECORDED_DATE_UNLIKELY",
    "REFERENCES_URI_INVALID"
  ) %>% 
    create_predicate_multiple(type = "in", key = "ISSUE", value = .) %>% 
    create_predicate_logical(type = "not")
  image_predicate <- "StillImage" # key = mediaType
  source_predicate <- c( # key = basisOfRecord
    "HUMAN_OBSERVATION", 
    "MACHINE_OBSERVATION"
    ) %>% 
    create_predicate_multiple(type = "in", key = "BASIS_OF_RECORD", value = .)
  # license_predicate <- c( # key = license
  #   "https://creativecommons.org/publicdomain/zero/1.0/legalcode",
  #   "https://creativecommons.org/licenses/by/4.0/legalcode",
  #   "https://creativecommons.org/licenses/by-nc/4.0/legalcode"
  # ) I am not sure it is possible to filter by media license, so this is not used ATM
  reproducibility_predicate <- list(
    create_predicate_single("equals", "MONTH", "1,7"),
    create_predicate_single("equals", "YEAR", "2023")
  ) %>%
    create_predicate_logical("and") %>% 
    list(create_predicate_single("equals", "YEAR", "2000,2022")) %>% 
    create_predicate_logical("or")
  
  # Compose predicates
  predicates <- list(
    GADM_predicate,
    taxons_predicate,
    standard_predicate,
    create_predicate_single(key = "MEDIA_TYPE", value = image_predicate),
    source_predicate,
    reproducibility_predicate
  ) %>% 
    create_predicate_logical(type = "and")
  
  return(predicates)
}

# Call the GBIF occurrence download API with a request, preferable made with the function above
call_api <- function(predicate, user, email, verbose = FALSE, ...) {
  # POST the request
  post_response <- post_download_request(
    predicate = predicate,
    user = user,
    email = email,
    ...
  )
  Sys.sleep(1)
  
  # Process the response
  if (http_error(post_response)) {
    status <- http_status(post_response)
    
    paste0("Request failed:\n",
           status$message, 
           "\n\tError:\n\t", 
          tryCatch(content(post_response), 
                   error = function(DUM) "Post response too slow (likely not related to the error).")) %>% 
      print
    stop("See error message above!")
  }
  download_code <- rawToChar(post_response$content)
  
  return(get_result_from_code(download_code, verbose = verbose))
}
  
get_result_from_code <- function(download_code, verbose = FALSE) {
  # Wait for a response
  GET_url <- paste0("https://api.gbif.org/v1/occurrence/download/request/", 
                    download_code)
  get_response <- GET(GET_url)
  if (http_error(get_response)) {
    status <- http_status(get_response)
    
    if (verbose) {
      paste0("Get failed:\n",
             status$message, 
             "\n\tError:\n\t", 
             content(get_response)) %>% 
        print
      print("RETRYING:")
    }
    get_response <- RETRY("GET", GET_url,
                          pause_min = 10,
                          pause_base = 10,
                          pause_cap = 150,
                          times = 25)
    if (http_error(get_response)) {
      status <- http_status(get_response)
      
      paste0("Get failed:\n",
             status$message, 
             "\n\tError:\n\t", 
             tryCatch(content(get_response), 
                      error = function(DUM) "Post response too slow (likely not related to the error).")) %>% 
        warning()
      write_lines(download_code, "stalledDownloadRequests.txt", append = T)
      return(tibble(status = "STALLED", code = download_code))
    }
  }
  
  # Download results
  download_url <- get_response$url
  
  zip_path <- paste0("GBIF_Downloads/", download_code, ".zip")
  if (!dir.exists("GBIF_Downloads")) dir.create("GBIF_Downloads")
  download.file(download_url, zip_path)
  unzip(zip_path, exdir = paste0("GBIF_Downloads/", download_code))
  
  # Process download
  media_useful_columns <- c(
    "gbifID",
    "identifier",
    "license",
    "rightsHolder"
  )
  
  occurrence_useful_columns <- c(
    "acceptedScientificName",
    "taxonKey",
    "taxonRank",
    "genus",
    "family",
    "order",
    "hasCoordinate",
    "decimalLongitude",
    "decimalLatitude",
    "level0Gid",
    "level1Gid",
    "continent",
    "year",
    "eventDate",
    "basisOfRecord",
    "gbifID"
  )
  
  multimedia <- read_delim(paste0("GBIF_Downloads/", download_code, "/multimedia.txt"), "\t") %>%
    select(all_of(media_useful_columns))
  occurrence <- read_delim(paste0("GBIF_Downloads/", download_code, "/occurrence.txt"), "\t") %>% 
    select(all_of(occurrence_useful_columns))
  
  combined <- multimedia %>% 
    filter(!is.na(identifier)) %>% 
    left_join(occurrence, by = "gbifID")
  
  # Save the queried images
  if (!dir.exists("GBIF_image_metadata")) dir.create("GBIF_image_metadata")
  save_path <- paste0("GBIF_image_metadata/", download_code, ".csv")

  combined %>% 
    write_csv2(save_path)
  
  if (verbose) print(paste0("Successfully saved the result to: ", save_path))
  
  return(combined)
}


get_taxonKey_from_name <- function(sname) {
  if (!is.character(sname) || is.na(sname) || nchar(sname) == 0) stop("Argument 'sname' must be a single character containing a scientific name!")
  
  match_result <- paste0("https://api.gbif.org/v1/species/search?q=", sname, "&rank=SPECIES&status=ACCEPTED") %>% 
    URLencode %>% 
    GET %>% 
    content %$%
    results
  
  if (length(match_result) == 0) {
    warning(paste0("No match found for \"", sname, "\"!"))
    write_lines(sname, "speciesMatchLog.txt", append = T)
    return(NA)
  }
  
  if (is.list(match_result) & length(match_result) == 1) {
    match_result <- match_result[[1]]
  } else {
    no_obs <- map_lgl(match_result, function(x) {
      if (is.null(names(x))) return(FALSE)
      if (any(!(c("numDescendants", "numOccurrences") %in% names(x)))) return(FALSE)
      if (is.na(x$numDescendants) | is.na(x$numOccurrences) | !is.numeric(x$numDescendants) | !is.numeric(x$numOccurrences)) return(FALSE)
      out <- x$numDescendants != 0 | x$numOccurrences != 0
      if (is.null(out) || !is.logical(out) || is.na(out)) {
        print(paste0("Out:"))
        print(out)
        print("Origin:")
        print(match_result)
        stop("!?!")
      }
      return(out)
    })
    match_result <- match_result[!no_obs]
    if (length(match_result) == 0) {
      warning(paste0("No match found for \"", sname, "\"!"))
      write_lines(sname, "speciesMatchLog.txt", append = T)
      return(NA)
    }
    for (i in 1:length(match_result)) {
      if (match_result[[i]]$taxonomicStatus == "ACCEPTED" & !match_result[[i]]$synonym & as.vector(adist(sname, match_result[[i]]$canonical_name)) < 2) {
        match_result <- match_result[[i]]
        break
      }
    }
  }
  
  # Multiple matches
  if (is.null(names(match_result))) {
    print(match_result)
    for (i in 1:length(match_result)) {
      print("---------------------------")
      print(paste0("Result (", i, "):"))
      print(
        match_result[[i]][c(
          "class",
          "order",
          "scientificName",
          "taxonomicStatus",
          "synonym",
          "speciesKey"
        )]
      )
    }
    user_input <- readline(paste0("Multiple matches for \"", sname, "\" please select 1:", length(match_result), ":")) %>% 
      as.integer
    
    if (!is.na(user_input)) match_result <- match_result[[user_input]] else stop("INVALID MATCH")
  }

  if ("speciesKey" %in% names(match_result)) {
    key <- if ("nubKey" %in% names(match_result) && !is.null(match_result$nubKey)) match_result$nubKey else match_result$speciesKey
    if (is.null(key)) {
      warning(paste0("Error occurred with extracting the key from \"", sname,"\""))
      write_lines(sname, "speciesMatchLog.txt", append = T)
      return(NA)
    } else {
      return(key)
    }
  } else {
    print(paste0("INPUT = ", sname))
    print(match_result)
    stop("No species key present in result")
  }
}


####################################################################################################
###################################### SCRIPT PART STARTS HERE #####################################
####################################################################################################


moth_lists <- read_rds("moth_species_with_GADM.rds")

if (!file.exists("speciesKeyDict.csv")) {
  speciesKeyDict <- moth_lists %>% 
  unnest(mothData) %>% 
  distinct(internal_taxon_name) %>% 
  mutate(
    taxonKey = map_int(internal_taxon_name, get_taxonKey_from_name)
  )
  write_csv2(speciesKeyDict, "speciesKeyDict.csv")
} else {
  speciesKeyDict <- read_csv2("speciesKeyDict.csv")
}


moth_lists <- moth_lists %>% 
  mutate(
    mothData = map(mothData, function(x) {
      x %>% 
        left_join(speciesKeyDict, by = "internal_taxon_name")
    })
  )

stalled_requests <- tibble(
  download_code = read_lines("stalledDownloadRequests.txt")
) %>% 
  drop_na %>% 
  filter(nchar(download_code) > 10) %>% 
  mutate(
    data = map(download_code, get_result_from_code),
    path = paste0("GBIF_image_metadata/", download_code, ".csv") %>% 
      {
        stopifnot(all(sapply(., file.exists)))
        .
      }
    )

write_lines(NULL, "stalledDownloadRequests.txt")


finished_queries <- "GBIF_image_metadata" %>% 
  {paste0(., "/", list.files(.))} %>% 
  tibble(path = .) %>% 
  mutate(
    download_code = str_extract(path, "(?<=\\/)\\d+-\\d+"),
    data = map(path, read_csv2, 
               show_col_types = F, 
               locale = locale(decimal_mark = ",",
                               grouping_mark = ".")),
    GADM = map(data, ~ unique(.x$level1Gid)),
    match = map_chr(GADM, function(x) {
      moth_lists %>% 
        filter(map_lgl(GID_1, function(y) {
          any(x %in% y)
        })) %>% 
        pull(country) %>% 
        unique
    })
  )

                                    ########## OBS ##########
                                    # This takes many hours #
                                    #########################

moth_lists_download <- moth_lists %>% 
  filter(!(country %in% finished_queries$match)) %>% 
  mutate(
    taxonKeys = map(mothData, ~.x$taxonKey),
    images = map2(taxonKeys, GID_1, function(t, g) construct_api_call(g, t) %>% 
                    call_api(user, email, 
                             password = password, 
                             sendNotification = FALSE))
  ) 

rm(moth_lists_download)