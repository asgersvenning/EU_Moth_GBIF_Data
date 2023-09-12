# User parameters; change to the appropriate paths
species_list_path <- "countries.rds" # Path to the species list(s)
GADM_filtered_path <- "GADM/GADM_EuropeRussia.shp" # Path to the filtered GADM ESRI Shapefile

### SCRIPT BEGIN ###
library(tidyverse)
library(sf)
library(units)

sf_use_s2(FALSE)

## Helper function for cleaning edge-case names.
# Cyprus is especially annoying since it consists of 3 (!?) different sovereign territories:
# United Kingdom military areas, Cyprus (greek ancestry) and Northern Cyprus (turkish ancestry)
translate_to_GADM <- function(x) {
  if (is.null(x)) return(NULL)
  if (!is.character(x)) stop("'x' must be a character (vector)!")
  
  dict <- c(
    "Baleares" = "Islas Baleares",
    "Canary Islands" = "Islas Canarias",
    "Corsica" = "Corse",
    "Cyprus" = "Cyprus|Northern Cyprus|Akrotiri and Dhekelia",
    "East Aegean Islands" = "Aegean",
    "Kriti" = "Crete",
    "Krym" = "Crimea",
    "Sicilia" = "Sicily"
  )
  
  map_chr(x, function(x0) {
    if (x0 %in% names(dict)) dict[x0] else if (str_detect(x0, regex("russia", T))) "Russia" else x0
  })
  
}

countries <- read_rds(species_list_path) %>% 
  as_tibble() %>% 
  mutate(country = str_replace(country, "Is\\.", "Islands") %>% 
           str_remove("\\(mainland\\)")) %>% 
  rename_with(function(x) str_remove(x, "CountryOccurrence\\.CountryOccurrenceSubfield\\.") %>% 
                str_replace("CountryOccurrence", "CO_")) %>% 
  mutate(country = ifelse(str_detect(country, regex("russia", T)), "Russia", country),
         CO_Lookup = ifelse(country == "Russia", "RUS", CO_Lookup)) %>% 
  select(
    where(function(x) {
      len <- length(x)
      n_NA <- sum(is.na(x))
      x <- x[!is.na(x)]
      n_empty <- sum(str_detect(x, "^\\s*$"))
      (len - (n_NA + n_empty)) != 0
    })
  ) %>%  
  distinct(across(everything()))

GADM <- read_sf(GADM_filtered_path)

GADM_cols <- GADM %>% 
  st_drop_geometry() %>% 
  group_by(GID_1) %>% 
  summarize(
    across(everything(), ~length(unique(.x[!is.na(.x)])))
  ) %>% 
  select(GID_1, where(~mean(. == 1, na.rm = T) > .95)) %>% 
  names %>% 
  c("ISO_1")

GADM_GID_1 <- GADM %>% 
  mutate(
    GID_1 = ifelse(GID_1 == "?", "UKR.11_1", GID_1), # "Fix" for weird polygon in Kiev
    GID_1 = ifelse(is.na(GID_1) & !is.na(GID_0), GID_0, GID_1) # "Fix" for missing GID_1 for very small areas; e.g. Gibraltar
    ) %>% 
  select(all_of(GADM_cols)) %>% 
  group_by(GID_1) %>% 
  summarize(
    across(everything(), function(x) {
      if (!any((is.character(x) | is.numeric(x)) & !is.na(x))) return(NA)
      
      x <- x %>% 
        table %>% 
        magrittr::extract(which.max(.)) %>% 
        names %>% 
        unlist %>% 
        first
      
      if (is.na(x)) NA else x
    }),
    do_union = F
  )


GADM_GID_1_aggr <- GADM_GID_1 %>% 
  st_simplify(F, 0.01) %>% 
  st_union(by_feature = T, is_coverage = T) %>% 
  filter(!st_is_empty(geometry)) %>% 
  nngeo::st_remove_holes() %>% 
  st_make_valid() %>% 
  st_transform("epsg:25832") %>% 
  st_buffer(500) %>% 
  nngeo::st_remove_holes() %>%
  st_difference(.) 


GADM_GID_1_aggr %>% 
  filter(CONTINENT != "Asia" | COUNTRY == "Turkey") %>% 
  ggplot(aes(fill = GID_0)) +
  geom_sf()


nsp_countries <- countries %>% 
  group_by(country) %>% 
  summarize(
    nsp = length(unique(internal_taxon_name[!is.na(internal_taxon_name)]))
  ) %>% 
  mutate(
    GADM_match_country = translate_to_GADM(country)
  )


GADM_GID_1_match <- GADM_GID_1_aggr %>%  
  filter(!(COUNTRY == "Russia" & CONTINENT == "Asia")) %>% 
  mutate(country_match = pmap(list(NAME_0, NAME_1, SOVEREIGN), function(i0, i1, s) {
    this_nsp <- nsp_countries %>% 
      mutate(
        match = map_chr(GADM_match_country, function(x) {
          multiple_area_flag <- FALSE
          if (str_detect(x, "\\|")) {
            multiple_area_flag <- T
            x <- str_split(x, "\\|", simplify = T)
          }
            
          m0 <- if (!is.na(i0)) {
            x %>%  
              adist(i0, 
                    list("insertions" = 1, 
                         "deletions" = 0.5, 
                         "substitutions" = 1)) %>% 
              as.vector
          } else {
            Inf
          }
            
          m1 <- if (!is.na(i1)) {
            x %>% 
              adist(i1, 
                    list("insertions" = 1, 
                         "deletions" = 0.5, 
                         "substitutions" = 1)) %>% 
              as.vector
          } else {
            Inf
          }
            
          sov <- if (!is.na(s)) {
            x %>% 
              adist(s, 
                    list("insertions" = 1, 
                         "deletions" = 0.5, 
                         "substitutions" = 1)) %>% 
              as.vector
          } else {
            Inf
          }
            
          # Sanity check
          if (length(c(m0, m1, sov)) != 3 & !multiple_area_flag) {
            stop("Invalid format of 'GADM_match_country'.")
          }
            
          if (any(m1 < 1)) {
            return("Region")
          }
          else if (any(m0 < 1)) {
            return("Country")
          }
          else if (any(sov < 1)) {
            return("State")
          }
          else {
            return("None")
          }
        })
        ) %>%
      filter(match != "None") %>% 
      filter(n() == 1 | match != "State") %>%
      filter(n() == 1 | match != "Country") %>% 
      rename_with(~paste0(("list_"), .))
    
    if (nrow(this_nsp) != 1) {
      return(NA)
    }
    else {
      return(this_nsp)
    }
  })) %>% 
  unnest(country_match)


countries_GADM <- GADM_GID_1_match %>% 
  filter(!is.na(list_match)) %>% 
  st_collection_extract() %>% 
  filter(st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON")) %>% 
  group_by(list_country) %>% 
  summarise(
    across(c(GID_1, GID_0, ISO_1, NAME_0, NAME_1), ~list(unique(.x[!is.na(.x)]))),
    across(contains("list_"), first)
  )


## Save matched GADM borders with Moth areas as a GeoPackage
countries_GADM %>%
  mutate(
    across(where(~first(class(.)) == "list"), ~map_chr(.x, function(x) if (length(x) == 0) "NA" else paste0(as.character(x), collapse = ";")))
    ) %>% 
  sf::write_sf("cleanedGeography/countries_GADM.gpkg")


## Check for any non-matched countries; should print a 0-row tibble
countries %>% 
  distinct(country) %>% 
  filter(
    !(country %in% {
      countries_GADM %>% 
  st_drop_geometry() %>% 
  pull(list_country)
      })
  )


countries_GADM_full <- countries_GADM %>% 
  st_drop_geometry() %>% 
  arrange(list_country) %>% 
  bind_cols(
    countries %>% 
      nest(mothData = !c(country, CO_Lookup)) %>% 
      arrange(country)
  ) %>% 
  unnest(c(NAME_0, GID_0)) %>%
  select(!c(list_country, list_GADM_match_country, list_nsp)) %>% 
  relocate(
    NAME_0, country, 
    CO_Lookup, GID_0,
    list_match,
    mothData, 
    NAME_1, GID_1,
    ISO_1
  ) 

## Final save of the necessary data for the GBIF API requests.
# In particular the column "mothData" contains scientific names,
# while the column "GID_1" contains the medium-scale geographic regions which are tied to each
# species list, this can be used with the "gadmLevel1Gid" GBIF parameter
write_rds(countries_GADM_full, "moth_species_with_GADM.rds")