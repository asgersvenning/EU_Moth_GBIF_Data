library(tidyverse)
library(magrittr)
library(sf)

library(extrafont)
library(ggpubr)
library(ggforce)

theme_set(
  theme_pubr(
    base_family = "CMU Serif",
    legend = "right"
  ) +
    theme(
      title = element_text(face = "bold",
                           size = 14),
      plot.title = element_text(face = "plain",
                                size = 20,
                                hjust = .5)
    )
)

geography <- read_sf("cleanedGeography/countries_GADM.gpkg") %>% 
  nngeo::st_remove_holes(10^7)

if (!file.exists("GBIF_combined.rds")) {
  list_meta <- read_rds("moth_species_with_GADM.rds") %>% 
    group_by(country) %>%
    reframe(
      mothData = bind_rows(mothData) %>% list,
      across(c(NAME_1, GID_1, ISO_1), ~list(unique(c(unlist(.x))))),
      across(!where(is.list), ~list(unique(.x)))
    ) %>% 
    mutate(
      across(where(~is.list(.) & all(map_dbl(., length) == 1)), unlist)
    ) %>% 
    relocate(!where(is.list)) %>% 
    select(!c(ISO_1))
  
  missing_species <- read_lines("speciesMatchLog.txt")
  
  GBIF_downloads <- "GBIF_image_metadata" %>% 
    {tibble(
      path = paste0(.,"/",list.files(.)),
      download_code = str_remove(list.files(.), "\\.csv$")
    )} %>% 
    mutate(
      data = map(path, read_csv2, 
                 show_col_types = F,
                 locale = locale(
                   decimal_mark = ",",
                   grouping_mark = "."
                 )),
      areas = map(data, ~ unique(.x$level1Gid))
    ) %>% 
    mutate(
      match = map_chr(areas, function(x) {
        x <- x[!is.na(x)]
        if (length(x) == 0) return(NA)
        list_meta %>% 
          filter(map_lgl(GID_1, function(y) {
            all(x %in% y)
          })) %>% 
          pull(country) %>% 
          unique
      }),
      match = ifelse(!is.na(match), match, list_meta$country[!(list_meta$country %in% match[!is.na(match)])])
    ) %>% 
    group_by(match) %>% 
    reframe(
      across(c(path, download_code), list),
      data = data %>% 
        bind_rows %>% 
        list,
      areas = areas %>% 
        unlist %>% 
        c %>% 
        unique %>% 
        list
    ) %>% 
    left_join(list_meta, by = c(match = "country")) 
  
  correct_types <- GBIF_downloads %>% 
    select(data) %>% 
    mutate(data = map(data, function(x) {
      x %>% 
        summarize(
          across(everything(), typeof)
        )
    })) %>% 
    unnest(data) %>% 
    count(across(everything())) %>% 
    filter(n == max(n)) %>% 
    select(!n) %>% 
    unlist()
  
  GBIF_combined <- GBIF_downloads %>% 
    mutate(
      data = map(data, function(x) {
        x %>% 
          mutate(
            across(names(correct_types)) %>% 
              map2(correct_types, ~as(.x, .y)) %>% 
              as_tibble
          ) 
      })
    ) %>% 
    select(match, data) %>% 
    unnest(data)
  
  write_rds(GBIF_combined, "GBIF_combined.rds")
} else {
  GBIF_combined <- read_rds("GBIF_combined.rds")
}
