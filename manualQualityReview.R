library(tidyverse)
library(furrr)
library(future)
source("utils/browseImages.R")

if (!file.exists("GBIF_combined_distinct.rds")) {
  GBIF_combined_distinct <- read_rds("GBIF_combined.rds") %>% 
    distinct(gbifID, .keep_all = TRUE) 
  
  write_rds(GBIF_combined_distinct, "GBIF_combined_distinct.rds")
} else if (!("GBIF_combined_distinct" %in% objects())) {
  GBIF_combined_distinct <- read_rds("GBIF_combined_distinct.rds")
}

# This function gets the image and returns a promise which resolves once the image is downloaded
get_image_async <- function(
    image, 
    quiet = T,
    path = tempfile(
      pattern = digest::digest(image, algo = "md5"),
      tmpdir = tempdir(),
      fileext = stringr::str_extract(image, "\\.\\w+$"))
) {
  # download.file(image, path, mode = "wb", quiet = quiet)
  # path
  future({
      tryCatch({
        download.file(future_image, future_path, mode = "wb", quiet = future_quiet)
      future_path
      }, error = function(e) {
        if ("message" %in% names(e)) {
          print(paste0("An error occurred while downloading the image: ",
                       e$message))
        } else {
          print("An error occurred while downloading the image.")
        }
        NA_character_
      })
    },
    globals = list(
      future_image = image,
      future_path = path,
      future_quiet = quiet
    )
  )
}

create_cache <- function(cache_size_max, cache_size_min) {
  cache <- setRefClass("cache",
                       fields = list(
                         queue = "list",
                         cache_size_max = "numeric",
                         cache_size_min = "numeric"
                       ),
                       methods = list(
                         append = function(element) {
                           if (!inherits(element, "Future")) {
                             stop(
                               paste0("Argument 'element' must be a future not a ", 
                                      class(element),
                                      ". Use 'future::future()' to create one.")
                             )
                           }
                           while (length(queue) >= cache_size_max) {
                             # Evict the first element to maintain the minimum cache size
                             queue <<- queue[-1]
                             warning("Cache is full, evicting first ", cache_size_min, " elements.")
                           }
                           
                           queue <<- c(queue, list(element))
                           invisible(NULL)
                         },
                         pop = function() {
                           if (length(queue) == 0) {
                             stop("Cache is empty.")
                           }
                           
                           future_expr <- queue[[1]]
                           
                           # Attempt to resolve the future, catching any errors
                           value <- tryCatch({
                             value(future_expr)
                           }, error = function(e) {
                             if ("message" %in% names(e)) {
                               print(paste0("An error occurred while resolving the future: ",
                                            e$message))
                             } else {
                               print("An error occurred while resolving the future.")
                             }
                             NA
                           })
                           
                           # Remove the resolved future from the cache
                           queue <<- queue[-1]
                           
                           if (length(queue) < cache_size_min) {
                             warning("Cache is below minimum size.")
                           }
                           
                           # Return the resolved value
                           return(value)
                         }
                       )
  )
  
  # Initialize the fields
  instance <- cache$new()
  instance$queue <- list()
  instance$cache_size_max <- cache_size_max
  instance$cache_size_min <- cache_size_min
  
  return(instance)
}

annotate_images <- function(df, urls, names, dst_dir, 
                            workers = 6, max_cache = 2 * workers, max_block_size = 2^13,
                            remaining_data = "remaining_data.rds",
                            check_log = TRUE,
                            force_restart = FALSE) {
  # The argument 'force_restart' exists mostly for debugging purposes and is extremely dangerous to
  # work with, since it **WILL** delete the entire annotation directory ('dst_dir') recursively! 
  if (isTRUE(force_restart)) {
    force_restart <- select.list(
      c("NO", "I am not sure", "yes"),
      title = "\n##### OBS ##### - - This action is irreversible - - ##### OBS #####\n\nYou are about to delete all annotated images and any information related to it.\nAre you really sure you want to delete everything that has been annotated so far?\n\n##### OBS ##### - - This action is irreversible - - ##### OBS #####\n"
    )
    if (force_restart == "yes") {
      if (dir.exists(dst_dir)) unlink(dst_dir, recursive = T)
    }
    else stop("!!! USE THE 'force_restart' ARGUMENT WITH EXTREME CAUTION !!!")
  }
  if (!dir.exists(dst_dir)) dir.create(dst_dir)
  processed_ids <- c()
  if (check_log) {
    log_recreated <- recreate_log_on_error(dst_dir)
  }
  # If this file is used a second time, load the remaining data instead of using the supplied 'df'
  if (file.exists(paste0(dst_dir, "/", remaining_data))) {
    df <- read_rds(paste0(dst_dir, "/", remaining_data))
  }
  # Remove the already processed observations from the 'df'
  logs <- dst_dir %>% 
    paste0(., "/", list.files(., "sample_quality_log"))
  if (length(logs) > 0) {
    processed_ids <- logs  %>% 
      lapply(function(x) {
        log <- read_delim(x,
                          delim = ";",
                          show_col_types = F,
                          locale = locale(
                            grouping_mark = ".",
                            decimal_mark = ","),
                          col_types = cols(
                            match = col_character(),
                            gbifID = col_double(),
                            identifier = col_character(),
                            license = col_character(),
                            rightsHolder = col_character(),
                            acceptedScientificName = col_character(),
                            taxonKey = col_double(),
                            taxonRank = col_character(),
                            genus = col_character(),
                            family = col_character(),
                            order = col_character(),
                            hasCoordinate = col_logical(),
                            decimalLongitude = col_double(),
                            decimalLatitude = col_double(),
                            level0Gid = col_character(),
                            level1Gid = col_character(),
                            continent = col_character(),
                            year = col_double(),
                            eventDate = col_double(),
                            basisOfRecord = col_character(),
                            sample_weight = col_double(),
                            name = col_character(),
                            quality = col_character(),
                            path = col_character()
                          )
        )
        log$gbifID
      }) %>% 
      unlist %>% 
      as.numeric
  }
  df <- df %>% 
    filter(!gbifID %in% processed_ids)
  if (!is.character(urls) | !is.character(names) | !inherits(df, "data.frame")) {
    stop("Invalid input type for argument(s) 'df', 'urls' and/or 'names'.")
  }
  if (nrow(df) < 1) stop("Dataframe 'df' must not be empty.")
  if ("quality" %in% names(df)) stop("Dataframe 'df' must not contain the column \"quality\".")
  if (length(urls) != nrow(df)) {
    if (length(urls) != 1) stop("'urls' must be a character string of length 1 or equal to number of rows in 'df'")
    urls <- df[[urls]]
  }
  if (length(names) != nrow(df)) {
    if (length(names) != 1) stop("'names' must be a character string of length 1 or equal to number of rows in 'df'")
    names <- df[[names]]
  }
  
  if (workers > 1) {
    old_plan <- plan("multisession", workers = workers)
    on.exit({
      future::plan(old_plan)
      }, add = TRUE)
    on.exit({
      # The logs are written when the function, to ensure that the logs are written even if the
      # function is interrupted by the user or an error occurs
      
      # Remove unfinished quality elements
      quality <- quality[!is.na(quality)]
      
      # Write the remaining data to a file for easy resuming
      write_rds(
        x = slice_tail(df, n = nrow(df) - length(quality)),
        file = paste0(dst_dir, "/", remaining_data)
      )
      
      # write the annotated data to a log *and* return
      annotated_df <- df %>% 
        slice_head(n = length(quality)) %>% 
        mutate(
          quality = quality,
          path = ifelse(
            quality == "Error",
            "Error",
            path
          )
        )
      
      # Count the number of current logs in the destination directory 
      # (DO NOT CREATE ANY FILES FOLLOWING THIS NAMING SCHEMA USING ANY OTHER MEANS THAN THIS FUNCTION)
      n_logs <- list.files(dst_dir, "^sample_quality_log\\d+\\.csv$") %>% 
        length
      
      # Write the log
      write_csv2(
        x = annotated_df, 
        file = paste0(dst_dir, "/sample_quality_log", n_logs + 1, ".csv")
      )
    })
  }
  stop <- FALSE
  
  # Pre-fill the cache with a few images 
  # (the cache is evaluated in a pre-defined environment to avoid any scoping issues)
  cache_index <- 1
  cache_env <- environment()
  cache <- create_cache(max_cache, max_cache - 2)
  for (i in 1:max_cache) {
    cache$append(get_image_async(urls[cache_index]))
    assign("cache_index", cache_index + 1, envir = cache_env)
  }
  
  quality <- rep(NA_character_, min(max_block_size, length(urls)))
  path    <- rep(NA_character_, min(max_block_size, length(urls)))
  
  for (image_index in 1:length(urls)) {
    # The stop flag is used to prematurely stop the annotation process, allowing the user to 
    # annotate for a desired amount of time, without advance knowledge
    if (stop) break
    
    # Get the next image from cache and remove it from the list
    this_image <- cache$pop()
    cache$append(get_image_async(urls[cache_index]))
    assign("cache_index", cache_index + 1, envir = cache_env)
    print(cache_index)
    
    if (!inherits(this_image, "character")) return(this_image)
    this_description <- paste0(
      "Image ", image_index, " of ", length(urls), "\n",
      "URL: ", urls[image_index], "\n",
      "Name: ", names[image_index], "\n"
    )
    
    # Display the image and query the user for an annotation
    this_path_quality <- tryCatch({
      display_image(
      image = this_image,
      prompt = function(desc = this_description) {
        # First ask if the image is in either of these super-classes, or to abort annotation
        user_select <- select.list(
          c("Larvae", "Adult", "Other", "STOP", "Change Last Classification"),
          title = desc
        )
        # if the user chooses to remove the last annotation, remove the last annotation (cursed)
        if (user_select == "Change Last Classification") {
          if (image_index == 1) {
            print("Cannot remove last annotation, as this is the first image.")
            return(eval(quote(prompt()), envir = parent.frame()))
          }
          # Save the old quality
          old_quality <- quality[image_index - 1]
          # Display the image again
          temp_html <- display_html(path[image_index - 1])
          # Ask the user for the new quality
          new_quality <- eval(
            quote(prompt("Change the class of the last image")), 
            envir = parent.frame()
          )
          quality[image_index - 1] <<- new_quality[2]
          print(quality[image_index - 1])
          file.remove(temp_html)
          
          if (old_quality != quality[image_index - 1]) {
            # If the quality has changed, rename the file
            # (i.e. move it to the appropriate sub-directory)
            src_file <- path[image_index - 1]
            dst_file <- str_replace(path[image_index - 1], 
                                    old_quality, 
                                    quality[image_index - 1]) 
            ensure_dir_exists(dst_file)
            file.rename(
              src_file,
              dst_file
            )
          }
          # Display the current image again
          temp_html <- display_html(this_image)
          # And ask the user for the quality again (restart the prompt)
          restart_prompt <- eval(quote(prompt()), envir = parent.frame())
          file.remove(temp_html)
          return(restart_prompt)
        }
        
        # If the user chooses to abort the annotation set the stop flag to TRUE and return NA
        # signalling the outer functions interrupt their normal control-flow anddelete temp-files
        if (user_select == "STOP") {
          stop <<- TRUE
          return(NA)
        }
        # If the user selects the class "Adult" ask which qualitative quality the image has
        if (user_select == "Adult") user_select <- select.list(
          c("Blurry", "Visible", "Clear", "HD"),
          title = this_description
        )
        # Create the file-name at the appropriate sub-directory according to the user-selected class
        # as well as the image name and suffix (file-type).
        image_dst <- paste0(dst_dir, "/", # Destination root directory 
                            user_select, "/", # Destination class sub-directory
                            names[image_index],  # File-name
                            stringr::str_extract(urls[image_index], "\\.\\w+$")) # File suffix
        # Return the image destination file-name and the user-selected classification
        return(c(image_dst, user_select))
      }
    )
    }, error = function(e) {
      # If an error occurs, print it and return "Error" as the quality
      if ("message" %in% names(e)) {
        print(e$message)
        print(str(e))
        if (select.list(c("yes", "no"), title = "Continue?") == "no") stop(e)
      } else {
        print("Unknown error:")
        print(e)
        print("Str:")
        print(str(e))
      }
      return("Error")
    })
    
    last_path <- this_path_quality[1]
    last_quality <- this_path_quality[2]
    
    if (image_index > length(quality)) {
      quality <- c(quality, rep(NA_character_, min(max_block_size, length(urls) - length(quality))))
    }
    if (image_index > length(path)) {
      path <- c(path, rep(NA_character_, min(max_block_size, length(urls) - length(path))))
    }
    quality[image_index] <- last_quality
    path[image_index] <- last_path
  }
  
  # Remove unfinished quality elements
  quality <- quality[!is.na(quality)]
  
  # write the annotated data to a log *and* return
  annotated_df <- df %>% 
    slice_head(n = length(quality)) %>% 
    mutate(
      quality = quality,
      path = ifelse(
        quality == "Error",
        "Error",
        path
      )
    )
  
  return(annotated_df)
}

set.seed(1234)
## Use this annotation function
annotated_images <- GBIF_combined_distinct %>% 
  # slice_sample(n = 10^5) %>%
  # group_by(family, genus, match) %>%
  # mutate(sample_weight = n()) %>%
  # ungroup %>%
  # mutate(
  #   name = paste0(match, "_", stringr::str_replace_all(acceptedScientificName, "[^[:alnum:][:space:]]+", ""), "_", gbifID)
  # ) %>%
  # slice(
  #   sample_weighted_indices(1 / log10(sample_weight + max(log10(sample_weight + 1))))
  # ) %>%
  annotate_images(
    "identifier",
    'name',
    dst_dir = "quality_review_v3",
    force_restart = FALSE,
    check_log = TRUE
  ) 
  # select(match, acceptedScientificName, quality, identifier) 

