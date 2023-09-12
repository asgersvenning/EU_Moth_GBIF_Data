# Encode image as base64 string
encode_image_base64 <- function(image) {
  # Get the image type
  image_type <- stringr::str_extract(image, "(?<=\\.)[a-z]+$")
  
  # Convert the image to base64
  data <- readr::read_file_raw(image) %>% 
    RCurl::base64Encode() %>% 
    unclass
  
  # Return the base64 string with the appropriate prefix
  return(
    paste0("data:image/", image_type, ";base64,", data)
  )
}

# Helper functions for image browsing
ensure_dir_exists <- function(path) {
  # If the path is a file, only consider its directory
  file_info <- file.info(path)
  if (is.na(file_info$isdir) || !file_info$isdir) {
    # Remove the file name from the path:
    path <- stringr::str_extract(path, "^.+/") # Match everything up to and including the last
    path <- stringr::str_remove(path, "/$") # Remove the trailing slash
  }
  
  # Create the directory recursively if it doesn't exist
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
}

display_html <- function(image) {
  # Create a temporary html file to display the image
  temp_html <- tempfile(fileext = ".html")
  
  # Write a barebones html page that displays and resizes the image appropriately
  # The box div is used to center the image
  box_style <- c("display: flex",
                 "height: 100%",
                 "margin: 0",
                 "padding: 0") %>% 
    paste0(collapse = ";")
  
  # The image is resized to fit the screen
  image_style <- c("max-width: 100%",
                   "max-height: 100vh",
                   "margin: auto",
                   "padding: 0") %>% 
    paste0(collapse = ";")
  
  # Create the html file by combining the styles and the image path
  paste0(
    # Use the RStudio background color behind the image
    '<html><body style="margin: 0px; background-color: ',
    rstudioapi::getThemeInfo()$background, 
    ';"><div style="', box_style,
    '"><img style="', image_style,
    # Encode the image as base64 and insert it as a data URI 
    # to avoid any issues with local resource permissions
    '" src="', encode_image_base64(image),
    '"></div></body></html>'
  ) %>% 
    write_lines(temp_html) # Write the html file at the temporary location
  
  rstudioapi::viewer(temp_html) # Display the image in the viewer pane
  return(temp_html) # Return the path to the temporary html file
}

display_image <- function(image, prompt) {
  if (is.na(image) || !is.character(image)) return("Error")
  
  temp_html <- display_html(image)
  user_input <- prompt() # Prompt the user for input
  
  file.remove(temp_html) # Remove the temporary html file
  
  # Type check the user input
  if (!(length(user_input) == 2 & is.character(user_input))) {
    file.remove(image) # Remove the image
    return(NA) # Return NA to indicate that an invalid user input was given or an error occurred
  }
  
  # Move the image to the specified path
  ensure_dir_exists(user_input[1]) # Ensure the directory of the path exists
  file.rename(image, user_input[1]) # Move the image to the destination
  
  return(user_input) # Return the user input selection
}

# Function to recreate the log from the images in the destination directory if the log is lost
recreate_log_on_error <- function(dst_dir) {
  # Count the number of current logs in the destination directory 
  # (DO NOT CREATE ANY FILES FOLLOWING THIS NAMING SCHEMA USING ANY OTHER MEANS THAN THIS FUNCTION)
  n_logs <- list.files(dst_dir, "^sample_quality_log\\d+\\.csv$") %>% 
    length
  
  # Check when the last log was created
  last_log <- paste0(dst_dir, "/sample_quality_log", n_logs, ".csv")
  last_log_time <- file.info(last_log)$mtime
  
  # Get all the file names in the class sub-directories (i.e. the images) and their modification times
  image_files <- list.files(dst_dir, ".*([^\\.][^c][^s][^v]|[^\\.][^r][^d][^s])$", 
                            recursive = TRUE, full.names = TRUE)
  image_files_times <- file.info(image_files)$mtime
  
  # Separate the image files that were created after the last log was created
  new_image_files <- image_files[image_files_times > last_log_time]
  if (length(new_image_files) == 0) {
    print("No images were added since the last log was created.")
    return(FALSE)
  }
  print("Recreating log...")
  
  # Extract the new image annotation classifications and ids
  new_image_parts <- new_image_files %>% 
    normalizePath("/") %>% 
    str_split("\\/", simplify = T)
  
  new_image_classes <- new_image_parts[, ncol(new_image_parts) - 1]
  new_image_ids <- str_extract(new_image_parts[, ncol(new_image_parts)], 
                               "\\d+(?=\\.\\w{2,4}$)") %>% 
    as.numeric() 
  
  # Get the remaining data file
  remaining_data <- paste0(dst_dir, "/remaining_data.rds") %>% 
    read_rds() 
  
  # Add the new classifications to the remaining data while matching the gbifID with the new_image_ids
  missing_log <- remaining_data %>% 
    filter(gbifID %in% new_image_ids) %>% 
    mutate(
      quality = map_chr(gbifID, function(x) {
        new_image_classes[new_image_ids == x]
      }
      )
    )
  stopifnot(nrow(missing_log) == length(new_image_ids))
  
  # Temporary return value
  return(missing_log)
  
  # Write the missing log as the newest log
  write_csv2(
    x = missing_log, 
    file = paste0(dst_dir, "/sample_quality_log", n_logs + 1, ".csv")
  )
  # and remove the fixed data from the remaining data
  remaining_data <- remaining_data %>% 
    filter(!(gbifID %in% new_image_ids))
  write_rds(
    x = remaining_data,
    file = paste0(dst_dir, "/remaining_data.rds")
  )
  warning("A log was recreated from the non-logged images in the destination directory.")
  return(TRUE)
}

# Function for sampling weighted
sample_weighted_indices <- function(weights) {
  weights <- weights / sum(weights)
  # Generate random values and weight them
  weighted_random_values <- runif(length(weights)) * weights
  
  # Order indices by the weighted random values
  sampled_indices <- order(weighted_random_values, decreasing = TRUE)
  
  return(sampled_indices)
}
