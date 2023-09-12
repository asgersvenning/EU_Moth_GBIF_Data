### Helper functions for using the GBIF occurrence download API
## Required packages: httr, rstudioapi

#### Must be used inside RStudio ####

post_download_request <- function(predicate, 
                                  user, email, 
                                  sendNotification = TRUE,
                                  password) {
  password <- if (missing(password)) {
    invisible(rstudioapi::askForPassword("Type your GBIF password to POST request"))
  }
  else {
    readLines(password)
  }
  
  httr::POST(
    url = "https://api.gbif.org/v1/occurrence/download/request",
    config = authenticate(
      user, 
      password
    ),
    add_headers(
      `Content-Type` = "application/json"
    ),
    body = list(
      creator = user,
      notificationAddresses = I(email),
      sendNotification = sendNotification,
      format = "DWCA",
      predicate = predicate
    ) %>% 
      toJSON(auto_unbox = T),
    encode = "raw"
  )
}

create_predicate_single <- function(type = "equals", key, value) {
  list(
    type = type,
    key = key,
    value = value
  )
}

create_predicate_multiple <- function(type, key, values) {
  if (type == "in" & length(values) == 1) {
    return(
      create_predicate_single("equals", key, values)
    )
  }
  list(
    type = type,
    key = key,
    values = values
  )
} 

create_predicate_logical <- function(predicates, type) {
  if (any(map_lgl(predicates, is_list))) {
  list(
    type = type,
    predicates = predicates
  )
  }
  else {
    list(
      type = type,
      predicate = predicates
    ) 
  }
}

create_predicate_advanced <- function(type, parameters) {
  c(list(type = type), parameters)
}