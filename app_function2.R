Api_token <- Sys.getenv("API_TOKEN")  # Retrieve the API token from environment variables

# Function to create and perform the request to get oac_dataverse content
## The IDs for each of the dataverse is extracted for the study detail API call
fetch_oac_info <- function() {
  # OAC content endpoint
  Base_url <- "https://borealisdata.ca/api/dataverses/"
  #Api_token <- Sys.getenv("API_TOKEN")  # Retrieve the API token from environment variables
  endpoint <- "oac/contents"
  # Define full url for the API call
  oac_fullurl <- paste0(Base_url, endpoint)

  # Perform the API request
  oac_response <- request(oac_fullurl) %>%
    req_headers(`X-Dataverse-key` = Api_token) %>%
    req_perform()

  # Check the response status
  if (oac_response$status == 200) {

    # Extract result from response
    oac_data <- oac_response$body %>%
      rawToChar() %>%
      fromJSON(flatten = TRUE)

    oac_data <- tibble(oac_data$data)
    return(oac_data)

  } else {
    cat("Error in req_perform(): HTTP status", oac_response$status, "\n")
    oac_data <- tibble()  # Return an empty tibble in case of error
  }
}



# Function to fetch content of each dataverse in OAC repo based on their IDs
## IDs are gotten from the fetch_oac_info()

fetch_dataverse_data <- function(ids) {

  Base_url <- "https://borealisdata.ca/api/dataverses/"
  #Api_token <- Sys.getenv("API_TOKEN")

  results <- lapply(ids, function(id) {
    # Construct the full URL for each ID
    dataverse_fullurl <- paste0(Base_url, id, "/contents")

    dataverse_id <- id

    # Use tryCatch to handle errors during the HTTP request and response processing
    tryCatch({
      # Perform the request
      response <- request(dataverse_fullurl) %>%
        req_headers(`X-Dataverse-key` = Api_token) %>%
        req_perform()

      # Check the status of the response and process if successful
      if (response$status_code == 200) {
        dataverse_raw_data <- rawToChar(response$body) %>%
          fromJSON(flatten = TRUE)

        # Check if data is not null and has rows
        if (!is.null(dataverse_raw_data$data) && nrow(dataverse_raw_data$data) > 0) {
          # Create a tibble and add the Dataverse ID
          data <- as_tibble(dataverse_raw_data$data) %>%
            mutate(dataverse_id = as.character(dataverse_id))  # Use the scoped variable for the dataverse ID

          return(data)
        } else {
          message("No data or empty data for ID ", id)
          return(NULL)
        }
      } else {
        message("Failed to fetch data for ID ", id, ": HTTP status ", response$status_code)
        return(NULL)
      }
    }, error = function(e) {
      message("Error during request for ID ", id, ": ", e$message)
      return(NULL)  # Return NULL if the request fails
    })
  })

  # Combine all tibbles into one and perform final cleaning
  combined_data <- bind_rows(results)

  return(combined_data)
}


# Function to fetch metadata of each dataverse in OAC repo based on their DOIs
## DOIs are gotten from the fetch_dataverse_data()

fetch_study_details <- function(data){

  BaseURL_details2 <- "https://borealisdata.ca/api/datasets/export?exporter=schema.org&persistentId="
  Api_token <- Sys.getenv("API_TOKEN")  # Retrieve the API token from environment variables

  detailed_data_list <- list() # create an empty list

  for (i in 1:nrow(data)) {

    # Constructing the persistent_id inside the loop for each row
    persistent_id <- paste0(data$protocol[i], ":", data$authority[i], "/", data$identifier[i])
    Full_url_details <- paste0(BaseURL_details2, persistent_id)

    # Using tryCatch to handle potential errors from the HTTP request or data processing
    tryCatch({
      # Create and send the request
      response_details <- request(Full_url_details) %>%
        req_headers(`X-Dataverse-key` = Api_token) %>%
        req_perform()

      # Process the response
      if (response_details$status_code == 200) {
        detail_oac2 <-  rawToChar(response_details$body) %>%
          jsonlite::fromJSON(flatten = TRUE)

        # Extract the needed information from the API response
        detailed_data_list[[i]] <- data.frame(
          Title = detail_oac2[["name"]],
          PublicationDate = detail_oac2[["datePublished"]],
          Authors = paste(detail_oac2[["author"]][["name"]], collapse = "; "),
          Affiliations = paste(detail_oac2[["author"]][["affiliation.name"]], collapse = "; "),
          Keywords = paste(detail_oac2[["keywords"]], collapse = "; "),
          Objectives = detail_oac2[["description"]],
          Citation = paste(detail_oac2[["citation"]][["name"]], collapse = ", "),
          PeriodCovered = paste(detail_oac2[["temporalCoverage"]], collapse = "; "),
          StudyLocation = paste(detail_oac2[["spatialCoverage"]], collapse = ", "),
          Funder = paste(detail_oac2[["funder"]][["name"]], collapse = "; "),
          FileList = paste(detail_oac2[["distribution"]][["name"]], collapse = "; "),
          DOI = persistent_id,
          stringsAsFactors = FALSE
        )

      } else {
        warning(paste("Failed to fetch data for ID:", data$id[i], "Status code:", response_details$status_code))
      }
    }, error = function(e) {
      # Silently handle errors by returning NULL
      detailed_data_list[[i]] <- data.frame()  # Create an empty data frame on error
    })
  }

  # Combine all tibbles into one and perform final cleaning
  detailed_data <- do.call(rbind, detailed_data_list) %>%
    unique()

  return(detailed_data)
}


# library(dplyr)
# library(httr2)
# access_data <- function(doi) {
#
#   #api_token <- Sys.getenv("API_TOKEN")  # Retrieve the API token from environment variables
#
#   # Construct the full URL
#   full_url_dataAccess <- paste0("https://borealisdata.ca/api/access/dataset/",
#                                 ":persistentId/?persistentId=",
#                                 doi)
#
#   # Attempt to make the API request and handle errors gracefully
#   tryCatch({
#     response <- request(full_url_dataAccess) %>%
#       req_headers(`X-Dataverse-key` = Api_token) %>%
#       req_perform()
#
#     # Check the status of the response
#     if (resp_status(response) == 200) {
#       # Get the content of the response as a raw vector
#       zip_content <- resp_body_raw(response)
#
#       # Use tempfile to create a temporary file for the zip content
#       temp_zip <- tempfile(fileext = ".zip")
#
#       # Write the raw vector to the temporary file
#       writeBin(zip_content, temp_zip)
#
#       # Use a temporary directory to extract the files
#       temp_unzip_dir <- tempdir()
#
#       # Extract the files to the temporary directory
#       unzip(temp_zip, exdir = temp_unzip_dir)
#
#       # List files in the temporary directory
#       file_list <- list.files(temp_unzip_dir, full.names = TRUE)
#
#       # Clean up the temporary zip file
#       unlink(temp_zip)
#
#       return(file_list)
#
#     } else {
#       return(NULL)
#     }
#   }, error = function(e) {
#     # Silently handle errors by returning NULL
#     return(NULL)
#   })
#
# }

access_data <- function(doi) {
  Base_url <- "https://borealisdata.ca/api/"
  endpoint3 <- "access/dataset/"
  Api_token <- "9d1d1699-56f5-4efe-801e-cf841de6f344"

  # Sanitize the DOI to create a valid directory name
  sanitized_doi <- gsub("[^A-Za-z0-9]", "_", doi)
  unique_dir <- paste0("data_", sanitized_doi)

  # Check and create a unique directory
  if (!dir.exists(unique_dir)) {
    dir.create(unique_dir, recursive = TRUE)
  }

  # Construct the full URL
  full_url3 <- paste0(Base_url, endpoint3, ":persistentId/?persistentId=", doi)

  # Path to save the downloaded ZIP file
  zip_path <- file.path(unique_dir, "downloaded_data.zip")

  # Make the API request and download the ZIP file
  tryCatch({
    response <- request(full_url3) %>%
      req_headers(`X-Dataverse-key`= Api_token) %>%
      req_perform()

    if (response$status == 200) {
      writeBin(response$body, zip_path)
      unzip_dir <- file.path(unique_dir, "unzipped_data")
      if (!dir.exists(unzip_dir)) {
        dir.create(unzip_dir, recursive = TRUE)
      }
      unzip(zip_path, exdir = unzip_dir)
      file_list <- list.files(unzip_dir, full.names = TRUE)
      return(file_list)
    } else {
      return(NULL)
    }
  }, error = function(e) {
    # Silently handle errors by returning NULL
    return(NULL)
  })
}

access_data("doi:10.5683/SP3/4RA3KT")

# Function to filter for .txt file and .tab/.csv file in the filelist and extract just the basename
filter_filelist <- function(file_list, is_txt) {
  if (is_txt) {
    # Filter for .txt files with "README" in the basename
    filtered_files <- file_list[grep("REA.*\\.txt$", basename(file_list), ignore.case = TRUE)]
  } else {
    # Filter for .tab or .csv files
    filtered_files <- file_list[grep("(\\.tab$|\\.csv$)", basename(file_list), ignore.case = TRUE)]
  }
  return(filtered_files)
}

# Check if a column contains any letters
contains_letters <- function(x) {
  any(grepl("[a-zA-Z]", x))
}


