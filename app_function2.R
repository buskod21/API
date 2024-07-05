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

access_data <- function(doi) {
  Acess_data_url <- "https://borealisdata.ca/api/access/dataset/"

  # Construct the full URL
  full_url3 <- paste0(Acess_data_url, ":persistentId/?persistentId=", doi)

  # Create a unique temporary directory for this operation
  unique_temp_dir <- tempfile("data_download")
  dir.create(unique_temp_dir)
  on.exit(unlink(unique_temp_dir, recursive = TRUE), add = TRUE)  # Ensure this directory is deleted after use

  # Make the API request and download the ZIP file
  tryCatch({
    response <- request(full_url3) %>%
      req_headers(`X-Dataverse-key`= Api_token) %>%
      req_perform()

    if (response$status_code == 200) {


      # Path to save the downloaded ZIP file
      zip_path <- file.path(unique_temp_dir, "downloaded_data.zip")
      writeBin(response$body, zip_path)

      # Directory to extract files
      unzip_dir <- file.path(unique_temp_dir, "unzipped_data")
      dir.create(unzip_dir)

      # Unzip in the new unique directory
      unzip(zip_path, exdir = unzip_dir)

      # Process files directly
      file_list <- list.files(unzip_dir, full.names = TRUE)
      return(file_list)  # Return contents of the files or other processing result

      } else {
      return(NULL)
    }
  }, error = function(e) {
        print("Restricted data not accessible.")
        return(NULL)
  })
}

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


