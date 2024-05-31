# Function to fetch all studies
fetch_all_studies <- function() {
  Base_url <- "https://borealisdata.ca/api/"
  endpoint <- "dataverses/oacHIST/contents"
  Api_token <- Sys.getenv("API_TOKEN")  # Retrieve the API token from environment variables

  full_url <- paste0(Base_url, endpoint)
  response <- request(full_url) %>%
    req_headers(`X-Dataverse-key` = Api_token) %>%
    req_perform()

  if (response$status == 200) {
    response_JSON <- rawToChar(response$body) %>%
      fromJSON(flatten = TRUE)
    final_data <- tibble(response_JSON$data)
    return(final_data)
  } else {
    warning("Error in req_perform(): HTTP status", response$status)
    return(NULL)
  }
}

# Function to fetch details of each study

fetch_study_details <- function(final_data) {
  Base_url <- "https://borealisdata.ca/api/"
  endpoint2 <- "datasets/export?"
  Api_token <- Sys.getenv("API_TOKEN")  # Retrieve the API token from environment variables

  detailed_data_list <- list()

  for (i in 1:nrow(final_data)) {
    persistent_id <- paste0(final_data$protocol[i], ":", final_data$authority[i], "/", final_data$identifier[i])

    full_url2 <- paste0(Base_url, endpoint2, "exporter=dataverse_json&persistentId=", persistent_id)

    response <- request(full_url2) %>%
      req_headers(`X-Dataverse-key` = Api_token) %>%
      req_perform()

    if (response$status == 200) {
      details <- rawToChar(response$body) %>%
        fromJSON(flatten = TRUE)

      detailed_data_list[[i]] <- data.frame(
        Title = details$datasetVersion$metadataBlocks$citation$fields$value[[1]][1],
        PublicationDate = details$publicationDate,
        Authors = paste(details$datasetVersion$metadataBlocks$citation$fields$value[[2]][[4]], collapse = ", "),
        Affiliations = paste(details$datasetVersion$metadataBlocks$citation$fields$value[[2]][[8]], collapse = ", "),
        StudyObjective = details$datasetVersion$metadataBlocks$citation$fields$value[[4]][[4]],
        Keywords = paste(details$datasetVersion$metadataBlocks$citation$fields$value[[6]][[4]], collapse = ", "),
        StudyCountry = details$datasetVersion$metadataBlocks$geospatial$fields$value[[1]][[4]],
        FilesList = paste(details$datasetVersion$files$label, collapse = ", "),
        Persistent_id = persistent_id,
        stringsAsFactors = FALSE
      )
    } else {
      warning(paste("Failed to fetch details for persistent ID:", persistent_id))
    }
  }

  detailed_data <- do.call(rbind, detailed_data_list)
  return(detailed_data %>% unique())
}


# Function to access the datasets of each study.

access_data <- function(doi) {
  Base_url <- "https://borealisdata.ca/api/"
  endpoint3 <- "access/dataset/"
  Api_token <- Sys.getenv("API_TOKEN")  # Retrieve the API token from environment variables

  # Construct the full URL
  full_url3 <- paste0(Base_url, endpoint3, ":persistentId/?persistentId=", doi)

  # Make the API request and download the ZIP file as raw data
  response <- request(full_url3) %>%
    req_headers(`X-Dataverse-key`= Api_token) %>%
    req_perform()

  if (resp_status(response) == 200) {
    # Get the content of the response as a raw vector
    zip_content <- resp_body_raw(response)

    # Use tempfile to create a temporary file for the zip content
    temp_zip <- tempfile(fileext = ".zip")

    # Write the raw vector to the temporary file
    writeBin(zip_content, temp_zip)

    # Use a temporary directory to extract the files
    temp_unzip_dir <- tempfile()

    # Extract the files to the temporary directory
    unzip(temp_zip, exdir = temp_unzip_dir)

    # List files in the temporary directory
    file_list <- list.files(temp_unzip_dir, full.names = TRUE)

    # Clean up the temporary zip file
    unlink(temp_zip)

    # Process the files as needed (this example simply returns the list of files)
    return(file_list)
  } else {
    stop("Failed to fetch data: HTTP status ", status_code(response))
  }
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


