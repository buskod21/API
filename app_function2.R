# Function to fetch all studies
fetch_all_studies <- function() {
  Base_url <- "https://borealisdata.ca/api/"
  endpoint <- "dataverses/oacHIST/contents"
  Api_token <- "9d1d1699-56f5-4efe-801e-cf841de6f344"

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
  Api_token <- "9d1d1699-56f5-4efe-801e-cf841de6f344"

  detailed_data <- data.frame(
    Title = character(),
    PublicationDate = character(),
    Authors = character(),
    Affiliations = character(),
    StudyObjective = character(),
    Keywords = character(),
    StudyCountry = character(),
    FilesList = character(),
    Persistent_id = character(),
    stringsAsFactors = FALSE
  )

  for (i in 1:nrow(final_data)) {
    persistent_id <- paste0(final_data$protocol[i], ":", final_data$authority[i], "/", final_data$identifier[i])

    full_url2 <- paste0(Base_url, endpoint2, "exporter=dataverse_json&persistentId=", persistent_id)

    response <- request(full_url2) %>%
      req_headers(`X-Dataverse-key` = Api_token) %>%
      req_perform()

    if (response$status == 200) {
      details <- rawToChar(response$body) %>%
        fromJSON(flatten = TRUE)

      detailed_data <-
        rbind(detailed_data,
              data.frame(
                Title = details$datasetVersion$metadataBlocks$citation$fields$value[[1]][1],
                PublicationDate = details$publicationDate,
                Authors = paste(details$datasetVersion$metadataBlocks$citation$fields$value[[2]][[4]], collapse = ", "),
                Affiliations = paste(details$datasetVersion$metadataBlocks$citation$fields$value[[2]][[8]], collapse = ", "),
                StudyObjective = details$datasetVersion$metadataBlocks$citation$fields$value[[4]][[4]],
                Keywords = paste(details$datasetVersion$metadataBlocks$citation$fields$value[[6]][[4]], collapse = ", "),
                StudyCountry = details$datasetVersion$metadataBlocks$geospatial$fields$value[[1]][[4]],
                FilesList = paste(details$datasetVersion$files$label, collapse = ", "),
                Persistent_id = persistent_id
              ))
    } else {
      warning(paste("Failed to fetch details for persistent ID:", persistent_id))
    }
  }

   return(detailed_data %>% unique())
}


# Function to access the datasets of each study.

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
    stop("Failed to fetch data: HTTP status ", response$status)
  }
}


filter_filelist <- function(file_list, is_txt) {
  if (is_txt) {
    # Filter for .txt files with "README" in the basename
    filtered_files <- file_list[grep("REA.*\\.txt$", basename(file_list), ignore.case = TRUE)]
  } else {
    # Filter for .tab files
    filtered_files <- file_list[grep("(\\.tab$|\\.csv$)", basename(file_list), ignore.case = TRUE)]
  }
  return(filtered_files)
}


fetch_detailed_info <- function(title) {
  # Assuming you have a data frame called detailed_data which contains all the detailed information
  # Search for the title in the detailed_data and retrieve the corresponding row
  row <- detailed_data()[detailed_data()$Title == title, ]

  # Check if the row is found
  if (nrow(row) > 0) {
    # Extract the necessary information
    detailed_info <- list(
      authors = row$Authors,
      affiliations = row$Affiliations,
      objective = row$StudyObjective
      # Add more fields if needed
    )
    return(detailed_info)
  } else {
    # If the title is not found, return NULL
    return(NULL)
  }
}

