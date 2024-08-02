fetch_study_details <- function(data){

  BaseURL_details <- "https://borealisdata.ca/api/datasets/:persistentId/metadata?persistentId="

  detailed_data_list <- list()

  for (i in 1:nrow(data)) {

    # Constructing the persistent_id inside the loop for each row
    persistent_id <- paste0(data$protocol[i], ":", data$authority[i], "/", data$identifier[i])
    Full_url_details <- paste0(BaseURL_details, persistent_id)

    print(paste("Fetching URL:", Full_url_details))  # Debug print

    # Using httr2 to create and send the request
    response_details <- request(Full_url_details) %>%
      req_headers(`X-Dataverse-key` = Api_token) %>%
      req_perform()

    if (inherits(response_details, "try-error") || response_details$status_code != 200) {
      print(paste("Error or not found for ID:", persistent_id, "- Status code:", response_details$status_code))
      next  # Skip to the next iteration of the loop
    }

    print(paste("HTTP Status Code:", response_details$status_code))  # Debug print

    if (response_details$status_code == 200) {
      detail_oac <-  rawToChar(response_details$body) %>%
        jsonlite::fromJSON(flatten = TRUE)

      detailed_data_list[[i]] <- data.frame(
        Title = detail_oac[["data"]][["title"]],
        PublicationDate = detail_oac[["data"]][["dateOfDeposit"]],
        Authors = paste(detail_oac[["data"]][["author"]][["citation:authorName"]], collapse = "; "),
        Subject = paste(detail_oac[["data"]][["subject"]], collapse = "; "),
        Keywords = paste(detail_oac[["data"]][["citation:keyword"]][["citation:keywordValue"]], collapse = "; "),
        DataType = paste(detail_oac[["data"]][["kindOfData"]], collapse = "; "),
        Objectives = detail_oac[["data"]][["citation:dsDescription"]][["citation:dsDescriptionValue"]],
        PeriodCovered = paste(detail_oac[["data"]][["citation:dateOfCollection"]][["citation:dateOfCollectionStart"]],
                              "to ",detail_oac[["data"]][["citation:dateOfCollection"]][["citation:dateOfCollectionEnd"]]),
        Affiliations = paste(detail_oac[["data"]][["author"]][["citation:authorAffiliation"]], collapse = "; "),
        # StudyCountry = detail_oac[["data"]][["geospatial:geographicCoverage"]][["geospatial:country"]],
        NameOfDataverse = detail_oac[["data"]][["schema:isPartOf"]][["schema:name"]],
        DOI = persistent_id,

        stringsAsFactors = FALSE
      )

    } else {
      warning(paste("Failed to fetch data for ID:", data$id[i], "Status code:", response_details$status_code))
    }
  }

  detailed_data <- do.call(rbind, detailed_data_list)
  return(unique(detailed_data))
}

 detailed_data1 <- fetch_study_details(all_data)

 write.csv(detailed_data1, "allOAC_info.csv")


 # # User-selected IDs from the input
 # selectedIDs <- input$select_dataverse
 # # Debugging: Print selected IDs to the console
 # print(selectedIDs)
 #
 # # Decide whether to fetch data for all IDs or just selected IDs
 # if (is.null(selectedIDs) || identical(selectedIDs, allIDs)) {
 #   data <- fetch_dataverse_data(allIDs)
 # } else {
 #   data <- fetch_dataverse_data(selectedIDs)
 # }

 # access_data <- function(doi) {
 #   Acess_data_url <- "https://borealisdata.ca/api/access/dataset/"
 #
 #   # Use system temporary directory
 #   sanitized_doi <- gsub("[^A-Za-z0-9]", "_", doi)
 #   unique_dir <- file.path(tempdir(), paste0("data_", sanitized_doi))
 #
 #   # Ensure the unique temporary directory is ready
 #   if (!dir.exists(unique_dir)) {
 #     dir.create(unique_dir, recursive = TRUE)
 #   }
 #
 #   # Full URL construction and file path setup
 #   full_url3 <- paste0(Acess_data_url, ":persistentId/?persistentId=", doi)
 #   zip_path <- file.path(unique_dir, "downloaded_data.zip")
 #
 #   # Make the API request and download the ZIP file
 #   tryCatch({
 #     response <- request(full_url3) %>%
 #       req_headers(`X-Dataverse-key`= Api_token) %>%
 #       req_perform()
 #
 #     if (response$status_code == 200) {
 #
 #
 #       # Path to save the downloaded ZIP file
 #       writeBin(response$body, zip_path)
 #
 #       # Directory to extract files
 #       unzip_dir <- file.path(unique_dir, "unzipped_data")
 #       dir.create(unzip_dir, recursive = TRUE)
 #
 #       # Unzip in the new unique directory
 #       unzip(zip_path, exdir = unzip_dir)
 #
 #       # Process files
 #       file_list <- list.files(unzip_dir, full.names = TRUE)
 #       if (length(file_list > 0)){
 #         return(file_list)
 #
 #         on.exit(unlink(unique_dir, recursive = TRUE), add = TRUE)  # Ensure this directory is deleted after use
 #       } else {
 #         return(NULL)
 #       }
 #     }
 #   }, error = function(e) {
 #     print("Restricted data not accessible.")
 #     return(NULL)
 #   })
 # }


 # access_data("doi:10.5683/SP3/WVC09T")

 # Reactive expression for fetching detailed data based on selected dataverse
 detailed_data <- reactive({
   # Always load OAC_info; it is needed to get all IDs if specific IDs are not selected
   req(OAC_info)

   # Determine the current selection or default to all IDs
   selectedIDs <- if (is.null(input$select_dataverse) || identical(input$select_dataverse, OAC_info$id)) {
     OAC_info$id  # Default to all IDs
   } else {
     req(input$select_dataverse)  # Only require input selection if it's used
     input$select_dataverse  # Use the selected IDs
   }

   # Fetch dataverse data based on current selection
   data <- fetch_dataverse_data(selectedIDs)

   # Fetch and return detailed study data based on the fetched dataverse data
   detailed_data <- fetch_study_details(data)
   return(detailed_data)
 }) %>%
   bindCache(OAC_info, input$select_dataverse)  # Cache based on changes to OAC_info or the dataverse selection

 # Reactive expression for fetching detailed data based on selected dataverse
 detailed_data <- reactive({

   # Ensure necessary inputs are loaded
   req(OAC_info, input$select_dataverse)

   # IDs selected by the user
   selectedIDs <- input$select_dataverse

   # Fetch dataverse data based on selection
   dataverse_data <- fetch_dataverse_data(selectedIDs)

   # Fetch and return detailed study data based on the fetched dataverse data
   detailed_data <- fetch_study_details(dataverse_data)

   return(detailed_data)

 }) %>%
   bindCache(OAC_info, input$select_dataverse)  # Cache this reactive based on changes in OAC_info or selected dataverse


 # # Code to filter and populate the metadata and datafile dropdown list using the DOI and access_data()
 #
 #   full_paths <- reactiveVal(list()) # Initialize a reactive value to store the files full file path
 #   files <- reactiveValues() # Create a reactiveValues object for file list, filtered metadata and data files
 #
 #   # This observeEvent should trigger once a study is selected
 #   observe({
 #     # Ensure a selection has been made
 #     req(input$study_select)
 #
 #     # Dynamically choose between all_data() and detailed_() to display study details
 #     data <- if (is.null(input$select_dataverse)) all_data() else detailed_data()
 #
 #     # Fetch DOI, file lists, and update inputs
 #     selected_title <- input$study_select
 #     selected_doi <- data %>%
 #       filter(Title == selected_title) %>%
 #       pull(DOI) %>%
 #       unique()
 #
 #     # Ensure that selected_doi is a single value
 #     # Handle cases where the DOI count is not 1 silently
 #     if (length(selected_doi) != 1) {
 #       # Simply reset input fields and return early
 #       updateSelectInput(session, "metadata_select", choices = NULL, selected = NULL)
 #       updateSelectInput(session, "dataset_select", choices = NULL, selected = NULL)
 #       full_paths(list())  # Ensure any previous paths are cleared
 #       return()  # Exit the observer without further action
 #     }
 #
 #     # Fetch the file list
 #     files$file_list <- access_data(selected_doi)
 #
 #
 #     # Check if the file list is NULL or empty
 #     if (is.null(files$file_list) || length(files$file_list) == 0) {
 #       # Update UI components to show no data is available
 #       updateSelectInput(session, "metadata_select", choices = NULL, selected = NULL)
 #       updateSelectInput(session, "dataset_select", choices = NULL, selected = NULL)
 #
 #       full_paths(list())  # Reset full_paths to empty, ensuring it's cleared even when data is absent
 #       return()  # Exit the function early to prevent further execution
 #     }
 #
 #     # Update full_paths right after file_list is validated and stored
 #     full_paths(setNames(files$file_list, basename(files$file_list)))
 #
 #     files$metadata <-filter_filelist(files$file_list, is_txt = TRUE)
 #     files$datafile <-filter_filelist(files$file_list, is_txt = FALSE)
 #
 #     #  Update metadata_select
 #     updateSelectInput(session, "metadata_select", choices = basename(files$metadata), selected = NULL)
 #
 #     # Process update dataset_select  based on the active tab dynamically
 #     if (input$datafiles == "view_alldata") {
 #       updateSelectInput(session, "dataset_select", choices = basename(files$file_list), selected = NULL)
 #     }
 #     else {
 #       updateSelectInput(session, "dataset_select", choices = basename(files$datafile), selected = NULL)
 #     }
 #
 #   })
