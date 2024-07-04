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
