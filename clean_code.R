# Load required libraries
library(shiny)
library(shinycssloaders)
library(bs4Dash)
library(shinyWidgets)
library(httr2)
library(jsonlite)
library(tidyverse)
library(DT)
library(data.table)
library(shinyjs)
library(stringr)
library(purrr)
library(visNetwork)
library(DataExplorer)
library(heatmaply)



# Source the functions
source("app_function2.R")


# Create a folder to store the cached information
shinyOptions(cache = cachem::cache_disk("./cache_folder/cache/"))

# UI Definition
ui <- dashboardPage(
  skin = "light",
  scrollToTop = TRUE,
  fullscreen = TRUE,

  # Dashboard header
  dashboardHeader(
    title = tags$h3(tags$b("Menu:")),
    skin = "light",
    status = "lightblue",
    navbarMenu(
    navbarTab(tags$b("Home"),
             #icon = icon("house"),
             tabName = "home"),
    navbarTab(tags$b("Explore borealis"),
             #icon = icon("server"),
             tabName = "Borealisdata")
    )
  ),

  # Disable dashboard sidebar
  dashboardSidebar(disable = TRUE),

  # Dashboard body
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "home",  # Content for the "home" tab ----
        h3(strong("Welcome to the reusable data explorer App.")),  # Subheading

        p("This shiny web application was developed as a part of the Reusable
        research data made shiny workshop that was held at the University of Guelph,
        Guelph Ontario Canada.","You can find more information about the workshop
        and access the workshop materials",
          tags$a(href = "https://github.com/agrifooddatacanada/RRDMS_Workshop",
                 "here.", target = "_blank")),  # Create a hyperlink

        h4(strong("About the App")),

        p("The ",
          strong("Explore borealis tab"),
          "allows you to view the networks of studies in the OAC historical reproducible
          project repository on borealis based on keywords and author's name.", "Users can
          then select a particular keyword or author to futher explore the associated studies.",
          "The study overview, metadata and explore data tab allows users to gain a deeper insight
          about the study and data. Click ",
          tags$a(href = "https://borealisdata.ca/dataverse/oacHist",
                 "here",
                 target = "_blank"),
          " to access the oac historical reproducible project repository in the Borealis database."),

        br(),

        img(src = 'workshop1.jpeg', height = 400, width = 700),

        br(),

        p(h4(strong("Have fun exploring re-usable data!"))
        )
      ),

      tabItem(
        tabName = "Borealisdata",
        tabBox(id="mainbox",
               title = "",
               width = 12,
               collapsible = TRUE,
               maximizable = TRUE,
               elevation = 1,
               solidHeader = TRUE,
               status = "lightblue",
               side = "right",
               type = "tabs",
               tabPanel("Study network", # UI Panel for the study network ----
                        fluidRow(
                          column(12,
                                 awesomeRadio(
                                   inputId = "event_type",
                                   label = "View network by :",
                                   choices = c("Keywords", "Authors"),
                                   selected = "Keywords",
                                   inline = TRUE
                                 ), hr(),
                                 withSpinner(visNetworkOutput("networkPlot",
                                                              width="100%",
                                                              height = "800px")
                                             ),
                                 absolutePanel(id = "infoPanel",
                                               fixed = TRUE,
                                               draggable = TRUE,
                                               top = 360, right = 60, width = 300,
                                               height = "auto",
                                               uiOutput("nodeInfo"))# uioutput for displaying node-related information
                          )
                        )
               ),

               tabPanel("Explore database", # UI for the explore database tabPanel ----
                        fluidRow(
                          column(12,
                                 div(id = "selectDiv",
                                     selectInput("study_select",
                                                 "Study selection",
                                                 choices = NULL)
                                 )
                          )
                        )
               )
        ),

        br(), # Inserts a line break

        # UI to show this tabBox when the tabPanel 'explore database' is clicked ----
        shinyjs::useShinyjs(),  # Set up shinyjs
        shinyjs::hidden(
          div(id = "overviewBox",
              withSpinner(
                tabBox(
                  title = "",
                  width = 12,
                  collapsible = TRUE,
                  maximizable = TRUE,
                  elevation = 1,
                  solidHeader = TRUE,
                  status = "lightblue",
                  side = "right",
                  type = "tabs",
                  tabPanel("Study Overview", # Panel for the study overview ----
                           fluidRow(
                             column(12,
                                    withSpinner(
                                      DT::dataTableOutput("study_details")
                                    )
                             )
                           )
                  ),

                  tabPanel("Metadata", # Panel for the metadata output ----
                           fluidRow(
                             column(12,
                                    selectInput("metadata_select",
                                                "Select a Metadata",
                                                choices = NULL),


                                    br(),

                                    # UI for the metadata
                                    tags$b("Data description"),
                                    hr(),
                                    dataTableOutput("meta"),

                                    # UI for the schema output
                                    tags$b("Data Schema"),
                                    hr(),
                                    DT::dataTableOutput("schema")
                             )
                           )
                  ),

                  tabPanel("Data exploration", # tabPanel for data exploration ----
                           fluidRow(
                             column(12,
                                    selectInput("dataset_select",
                                                "Select a dataset",
                                                choices = NULL),
                                    br(),

                                    bs4Dash::tabsetPanel(
                                      tabPanel("Data summary",

                                               br(),

                                               box(
                                                 title = "View raw data",
                                                 status = "white",
                                                 solidHeader = TRUE,
                                                 collapsible = TRUE,
                                                 elevation = 1,
                                                 width = 12,
                                                 collapsed = F,
                                                 DT::dataTableOutput("rawtable")
                                               ),

                                               fluidRow(
                                                 column(6,
                                                        box(
                                                          title = "Data structure",
                                                          status = "white",
                                                          solidHeader = TRUE,
                                                          collapsible = TRUE,
                                                          elevation = 1,
                                                          width = 12,
                                                          collapsed = F,
                                                          DT::dataTableOutput("structure")
                                                        )
                                                 ),
                                                 column(6,
                                                        box(
                                                          title = "Missing values",
                                                          status = "white",
                                                          solidHeader = TRUE,
                                                          collapsible = TRUE,
                                                          elevation = 1,
                                                          width = 12,
                                                          collapsed = F,
                                                          plotOutput("missing_value")
                                                        )
                                                 )

                                               ),
                                               box(
                                                 title = "Descriptive statistics",
                                                 status = "white",
                                                 solidHeader = TRUE,
                                                 collapsible = TRUE,
                                                 elevation = 1,
                                                 width = 12,
                                                 collapsed = F,
                                                 DT::dataTableOutput("summary")
                                               )
                                      ),

                                      tabPanel("Data visualization",

                                               br(),

                                               fluidRow(
                                                 column(3,
                                                        box(
                                                          title = "Select analysis type",
                                                          status = "white",
                                                          solidHeader = TRUE,
                                                          collapsible = FALSE,
                                                          elevation = 1,
                                                          width = 12,
                                                          awesomeRadio(
                                                            inputId = "analysis_type",
                                                            label = NULL,
                                                            choices = c("Histogram",
                                                                        "Density plot",
                                                                        "QQ plot",
                                                                        "Heat map",
                                                                        "Boxplot",
                                                                        "Linear regression"),
                                                            selected = "Histogram",
                                                            inline = FALSE
                                                          ),
                                                          uiOutput("variable_selection_x"),
                                                          uiOutput("variable_selection_y")
                                                        )
                                                 ),
                                                 column(9,
                                                        # Box for displaying the plot area
                                                        box(
                                                          title = "Plot viewer",
                                                          status = "white",
                                                          solidHeader = TRUE,
                                                          collapsible = FALSE,
                                                          elevation = 1,
                                                          width = NULL,
                                                          withSpinner(uiOutput("plot"))
                                                        )
                                                 )
                                               )
                                      ),
                                      type = "pills",
                                      vertical = FALSE
                                    )
                             )
                           )
                  )
                )
              )
          )
        )
      )
    )
  )
)




server <- function(input, output, session) {

  # get the basic data that is used to get detailed data
  basic_data <- fetch_all_studies()

  #Reactive expression to get detailed data
  detailed_data <- reactive ({
    fetch_study_details(basic_data)
  }) %>%
    bindCache(basic_data) # detailed data is cached

  # View all studies if not filtered by the network
  observe({
    data <- detailed_data()
    updateSelectInput(session, "study_select", choices = unique(data$Title))
  })

  # Reactive expression to extract unique keywords from detailed data
  keywords <- reactive({

    req(detailed_data())

    data <- detailed_data()

    unique_keywords <- data %>%
      pull(Keywords) %>% # Extract the 'Keywords' column
      str_split(",\\s*") %>% # split keywords where there is a comma and ignore whitespaces
      unlist() %>% # change keyword list into a single vector i.e., flatten the list
      unique() %>% # # Get unique value
      na.omit() %>% # Remove missing values
      sort() # Sort the names alphabetically
    return(unique_keywords)
  })

  # Reactive expression to extract unique authors from detailed data
  authors <- reactive({
    req(detailed_data())

    data <- detailed_data()

    unique_authors <- data %>%
      pull(Authors) %>%  # Extract the 'Authors' column
      str_split(", ") %>%  # Split each row into individual names by comma
      # apply this function (from purr package) to map each of the author names
      map(~{
        # Check if the length is even, if not, the last name is left unpaired
        if (length(.) %% 2 == 1) {
          names <- .[-length(.)]  # Remove the last element if total number is odd
        } else {
          names <- .
        }
        # Convert names to a matrix and then to paired full names
        matrix(names, ncol = 2, byrow = TRUE) %>%
          apply(1, function(x) paste(x[1], x[2], sep = ", "))
      }) %>%
      unlist() %>%  # Flatten the list of names
      unique() %>%  # Get unique values
      na.omit() %>%  # Remove NA values
      sort()  # Sort the names alphabetically

    return(unique_authors)
  })

  # Reactive expression to prepare nodes data based on selected event type
  nodes_data <- reactive({
    req(input$event_type)

    data <- detailed_data() %>%
      # change date format
      mutate(PublicationDate = as.Date(PublicationDate, format="%Y-%m-%d"))

    # Conditional statement to toggle between keywords and authors
    if (input$event_type == "Keywords") {
      events <- keywords()
      event_column <- "Keywords"
    } else {
      events <- authors()
      event_column <- "Authors"
    }

    # Apply the function to each event in the events list and combine the results into a dataframe
    nodes <- map_df(events, function(event) {

      # Filter the data to find papers matching the current event
      matched_papers <- data %>%
        filter(str_detect(.data[[event_column]], regex(paste0("\\b", event, "\\b"), ignore_case = TRUE)))

      studies_count <- nrow(matched_papers)
      year_range <- if (studies_count > 0) {
        min_date <- min(matched_papers$PublicationDate, na.rm = TRUE)
        max_date <- max(matched_papers$PublicationDate, na.rm = TRUE)
        paste(min_date, "to", max_date)
      } else {
        "No found studies"
      }

      # Create a tibble (data frame) with the event information
      tibble(
        id = which(events == event),
        label = event,
        group = input$event_type,
        title = paste("Study Count:", studies_count, "<br>", "Year Range:", year_range)
      )
    })

    return(nodes)
  }) %>%
    bindCache(input$event_type, detailed_data()) #Cache the output of these reactives

  # Reactive expression to create edges data based on selected event type
  edges_data <- reactive({
    req(input$event_type)

    data <- detailed_data()

    # Initialize an empty data frame to hold edges
    edges <- data.frame(from = numeric(0), to = numeric(0), stringsAsFactors = FALSE)

    if (input$event_type == "Keywords") {
      events <- keywords()
      event_column <- "Keywords"
    } else {
      events <- authors()
      event_column <- "Authors"
    }

    # Create edges based on shared records
    for (i in 1:length(events)) {
      for (j in (i + 1):length(events)) {
        # Ensure all operations handle NA correctly
        shared_records <- sum(sapply(data[[event_column]], function(k) {
          if(is.na(k)) {
            FALSE
          } else {
            grepl(events[i], k, ignore.case = TRUE) && grepl(events[j], k, ignore.case = TRUE)
          }
        }), na.rm = TRUE)  # Remove NA results

        # If there are shared records, create an edge
        if (shared_records > 0) {
          edges <- rbind(edges, data.frame(from = i, to = j))
        }
      }
    }

    return(edges)
  }) %>%
    bindCache(input$event_type, detailed_data()) #Cache the output of this reactive based on the selected event type


  # Render the network plot
  output$networkPlot <- renderVisNetwork({
    req(nodes_data(), edges_data())
    visNetwork(nodes_data(), edges_data(), width = "100%", height = "800px") %>%
      visNodes(shape = " circle",
               scaling = list(label = list(enabled = TRUE, min = 10, max = 30))) %>%
      visEdges(smooth = FALSE) %>%
      visOptions(highlightNearest = TRUE,
                 nodesIdSelection=list(enabled = TRUE,
                                       main = "Select by ID",
                                       style = 'width: 300px; height: 26px;')) %>%
      visLayout(randomSeed = 123) %>%  # Consistent layout
      visInteraction(hover = FALSE,
                     navigationButtons = TRUE,
                     keyboard = TRUE,
                     tooltipDelay = 0) %>%
      visEvents(click = "function(nodes) {
        if (nodes.nodes.length > 0) {
          Shiny.setInputValue('selectedEvent', nodes.nodes[0], {priority: 'event'});
        }
      }")
  })

  # When an event is selected in the network, update the study select input
  observeEvent(input$selectedEvent, {
    req(input$event_type, detailed_data())

    data <- detailed_data()  # Ensure data is loaded
    collaborators <- NA  # Initialize collaborators

    if (!is.null(input$selectedEvent) && !is.na(input$selectedEvent)) {
      # Dynamically select the event type and corresponding column
      if (input$event_type == "Keywords") {
        event <- keywords()[input$selectedEvent]
        event_column <- "Keywords"
      } else {  # Assume Authors
        event <- authors()[input$selectedEvent]
        event_column <- "Authors"
      }

      # Apply filtering based on the selected node
      filtered_data <- data %>%
        filter(str_detect(.data[[event_column]], regex(paste0("\\b", event, "\\b"), ignore_case = TRUE)))

      # Calculate collaborators only if 'Authors' is selected
      if (input$event_type == "Authors") {
        collaborators <- filtered_data %>%
          pull(Authors) %>%
          str_split(",") %>%
          unlist() %>%
          str_trim() %>%
          unique() %>%
          setdiff(event) %>%
          length()
      }

      # Retrieve studies and DOIs from filtered data
      studies <- filtered_data %>% pull(Title)
      dois <- filtered_data %>% pull(Persistent_id) %>% unique()

      # Generate appropriate text for DOIs
      if (length(dois) == 0) {
        doi_text <- "No DOI found."
      } else if (length(dois) > 1) {
        doi_links <- sapply(dois, function(doi) {
          paste0('<a class="badge badge-info" href="https://doi.org/', doi, '" target="_blank">', doi, '</a>')
        })
        doi_text <- paste(paste(doi_links, collapse = ", "))
      } else {
        doi_links <- paste0('<a class="badge badge-info" href="https://doi.org/', dois, '" target="_blank">', dois, '</a>')
        doi_text <- paste(doi_links)
      }

      # Prepare the output information with HTML
      info_html <- tags$div(
        tags$p(tags$b("Number of studies: "), length(unique(studies))),
        tags$p(tags$b("Number of collaborators: "), ifelse(is.na(collaborators), "N/A", collaborators)),
        tags$p(tags$b("DOIs of studies: "), HTML(doi_text))  # Ensure HTML is properly rendered
      )

      # Update the UI output for HTML content
      output$nodeInfo <- renderUI({
        info_html
      })
    } else {
      # Handle no selection
      output$nodeInfo <- renderUI({
        tags$div(class = "alert alert-warning", "No node selected.")
      })
    }

    # Update the study select input with the relevant studies
    updateSelectInput(session, "study_select", choices = unique(studies))
  })



  # Observe changes in the tabPanel selection
  observeEvent(input$mainbox, {
    # Check if the 'Explore database' tabPanel is selected
    if (input$mainbox == "Explore database") {
      # Show the box
      shinyjs::show("overviewBox")
    } else {
      # Hide the box if any other tabPanel is selected
      shinyjs::hide("overviewBox")
    }
  })

  # Show study details based on selection
  output$study_details <- DT::renderDataTable({
    req(input$study_select, detailed_data())  # Ensure that a selection has been made and detailed_data is available
    # Fetch the detailed data
    data <- detailed_data()

    # Filter data first before transposing
    filtered_data <- data %>%
      filter(Title == input$study_select) %>% # Filter data first before transposing
      t() # Transpose the filtered data

    # Render the datatable
    datatable(filtered_data,
              options = list(dom = 'tp',
                             autoWidth = TRUE,
                             scrollX = TRUE),
              rownames = TRUE,
              colnames = c("Parameter", "Value"))
  })


  # define the list of full-path to store the mapping between base names and full paths
  full_paths <- reactiveVal(list())

  # This observeEvent should trigger once a study is selected
  observeEvent(input$study_select, {

    # Ensure a selection has been made
    req(input$study_select, detailed_data())

    # Fetch DOI, file lists, and update inputs
    selected_title <- input$study_select
    selected_doi <- detailed_data() %>%
      filter(Title == selected_title) %>%
      pull(Persistent_id) %>%
      unique()

    # Ensure that selected_doi is a single value
    if (length(selected_doi) != 1) {
      stop("Multiple or no DOIs found for the selected title")
    }

    file_list <- access_data(selected_doi)

    Metadata <- filter_filelist(file_list, is_txt = TRUE)
    datafiles <- filter_filelist(file_list, is_txt = FALSE)

    full_paths(setNames(file_list, basename(file_list)))

    updateSelectInput(session, "metadata_select", choices = basename(Metadata))
    updateSelectInput(session, "dataset_select", choices = basename(datafiles))
  })


  # ----- Metadata tabPanel -----

  # Reactive expression for loading metadata
  loaded_metadata <- reactive({
    req(input$metadata_select)  # Ensure a file is selected

    # Use the full path for reading the file
    selected_file_path <- full_paths()[input$metadata_select]

    # Read the file and suppress warning
    suppressWarnings({
      raw_lines <- readLines(selected_file_path)
    })

    # Replace only the first '=' with ':'
    corrected_lines <- sapply(raw_lines, function(line) {
      sub("=", ":", line, fixed = TRUE)
    })

    # Replace only the first ':' with '|'
    final_lines <- sapply(corrected_lines, function(line) {
      sub(":", "|", line, fixed = TRUE)
    })

    # Read the modified lines as a table, using '|' as the separator
    meta_file <- read.table(text = final_lines,
                            sep = "|",
                            fill = TRUE,
                            header = FALSE,
                            stringsAsFactors = FALSE)

    # Optionally, clean rows with all NA values
    meta_file <- meta_file[rowSums(is.na(meta_file)) != ncol(meta_file), ]

    return(meta_file)
  })



  # Logic to render a DataTable for data meta
  output$meta <- DT::renderDataTable({

    meta_data <- req(loaded_metadata())

    separate_by_tab <- grep("^\\t", meta_data$V1)

    # Ensure there's at least one match and only use the first one
    if (length(separate_by_tab) > 0) {
      first_tab <- separate_by_tab[1]  # Use only the first tab if there are multiple
      datatable_data <- meta_data[1:(first_tab - 1), , drop = FALSE]
    } else {
      # Handle the case where no tabs are found
      datatable_data <- meta_data
    }

    DT::datatable(datatable_data,
                  rownames = FALSE,
                  colnames = c("Key", "Value"),
                  options = list(dom = "tp",
                                 autoWidth = FALSE,
                                 scrollX = TRUE))
  })


  # Logic to render a DataTable for data schema
  output$schema <- DT::renderDataTable({
    meta_data <- req(loaded_metadata())

    separate_by_tab <- grep("^\\t", meta_data$V1)

    # Ensure there's at least one match and use the first one
    if (length(separate_by_tab) > 0) {
      first_tab <- separate_by_tab[1]  # Use only the first tab if there are multiple
      datatable_data <- meta_data[first_tab:nrow(meta_data), , drop = FALSE]
    } else {
      # Handle the case where no tabs are found
      datatable_data <- meta_data
    }

    DT::datatable(datatable_data,
                  rownames = FALSE,
                  colnames = c("Key", "Value"),
                  options = list(dom = "tp",
                                 autoWidth = FALSE,
                                 scrollX = TRUE))
  })

  # ----- Exploratory data analysis tabPanel -----

  # Reactive expression for loading data
  loaded_data <- reactive({
    req(input$dataset_select)  # Ensure a file is selected

    # Use the full path for reading the file
    selected_file_path_data <- full_paths()[input$dataset_select]

    # Attempt to read the file, handle failure
    data <- tryCatch({
      fread(selected_file_path_data, header = TRUE, quote = "\"")
    }, error = function(e) {
      # Return NULL or handle the error appropriately
      NULL
    })

    if (!is.null(data)) {

      # Convert real numeric variables identified as character into numeric variables
      data <- suppressWarnings(
        data %>%
          mutate(across(where(~ is.character(.) && !contains_letters(.)), as.numeric))%>%
          mutate(across(where(is.numeric), ~ ifelse(is.finite(.), ., NA))) %>%
          drop_na()
      )

      # Clean column names by keeping only letters and numbers
      # (e.g., some colnames have characters like %,*,#)
      names(data) <- gsub("[^A-Za-z0-9]", "", names(data))
    }

    return(data)
  })


  # ------Logic for data description tabsetPanel ------

  # Logic to render the raw data table
  output$rawtable <- DT::renderDataTable({

    req(loaded_data())  # Ensure data is loaded

    DT::datatable(loaded_data(),
                  rownames = FALSE,
                  options = list(dom = "tp",
                                 autoWidth = FALSE,
                                 scrollX = TRUE))
  })

  # Logic to render the data structure
  output$structure <- renderDataTable({
    req(loaded_data())  # Ensure data is loaded

    # Give the loaded_data() structure, convert to tibble and transpose
    data <- loaded_data() %>%
      introduce () %>%
      tibble () %>%
      t()

    # Data table output
    DT::datatable(data,
                  rownames = TRUE,
                  colnames = c("Key", "Value"),
                  options = list(dom = "tp",
                                 autoWidth = FALSE,
                                 scrollX = TRUE))
  })

  # logic for the missing value
  output$missing_value <- renderPlot({
    req(loaded_data())  # Ensure data is loaded

    # Identify and plot missing values to select the best variables
    plot_missing(loaded_data(),
                 ggtheme = theme_classic(),
                 theme_config = list(axis.ticks = element_blank(),
                                     axis.line = element_line(colour = "grey50"),
                                     panel.grid.minor = element_blank(),
                                     panel.grid.major.x = element_blank(),
                                     panel.grid.major.y = element_blank(),
                                     panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
                                     plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4")))
  })

  # logic for summary statistics
  output$summary <- renderDataTable({
    req(loaded_data())  # Ensure data is loaded

    data_file <- loaded_data() # rename the loaded_data()

    #Check if the input is a data frame
    if(!is.data.frame(data_file)) stop("data needs to be a dataframe")

    #Convert input to data frame if it's not already
    data_file <- as.data.frame(data_file)

    # Filter out and subsets only logical and numeric columns
    data_file <- data_file[sapply(data_file, is.logical) | sapply(data_file, is.numeric)]

    # Check if the filtered data frame is suitable
    if ((ncol(data_file) < 1) | (nrow(data_file) < 2)) stop("insuitable data frame (does it contain numerical data?)")

    # Compute the descriptive statistics
    data <- cbind(apply(data_file,2,function(x) as.integer(sum(!is.na(x)))),
                  apply(data_file,2,mean, na.rm=TRUE),
                  apply(data_file,2,stats::sd, na.rm=TRUE),
                  t(apply(data_file, 2, function(x)
                    stats::quantile(x,
                                    probs=c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE))))

    # Round up the decimal place for each column
    data <- round (data, digits = c(0, 3, 3, 3, 3, 3, 3, 3))

    # Assign column names
    colnames(data) <- c("N", "Mean", "Std. dev.", "Min.", "25 %", "Median", "75 %", "Max.")

    # output data as a datatable
    DT::datatable(data,
                  rownames = TRUE,
                  options = list(dom = "tp",
                                 autoWidth = FALSE,
                                 scrollX = TRUE))
  })


  # ------Logic for data visualization tabsetPanel ------

  # Dynamic UI for variable selection based on analysis type

  observeEvent(input$analysis_type, {

    analysis_type <- input$analysis_type

    if (analysis_type %in% c("Boxplot", "Linear regression")) {

      output$variable_selection_x <- renderUI({
        req(loaded_data())
        var_choices <- colnames(loaded_data())
        selectInput("variable_x", "Select X Variable", choices = var_choices)
      })

      output$variable_selection_y <- renderUI({
        req(loaded_data(), input$variable_x)
        y_choices <- setdiff(colnames(loaded_data()), input$variable_x)
        selectInput("variable_y", "Select Y Variable", choices = y_choices)
      })
    } else {
      output$variable_selection_x <- renderUI({ NULL })
      output$variable_selection_y <- renderUI({ NULL })
    }
  })

  # Generate analysis output based on chosen analysis
  output$plot <- renderUI({

    req(loaded_data()) # Ensure these inputs are set

    analysis_type <- input$analysis_type

    if (analysis_type == "Heat map") {
      plotlyOutput("heatmap_plot",
                   height = "auto")
    } else {
      plotOutput("other_plots",
                 height = "600px")
    }
  })

  # Logic to render other_plots

  output$other_plots <- renderPlot({

    # Ensure these variables are found
    req(loaded_data())

    analysis_type <- input$analysis_type

    data <- loaded_data()

    # set variable names

    if (analysis_type %in% c("Boxplot", "Linear regression")) {

      req(input$variable_x, input$variable_y)

      variable_x <- input$variable_x
      variable_y <- input$variable_y

      # a. Boxplot
      if (analysis_type == "Boxplot") {
        data[[variable_x]] <- factor(data[[variable_x]])
        if (length(levels(data[[variable_x]])) > 0) {
          boxplt <- ggplot(data, aes_string(x = variable_x,
                                            y = variable_y,
                                            fill = variable_x)) +
            geom_boxplot() +
            geom_point(size = 2) +
            ggtitle("A boxplot showing individual datapoints") +
            theme_classic() +
            theme(strip.text = element_text(face = "bold", size = 12),
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
                  plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
                  plot.caption = element_text(hjust = 0, size = 14),
                  axis.line = element_line(colour = "grey50"),
                  axis.text.x = element_text(vjust = 1.2, size = 12, color = "black"),
                  axis.text.y = element_text(color = "black", size = 12),
                  axis.title.x = element_text(vjust = 0, size = 14),
                  axis.title.y = element_text(size = 14),
                  legend.title = element_blank(),
                  legend.position = "none")
          print(boxplt)
        } else {
          warning("No levels in the factor variable for the fill aesthetic")
        }
      }

      # b. Linear regression
      if (analysis_type == "Linear regression") {
        ggplot(data, aes_string(x = variable_x,
                                y = variable_y)) +
          geom_point(size = 3.5) +
          ggtitle("Relationship between two continous variables") +
          geom_smooth(method = "lm", color = "blue", se = FALSE, formula = y ~ x) +
          theme_classic() +
          theme(strip.text = element_text(face = "bold", size = 12),
                panel.grid.major = element_blank(),
                panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
                plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
                panel.grid.minor = element_blank(),
                plot.caption = element_text(hjust = 0, size = 10),
                axis.line = element_line(),
                axis.text.x = element_text(size = 12, color = "black"),
                axis.text.y = element_text(color = "black", size = 12),
                axis.title.x = element_text(vjust = 0, size = 16, face = "bold"),
                axis.title.y = element_text(size = 16, face = "bold"),
                legend.text = element_text(face = "bold", color = "black", size = 12))
      }

    } else {
      # For other plot types that don't need variable selection
      # c. Histogram
      if (analysis_type == "Histogram") {
        long_data <- pivot_longer(data, cols = where(is.numeric),
                                  names_to = "variable",
                                  values_to = "value")
        histogram_plot <- ggplot(long_data, aes(x = value)) +
          geom_histogram(fill = "blue", alpha = 0.5, binwidth = 2) +
          facet_wrap(~ variable, scales = "free") +
          labs(title = "Histogram plot for each numeric variable",
               x = "Value",
               y = "Histogram") +
          theme_classic() +
          theme(axis.ticks = element_blank(),
                axis.line = element_line(colour = "grey50"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
                plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"))
        print(histogram_plot)
      }

      # d. Density plot
      if (analysis_type == "Density plot") {
        long_data <- pivot_longer(data, cols = where(is.numeric),
                                  names_to = "variable",
                                  values_to = "value")
        density_plot <- ggplot(long_data, aes(x = value)) +
          geom_density(fill = "blue", alpha = 0.5) +
          facet_wrap(~ variable, scales = "free") +
          labs(title = "Density plot for each numeric variable",
               x = "Value",
               y = "Density") +
          theme_classic() +
          theme(axis.ticks = element_blank(),
                axis.line = element_line(colour = "grey50"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
                plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"))

        print(density_plot)
      }

      # e. QQ plot
      if (analysis_type == "QQ plot") {
        num_vars <- sum(sapply(loaded_data(), is.numeric))
        ncol <- ceiling(sqrt(num_vars))
        nrow <- ceiling(num_vars / ncol)
        plot_qq(loaded_data(), ncol = ncol, nrow = nrow,
                title = "QQ plot for each numeric variable",
                ggtheme = theme_classic(),
                theme_config = list(axis.ticks = element_blank(),
                                    axis.line = element_line(colour = "grey50"),
                                    panel.grid.minor = element_blank(),
                                    panel.grid.major.x = element_blank(),
                                    panel.grid.major.y = element_blank(),
                                    panel.background = element_rect(fill = "#fbf9f4",
                                                                    color = "#fbf9f4"),
                                    plot.background = element_rect(fill = "#fbf9f4",
                                                                   color = "#fbf9f4")))
      }
    }
  })

  # output for the heatmap
  output$heatmap_plot <- renderPlotly({

    req(loaded_data(),
        input$analysis_type == "Heat map") # Ensure data is loaded and non-null

    # Remove non-numeric columns and NA rows
    numeric_data <- loaded_data() %>%
      select(where(is.numeric)) %>%
      na.omit()

    heatmap<- heatmaply(cor(numeric_data),
                        k_col = 2,
                        k_row = 2,
                        main = " Heatmap showing relationship among the variables",
                        plot_method = "plotly")

    # Customize the heatmap theme
    heatmap <- heatmap %>%
      layout(
        xaxis = list(
          ticks = "",
          showline = FALSE,
          showgrid = FALSE,
          zeroline = FALSE
        ),
        yaxis = list(
          ticks = "",
          showline = FALSE,
          showgrid = FALSE,
          zeroline = FALSE
        ),
        paper_bgcolor = "#fbf9f4",
        plot_bgcolor = "#fbf9f4"
      )


    return(heatmap)
  })
}


# Run the application
shinyApp(ui = ui, server = server)
