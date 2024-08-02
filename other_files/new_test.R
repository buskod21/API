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
library(memoise)

## tell shiny to log all reactivity
library(reactlog)
#reactlog_enable()
# shiny::reactlogShow()
# shiny::reactlogReset()
#install.packages("ggstatsplot")
#library(ggstatsplot)

# Source the functions
source("app_function2.R")

# path to memorized cache file using memoise package
cache_dir <- cache_filesystem("./cache_folder")
memorized_detailed_data <- memoise(fetch_study_details, cache = cache_dir)

# Set global option for the shinyApp
# Disk-based cache storing cached data in the './cache_folder' directory
shinyOptions(cache = cachem::cache_disk("./cache_folder"))

# get the basic data that is used to get detailed data
basic_data <- fetch_all_studies()

# UI Definition
ui <- dashboardPage(
  skin = "blue",
  scrollToTop = TRUE,

  dashboardHeader(
    title = "Menu",
    status = "white",
    h3("Explore borealis")
  ),

  dashboardSidebar(
    collapsed = FALSE,
    minified = FALSE,
    skin = "dark",
    status = "lightblue",
    elevation = 4,
    sidebarMenu(
      menuItem("Explore borealis",
               icon = icon("server"),
               tabName = "Borealisdata")
    )
  ),

  dashboardBody(
    tabItems(
      tabItem(
        tabName = "Borealisdata",
        tabBox(id="mainbox",
               title = "",
               width = 12,
               collapsible = TRUE,
               maximizable = TRUE,
               elevation = 4,
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
                                                              height = "800px"))
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
                  elevation = 4,
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
                                    tags$b("Description of the Dataset"),
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
                                               box(
                                                 title = "View data",
                                                 status = "white",
                                                 solidHeader = TRUE,
                                                 collapsible = TRUE,
                                                 elevation = 3,
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
                                                          elevation = 3,
                                                          width = 12,
                                                          collapsed = F,
                                                          DT::dataTableOutput("structure")
                                                        )
                                                 ),
                                                 column(6,
                                                        box(
                                                          title = "Missing value",
                                                          status = "white",
                                                          solidHeader = TRUE,
                                                          collapsible = TRUE,
                                                          elevation = 3,
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
                                                 elevation = 3,
                                                 width = 12,
                                                 collapsed = F,
                                                 DT::dataTableOutput("summary")
                                               )
                                      ),

                                      tabPanel("Data visualization",
                                               fluidRow(
                                                 column(3,
                                                        box(
                                                          title = "Select Plot type",
                                                          status = "white",
                                                          solidHeader = TRUE,
                                                          collapsible = FALSE,
                                                          elevation = 3,
                                                          width = 12,
                                                          awesomeRadio(
                                                            inputId = "button",
                                                            label = NULL,
                                                            choices = c("Histogram",
                                                                        "Density plot",
                                                                        "QQ plot",
                                                                        "Boxplot",
                                                                        "Scatter plot",
                                                                        "Heat map"),
                                                            selected = "Histogram",
                                                            inline = FALSE
                                                          )
                                                        ),
                                                        # Box for selecting X and Y variables
                                                        box(
                                                          title = "Select variables",
                                                          status = "white",
                                                          solidHeader = TRUE,
                                                          collapsible = FALSE,
                                                          elevation = 3,
                                                          width = 12,
                                                          selectInput("Xvar",
                                                                      "X variable",
                                                                      choices = ""),
                                                          selectInput("Yvar",
                                                                      "Y variable",
                                                                      choices ="")
                                                        )
                                                 ),
                                                 column(9,
                                                        # Box for displaying the plot area
                                                        box(
                                                          title = "Plot Area",
                                                          status = "white",
                                                          solidHeader = TRUE,
                                                          collapsible = FALSE,
                                                          elevation = 3,
                                                          width = NULL,
                                                          withSpinner(plotOutput("plot"))
                                                        )
                                                 )
                                               )
                                      ),
                                      type = "pills",
                                      vertical = TRUE
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

  #Reactive expression to get the memorized detailed data
  detailed_data <- reactive ({
    memorized_detailed_data(basic_data)
  })

  # Reactive expression to extract unique keywords
  keywords <- reactive({
    req(detailed_data())
    data <- detailed_data()
    unique_keywords <- data %>%
      pull(Keywords) %>%
      str_split(",\\s*") %>%
      unlist() %>%
      unique() %>%
      na.omit() %>%
      sort()
    return(unique_keywords)
  })

  # Reactive expression to extract unique authors
  authors <- reactive({
    req(detailed_data())

    data <- detailed_data()

    unique_authors <- data %>%
      pull(Authors) %>%  # Extract the 'Authors' column
      str_split(", ") %>%  # Split each row into individual names
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
    req(input$event_type, detailed_data())

    data <- detailed_data() %>%
      mutate(PublicationDate = as.Date(PublicationDate, format="%Y-%m-%d"))

    if (input$event_type == "Keywords") {
      events <- keywords()
      event_column <- "Keywords"
    } else {
      events <- authors()
      event_column <- "Authors"
    }

    nodes <- map_df(events, function(event) {
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

      tibble(
        id = which(events == event),
        label = event,
        group = input$event_type,
        title = paste("Study Count:", studies_count, "<br>", "Year Range:", year_range)
      )
    })

    return(nodes)
  }) %>%
    bindCache(input$event_type) #Cache the output of this reactive based on the selected event type

  # Reactive expression to create edges data based on selected event type
  edges_data <- reactive({
    req(input$event_type, detailed_data())

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
    bindCache(input$event_type) #Cache the output of this reactive based on the selected event type


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
      visInteraction(hover = TRUE,
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

    data <- detailed_data()

    if (input$event_type == "Keywords") {
      event <- keywords()[input$selectedEvent]
      event_column <- "Keywords"
    } else {
      event <- authors()[input$selectedEvent]
      event_column <- "Authors"
    }
    studies <- data %>%
      filter(str_detect(.data[[event_column]], regex(paste0("\\b", event, "\\b"), ignore_case = TRUE))) %>%
      pull(Title)
    updateSelectInput(session, "study_select", choices = unique(studies))
  })



  # Show study details based on selection
  output$study_details <- DT::renderDataTable({
    req(input$study_select, detailed_data())  # Ensure that a selection has been made and detailed_data is available
    # Fetch the detailed data
    data <- detailed_data()

    # Filter data first before transposing
    filtered_data <- filter(data, Title == input$study_select)

    # Transpose the filtered data
    t_filtered_data <- t(filtered_data)

    # Render the datatable
    datatable(t_filtered_data,
              options = list(dom = 'tp',
                             autoWidth = TRUE,
                             scrollX = TRUE),
              rownames = TRUE,
              colnames = c("Parameter", "Value"))
  })


  # define the list of full-path to store the mapping
  # between base names and full paths
  full_paths <- reactiveVal(list())

  # This observeEvent should trigger once a study is selected either from network plot or directly from UI
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

  # Reactive expression for loading metadata
  loaded_metadata <- reactive({
    req(input$metadata_select)  # Ensure a file is selected

    # Use the full path for reading the file
    selected_file_path <- full_paths()[input$metadata_select]

    # Read the file
    raw_lines <- readLines(selected_file_path)

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

  # Reactive expression for loading data
  loaded_data <- reactive({
    req(input$dataset_select)  # Ensure a file is selected

    # Use the full path for reading the file
    selected_file_path_data <- full_paths()[input$dataset_select]

    # Attempt to read the file, handle failure
    tryCatch({
      fread(selected_file_path_data, header = TRUE)

    }, error = function(e) {
      # Return NULL or handle the error appropriately
      NULL
    })
  })

  ## Output for the data analysis tabpanel ##
  # Logic to render the raw data table
  output$rawtable <- DT::renderDataTable({
    # Use the reactive data
    data_file <- loaded_data()

    DT::datatable(data_file,
                  rownames = FALSE,
                  options = list(dom = "tp",
                                 autoWidth = FALSE,
                                 scrollX = TRUE))
  })

  #logic to render the data structure
  output$structure <- renderDataTable({
    data_file <- loaded_data()
    data <-introduce(data_file)

    data <- data %>%
      tibble %>%
      t()

    DT::datatable(data,
                  rownames = TRUE,
                  colnames = c("Key", "Value"),
                  options = list(dom = "tp",
                                 autoWidth = FALSE,
                                 scrollX = TRUE))
  })

  # logic for the missing value
  output$missing_value <- renderPlot({
    data_file <- loaded_data()

    plot_missing(data_file)
  })

  # logic for summary statistics
  output$summary <- renderDataTable({
    data_file <- loaded_data()

    # conditional statement to handle non-mumeric column
    if(!is.data.frame(data_file)) stop("df needs to be a dataframe")
    data_file <- as.data.frame(data_file)
    data_file <- data_file[sapply(data_file, is.logical) | sapply(data_file, is.numeric)]
    if ((ncol(data_file) < 1) | (nrow(data_file) < 2)) stop("insuitable data frame (does it contain numerical data?)")

    # compute the descriptive statistics
    data <- cbind(apply(data_file,2,function(x) as.integer(sum(!is.na(x)))),
                  apply(data_file,2,mean, na.rm=TRUE),
                  apply(data_file,2,stats::sd, na.rm=TRUE),
                  t(apply(data_file, 2, function(x)
                    stats::quantile(x,
                                    probs=c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE))))

    # round up the decimal place for each column
    data <- round (data, digits = c(0, 3, 3, 3, 3, 3, 3, 3))

    # Assign column names
    colnames(data) <- c("N", "Mean", "Std. dev.", "Min.", "25 %", "Median", "75 %", "Max.")


    DT::datatable(data,
                  rownames = TRUE,
                  options = list(dom = "tp",
                                 autoWidth = FALSE,
                                 scrollX = TRUE))
  })

  # Update selectInput for Xvar and Yvar based on selected data
  observe({
    data_file <- loaded_data()
    req(data_file)

    updateSelectInput(session,
                      inputId = 'Xvar',
                      label = 'X',
                      choices = colnames(data_file))

    # Update selectInput for Yvar based on selected data
    observe({
      data_file <- loaded_data()
      req(data_file)  # Ensure plot_file is set

      Ychoices <- subset(colnames(data_file), !(colnames(data_file) %in% input$Xvar))
      updateSelectInput(session,
                        inputId = 'Yvar',
                        label = 'Y',
                        choices = Ychoices)
    })
  })


  # Render the plot based on user input
  output$plot <- renderPlot({
    # Use the reactive data
    data_file <- loaded_data()
    req(data_file, input$Xvar, input$Yvar)  # Ensure these inputs are set

    # Clean column names by keeping only letters and numbers
    plot_data <- data_file
    names(plot_data) <- gsub("[^A-Za-z0-9]", "", names(plot_data))

    # Map original input variable names to cleaned names
    x_var_clean <- names(plot_data)[names(data_file) == input$Xvar]
    y_var_clean <- names(plot_data)[names(data_file) == input$Yvar]

    req(x_var_clean, y_var_clean)  # Ensure variables are found


    # Convert the selected X variable to a factor
    plot_data <- plot_data
    plot_data[[x_var_clean]] <- factor(plot_data[[x_var_clean]])

    # Plotting logic with cleaned column names
    plot_type <- input$button

    # a. Histogram
    if (plot_type == "Histogram") {
      # Count numeric variables
      num_vars <- sum(sapply(plot_data, is.numeric))

      # Calculate the number of columns and rows
      # Simple square root strategy to determine layout
      ncol <- ceiling(sqrt(num_vars))
      nrow <- ceiling(num_vars / ncol)

      # Plotting histograms with dynamic ncol and nrow
      plot_histogram(plot_data,
                     ncol = ncol,
                     nrow = nrow,
                     ggtheme = theme_classic(),
                     theme_config = list(axis.ticks = element_blank(),
                                         axis.line = element_line(colour = "grey50"),
                                         panel.grid.minor = element_blank(),
                                         panel.grid.major.x = element_blank(),
                                         panel.grid.major.y = element_line(linetype = "dashed"),
                                         panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
                                         plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4")))
    }

    # b. Density plot
    if (plot_type == "Density plot") {
      # Reshape the data to long format
      long_data <- pivot_longer(plot_data,
                                cols = where(is.numeric),
                                names_to = "variable",
                                values_to = "value")

      # Create the density plot
      density_plot <- ggplot(long_data, aes(x = value)) +
        geom_density(fill = "blue", alpha = 0.5) +
        facet_wrap(~ variable, scales = "free") +
        labs(title = "Density Plot for Each Numeric Variable",
             x = "Value",
             y = "Density") +
        theme_classic() +
        theme(axis.ticks = element_blank(),
              axis.line = element_line(colour = "grey50"),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_line(linetype = "dashed"),
              panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
              plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"))

      # Print the plot
      print(density_plot)
    }

    # c. QQ plot
    if (plot_type == "QQ plot") {

      # Count numeric variables
      num_vars <- sum(sapply(plot_data, is.numeric))

      # Calculate the number of columns and rows
      # Simple square root strategy to determine layout
      ncol <- ceiling(sqrt(num_vars))
      nrow <- ceiling(num_vars / ncol)

      # Plotting histograms with dynamic ncol and nrow
      plot_qq (plot_data,
               ncol = ncol,
               nrow = nrow,
               ggtheme = theme_classic(),
               theme_config = list(axis.ticks = element_blank(),
                                   axis.line = element_line(colour = "grey50"),
                                   panel.grid.minor = element_blank(),
                                   panel.grid.major.x = element_blank(),
                                   panel.grid.major.y = element_line(linetype = "dashed"),
                                   panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
                                   plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4")))
    }

    # d. Boxplot
    if (plot_type == "Boxplot") {
      boxplt <- ggplot(plot_data, aes_string(x = x_var_clean,  y = y_var_clean, group = x_var_clean, color = x_var_clean)) +
        geom_boxplot() +
        theme_classic()+
        theme(
          strip.text = element_text(face = "bold", size=12),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(linetype = "dashed"),
          panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
          plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
          plot.caption = element_text(hjust = 0, size=14),
          axis.line = element_line(colour = "grey50"),
          axis.text.x = element_text(vjust = 1.2,size = 12,color = "black"),
          axis.text.y = element_text(color = "black", size=12),
          axis.title.x = element_text(vjust = 0, size= 14),
          axis.title.y = element_text(size = 14),
          legend.title = element_blank(),
          legend.position = "none",
          legend.text=element_text(size=rel(1.1)))

      print(boxplt)
    }

    # e. Scatter plot
    if (plot_type == "Scatter plot") {
      ggplot(plot_data, aes_string(x = x_var_clean, y = y_var_clean, color = x_var_clean)) +
        geom_point() +
        theme_classic()+
        theme(
          strip.text = element_text(face = "bold", size=12),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(linetype = "dashed"),
          panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
          plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
          plot.caption = element_text(hjust = 0, size=14),
          axis.line = element_line(colour = "grey50"),
          axis.text.x = element_text(vjust = 1.2,size = 12,color = "black"),
          axis.text.y = element_text(color = "black", size=12),
          axis.title.x = element_text(vjust = 0, size= 14),
          axis.title.y = element_text(size = 14),
          legend.title = element_blank(),
          legend.position = "none",
          legend.text=element_text(size=rel(1.1)))
    }

    # f. Heat map
    else if (plot_type == "Heat map") {
      plot_correlation(na.omit(plot_data),
                       maxcat = 5L,
                       ggtheme = theme_classic(),
                       theme_config = list(axis.ticks = element_blank(),
                                           axis.line = element_line(colour = "grey50"),
                                           panel.grid.minor = element_blank(),
                                           panel.grid.major.x = element_blank(),
                                           panel.grid.major.y = element_line(linetype = "dashed"),
                                           panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
                                           plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4")))
    }
  })
}


# Run the application
shinyApp(ui = ui, server = server)
