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

#install.packages("ggstatsplot")
#library(ggstatsplot)

# Source the functions
source("app_function2.R")

shinyOptions(cache = cachem::cache_disk("./cache_folder/cache/"))

# UI Definition
ui <- dashboardPage(
  skin = "lightblue",
  fullscreen = TRUE,
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
                                                 title = "View raw data",
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
                                                          title = "Distribution",
                                                          status = "white",
                                                          solidHeader = TRUE,
                                                          collapsible = FALSE,
                                                          elevation = 3,
                                                          width = 12,
                                                          awesomeRadio(
                                                            inputId = "button_1",
                                                            label = NULL,
                                                            choices = c("Histogram",
                                                                        "Density plot",
                                                                        "QQ plot"),
                                                            selected = "Histogram",
                                                            inline = FALSE
                                                          )
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
                                                          withSpinner(plotOutput("plot_1"))
                                                        )
                                                 )
                                               ),

                                               # Break between box
                                               br(),

                                               # Box for selecting X and Y variables
                                               fluidRow(
                                                 column(3,
                                                        box(
                                                          title = "Inferential statistics",
                                                          status = "white",
                                                          solidHeader = TRUE,
                                                          collapsible = FALSE,
                                                          elevation = 3,
                                                          width = 12,
                                                          awesomeRadio(
                                                            inputId = "button_2",
                                                            label = NULL,
                                                            choices = c("Boxplot",
                                                                        "Scatter plot",
                                                                        "Linear regression",
                                                                        "Anova",
                                                                        "Heat map"),
                                                            selected = NULL,
                                                            inline = FALSE),

                                                          hr(),
                                                          h5("Select variable"),

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
                                                          withSpinner(plotOutput("plot_2"))
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

  # get the basic data that is used to get detailed data
  basic_data <- fetch_all_studies()

  #Reactive expression to get detailed data
  detailed_data <- reactive ({
    fetch_study_details(basic_data)
  }) %>%
    bindCache(basic_data)


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

  # Reactive expression for loading data
  loaded_data <- reactive({

    req(input$dataset_select)  # Ensure a file is selected

    # Use the full path for reading the file
    selected_file_path_data <- full_paths()[input$dataset_select]

    # Read the selected data using fread() function
    data <- fread(selected_file_path_data, header = TRUE)

    # Convert real numeric identified as character into numeric variable by
    # applying the selective conversion
    data <- data %>%
      mutate(across(where(~ is.character(.) && !contains_letters(.)), as.numeric))%>%
      suppressWarnings()

    return(data)
  })


  ## Output for the data analysis tabPanel ##

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

    plot_missing(loaded_data())
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


    DT::datatable(data,
                  rownames = TRUE,
                  options = list(dom = "tp",
                                 autoWidth = FALSE,
                                 scrollX = TRUE))
  })

  # Logic for data visualization tab

  # Description
  output$plot_1 <- renderPlot({
    req(loaded_data()) # Ensures data is loaded

    # Reshape the data to long format
    long_data <- pivot_longer(loaded_data(),
                              cols = where(is.numeric),
                              names_to = "variable",
                              values_to = "value")


    plot_type <- input$button_1

    # a. Histogram
    if (plot_type == "Histogram") {
      histogram_plot <- ggplot(long_data, aes(x = value)) +
        geom_histogram(fill = "blue", alpha = 0.5, binwidth = 2) +
        facet_wrap(~ variable, scales = "free") +
        labs(title = "Histogram plot for each numeric variable",
             x = "Value",
             y = "Histogram")+
        theme_classic() +
        theme(axis.ticks = element_blank(),
              axis.line = element_line(colour = "grey50"),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_line(linetype = "dashed"),
              panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
              plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"))

      # Print the plot
      print(histogram_plot)
    }

    # b. Density plot
    if (plot_type == "Density plot") {
      # Create the density plot
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
              panel.grid.major.y = element_line(linetype = "dashed"),
              panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
              plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"))

      # Print the plot
      print(density_plot)
    }

    # c. QQ plot
    else if (plot_type == "QQ plot") {

      # Count numeric variables
      num_vars <- sum(sapply(loaded_data(), is.numeric))

      # Calculate the number of columns and rows
      # Simple square root strategy to determine layout
      ncol <- ceiling(sqrt(num_vars))
      nrow <- ceiling(num_vars / ncol)

      # Plotting histograms with dynamic ncol and nrow
      plot_qq (loaded_data(),
               ncol = ncol,
               nrow = nrow,
               title = "QQ plot for each numeric variable",
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

  # Logic to render inferential statistics
  # Update selectInput for Xvar and Yvar based on selected data
  observe({

    req(loaded_data()) # Ensure data is loaded

    updateSelectInput(session,
                      inputId = 'Xvar',
                      label = 'X',
                      choices = colnames(loaded_data()))

    # Update selectInput for Yvar based on selected data
    observe({
      Ychoices <- subset(colnames(loaded_data()),
                         !(colnames(loaded_data()) %in% input$Xvar))
      updateSelectInput(session,
                        inputId = 'Yvar',
                        label = 'Y',
                        choices = Ychoices)
    })
  })


  # Render the plot based on user input
  output$plot_2 <- renderPlot({

    req(loaded_data(), input$Xvar, input$Yvar) # Ensure these inputs are set


    # Clean column names by keeping only letters and numbers
    plot_data <- loaded_data()
    names(plot_data) <- gsub("[^A-Za-z0-9]", "", names(plot_data))

    # Map original input variable names to cleaned names
    x_var_clean <- names(plot_data)[names(loaded_data()) == input$Xvar]
    y_var_clean <- names(plot_data)[names(loaded_data()) == input$Yvar]

    req(x_var_clean, y_var_clean)  # Ensure variables are found

    # Convert the selected X variable to a factor
    #plot_data <- plot_data
    plot_data[[x_var_clean]] <- factor(plot_data[[x_var_clean]])

    # Plotting logic with cleaned column names
    plot_type <- input$button_2

    # a.  Boxplot
    if (plot_type == "Boxplot") {
      boxplt <- ggplot(plot_data,
                       aes_string(x = x_var_clean,
                                  y = y_var_clean,
                                  group = x_var_clean,
                                  color = x_var_clean)) +
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

    # b. Scatter plot
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

    # c. Heat map
    else if (plot_type == "Heat map") {
      mtscaled <- as.matrix(scale(na.omit(loaded_data())))
      heatmap(mtscaled,
              col = topo.colors(200, alpha=0.5),
              Colv=F, scale="none")
      # plot_correlation(na.omit(plot_data),
      #                  maxcat = 5L,
      #                  ggtheme = theme_classic(),
      #                  theme_config = list(axis.ticks = element_blank(),
      #                                      axis.line = element_line(colour = "grey50"),
      #                                      panel.grid.minor = element_blank(),
      #                                      panel.grid.major.x = element_blank(),
      #                                      panel.grid.major.y = element_line(linetype = "dashed"),
      #                                      panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
      #                                      plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4")))
    }
  })
}


# Run the application
shinyApp(ui = ui, server = server)
