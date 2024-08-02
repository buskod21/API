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
library(readxl)
library(pdftools)
library(sas7bdat)
library(XML)
library(shinyalert)


# Source the functions
source("dataExplorer_functions.R")

## tell shiny to log all reactivity
library(reactlog)
reactlog_enable()
# reactlogShow()
reactlogReset()

# Fetch OAC dataverse content
OAC_info <- fetch_oac_info()


# Create a folder to store the cached information
shinyOptions(cache = cachem::cache_disk("./cache_folder/cache/"))

ui <- dashboardPage(
  skin = "lightblue",
  scrollToTop = TRUE,
  fullscreen = TRUE,
  help = NULL,
  dark = NULL,

  dashboardHeader(
    status = "lightblue",
    fixed = TRUE,

    # Custom CSS for styling the navbar
    tags$head(
      tags$style(HTML("
      .navbar-nav .nav-link.active {
        color: #fff;
        background-color: #3c8dbc;
      }
      .navbar-nav > li > a {
        display: grid;
        grid-template-columns: auto 1fr;
        align-items: baseline;
        justify-content: space-evenly;
      }
      .navbar-nav > li > a > i {
        margin-right: 5px; /* Margin between icon and text */
      }
    "))
    ),

    # Define the navbar menu
    navbarMenu(
      id = "navmenu",
      # Home tab
      navbarTab(tabName = "home_tab", text = tags$b("Home"), icon = icon("home")),
      # About tab
      navbarTab(tabName = "about_tab", text = tags$b("About"), icon = icon("book")),

      # Explore Borealis menu with sub-tabs
      navbarTab(
        text = tags$b("Explore Borealis"),

        # Study Network tab
        navbarTab(
          tabName = "network_tab",
          text = tags$span(
            style = "color: black; display: inline-flex; align-items: center; white-space: nowrap;",
            icon("sitemap", style = "color: black; margin-right: 5px;"),
            tags$b("Study Network")
          )
        ),

        # Data review tab
        navbarTab(
          tabName = "borealis_tab",
          text = tags$span(
            style = "color: black; display: inline-flex; align-items: center; white-space: nowrap;",
            icon("chart-bar", style = "color: black; margin-right: 5px;"),
            tags$b("Data review")
          )
        )
      )
    )
  ),

  dashboardSidebar(disable = TRUE),

  dashboardBody(
    tags$head(tags$style(".nav-pills .nav-link.active {color: #fff; background-color: #3c8dbc;}")),
    tags$head(tags$style(".nav-pills .nav-link:not(.active):hover {color: #3c8dbc !important;}")),

    tabItems(
      tabItem(tabName = "home_tab",
              tags$h3(strong("RE-USABLE DATA EXPLORER APP")),
              br(),
              tags$div(
                br(),
                tags$h4(tags$b("Explore Re-Usable data")),
                tags$hr(style = "border: 1px solid #3c8dbc; width: 150px; text-align: left; margin-left: 0;"),
                p("The reusable data explorer App, developed by Agri-food Data Canada,
            allows researchers to assess and explore historical data deposited in the
            borealis repository. This tool facilitates a preliminary review of past research data,
            and can also enable the integration of historical datasets into larger, contemporary projects.
            It provides access to detailed study information and exploratory data analysis functions.
            Such application enhances the efficiency and depth of research in historical studies."),
                style = "background-color:lightgrey; padding: 10px; width: 50%; text-align: left;"
              ),
              br(), br(),

              tags$div(
                tags$h4(tags$b("Mission of AgriFood Data Canada")),
                tags$hr(style = "border: 1px solid #3c8dbc; width: 150px; margin: auto;"),
                br(),
                p("Agri-food Data Canada, situated at the University of Guelph,
            strives to enhance the utility of research data by offering comprehensive
            tools and training aligned with the principles of making data Findable,
            Accessible, Interoperable, and Reusable (FAIR).
            We are dedicated to simplifying the application of these principles,
            ensuring researchers can maximize the impact of their data.
            By leveraging historical data and integrating past research into larger datasets,
            we aim to drive innovation and technology.
            Our commitment is to support U of G faculty in their research efforts,
            providing the necessary resources to facilitate groundbreaking discoveries.
            Learn more about the Agric-Food data Canada at the University of Guelph,",
                  tags$a(href = "https://agrifooddatacanada.ca/",
                         tags$b("here."), target = "_blank", style = "color:#3c8dbc;")
                ),
                style = "background-color:lightgrey; padding: 10px; width: 50%; text-align: center; margin: 0 auto;"
              ),
              br(),

              tags$div(
                tags$h4(tags$b("Unique Features of the App"), style = "text-align: center;"),
                tags$hr(style = "border: 1px solid #3c8dbc; width: 150px; margin: auto;"),
                br(),
                tags$div(
                  style = "display: flex; justify-content: space-around; align-items: flex-start;",
                  tags$div(
                    style = "flex: 1; text-align: center; padding: 10px;",
                    tags$h5(tags$b("Network")),
                    tags$img(src = "network.png", height = "100px"),
                    p("The network feature visually represents how studies are interconnected
                through shared keywords and authors, enhancing the discovery of related research.
                By showcasing these connections, it facilitates easier navigation and fosters a
                deeper understanding of the research landscape and collaborative opportunities.")
                  ),
                  tags$div(
                    style = "flex: 1; text-align: center; padding: 10px;",
                    tags$h5(tags$b("Metadata")),
                    tags$img(src = "Metadata.png", height = "100px"),
                    p("Metadata provides researchers with critical information about the data,
                enabling a comprehensive understanding of its context and content.
                It includes details on each variable, data collection methodologies,
                and study objectives, ensuring clarity, reliability, and effective
                data integration for accurate and innovative research.")
                  ),
                  tags$div(
                    style = "flex: 1; text-align: center; padding: 10px;",
                    tags$h5(tags$b("Exploratory Data Analysis")),
                    tags$img(src = "analysis.png", height = "100px"),
                    p("The Data Exploration tab empowers users to perform Exploratory Data Analysis (EDA)
                on various studies, enhancing the reusability of historical data.
                By utilizing visualization tools and statistical techniques,
                users can uncover patterns, trends, and relationships, summarize
                key data characteristics, identify anomalies, and integrate historical
                data into contemporary projects, driving innovation and advancing research outcomes.")
                  )
                ),
                style = "padding: 10px; width: 100%; text-align: center; margin: 0 auto;"
              ),

              tags$div(
                tags$h4(tags$b("How the App works"), style = "text-align: center;"),
                tags$hr(style = "border: 1px solid #3c8dbc; width: 100px; margin: auto;"),
                br(),
                p("The ", strong("Explore borealis tab"),
                  "allows user to view the networks of studies in the OAC repository
            on borealis based on keywords and author's name. Users can
            then select a particular keyword or author to further explore
            the associated studies.The study overview, metadata and explore data
            tab allows users to gain a deeper insight about the study and data. Click ",
                  tags$a(href = "https://borealisdata.ca/dataverse/oac",
                         tags$b("here"),
                         target = "_blank", style = "color:#3c8dbc;"),
                  " to access the OAC dataverse in the Borealis database."),
                style = "background-color:lightgrey; padding: 10px; width: 50%;
            text-align: center; margin: 0 auto;"
              ),

              br(),

              tags$div(
                style = "text-align: center;",
                p(tags$h3(tags$strong("Have fun exploring re-usable historical data!")))
              )
      ),

      tabItem(tabName = "about_tab",
              tags$div(
                tags$h3(strong("ABOUT")),
                br(),
                tags$h4(tags$b("How it started")),
                tags$hr(style = "border: 1px solid #3c8dbc; width: 80px;
                        text-align: left; margin-left: 0;"),
                p("Have you ever wondered where all the data from past research are?
            Have you considered the immense value these datasets hold,
            especially in the era of big data, machine learning, and AI?
            Integrating such valuable datasets into current models can drive
            innovation and automation, unlocking new possibilities and advancements."
                ),
                p("This app was developed as part of an initiative to enhance the value
            of research data by adhering to the FAIR data principles.
            These principles ensure that data is Findable, Accessible, Interoperable,
            and Reusable (FAIR). By simplifying the application of these principles,
            the app significantly improves the usability and value of research data,
            benefiting researchers, industry professionals, and stakeholders in the Agri-Food sector."
                ),
                p("The Agri-Food Data Canada at the University of Guelph,
            strives to support researchers in maximizing the potential of their data,
            fostering innovation, and advancing the agrifood sector."
                ),
                style = "padding: 10px; width: 100%; text-align: left; width: 50%"
              ),

              br(),

              tags$div(
                tags$h4(tags$b("The workshop"), style = "text-align: center;"),
                tags$hr(style = "border: 1px solid #3c8dbc; width: 70px; margin: auto;"),
                br(),
                p("The University of Guelph hosted a four-day workshop focused on
            enhancing research data reusability, led by experts Michelle Edwards and Lucas Alcantara.
            This event provided hands-on training in data organization, documentation,
            and sharing, emphasizing the FAIR data principles—Findable, Accessible,
            Interoperable, and Reusable. Participants learned best practices for
            metadata creation, data governance, and exploratory data analysis
            using R and Shiny frameworks. The workshop highlighted the importance of
            integrating historical datasets into current models to drive innovation and
            automation in research."),
                p("By equipping researchers with practical skills and tools,
            the workshop aimed to foster a mindset of data reusability,
            ensuring that valuable research data can be effectively reused
            to support advancements in the Agri-Food sector. At the end of the workshop,
            the idea to create an app that simplifies the process of accessing and
            understanding historical data was born, further supporting researchers
            in maximizing the potential of their data for future discoveries.
            This initiative was supported by Compute Ontario and
            included both online and in-person sessions at the Ontario Dairy Research Centre."),
                p("You can find more information about the workshop and access the workshop materials on Github",
                  tags$a(href = "https://github.com/agrifooddatacanada/RRDMS_Workshop",
                         tags$b("here."), target = "_blank", style = "color:#3c8dbc;")),  # Create a hyperlink
                img(src = 'workshop1.jpeg', height = 350, width = 450),
                style = "background-color:lightgrey; padding: 10px; width: 60%; text-align: center; margin: 0 auto;"
              ),

              br(),

              tags$div(
                tags$h4(tags$b("Funding")),
                tags$hr(style = "border: 1px solid #3c8dbc; width: 60px; text-align: left; margin-left: 0;"),
                br(),
                p(),
                style = "padding: 10px; width: 100%; text-align: left; width: 50%"
              ),

              br(),

              tags$div(
                tags$h4(tags$b("Licensing"), style = "text-align: center;"),
                tags$hr(style = "border: 1px solid #3c8dbc; width: 60px; margin: auto;"),
                br(),
                p(),
                style = "background-color:lightgrey; padding: 10px; width: 60%; text-align: center; margin: 0 auto;"
              )
      ),

      tabItem(tabName = "network_tab",
              fluidRow(
                column(12,
                       box(
                         title = tags$b("Select dataverse to view"),
                         width = 12,
                         collapsible = TRUE,
                         side = "right",
                         type = "pills",
                         sidebar = boxSidebar(id = "infoPanel",
                                              uiOutput("nodeInfo"),
                                              width = 25,
                                              startOpen = FALSE
                         ),
                         pickerInput(
                           inputId = "select_dataverse",
                           label = "",
                           choices = setNames(OAC_info$id, OAC_info$title),
                           multiple = TRUE,
                           ),
                         br(),
                         awesomeRadio(
                           inputId = "event_type",
                           label = "View network by :",
                           choices = c("Keywords", "Authors"),
                           selected = "Keywords",
                           inline = TRUE
                         ),
                         hr(),
                         withSpinner(visNetworkOutput("networkPlot",
                                                      width="100%",
                                                      height = "800px")
                         )
                       )
                )
              )
      ),

      tabItem(tabName = "borealis_tab",
              fluidRow(
                column(width= 12,
                       div(id = "selectDiv",
                           selectizeInput(
                             inputId = "study_select",
                             label = "Study selection",
                             choices = NULL,
                             selected = "",
                             multiple = TRUE,
                             options = list(maxItems = 1, placeholder = "Nothing selected ..."),
                             width = "100%"
                           )
                       )
                )
              ),

              br(),

              shinyjs::useShinyjs(),
              shinyjs::hidden(
                div(id = "overviewBox",
                    withSpinner(
                      tabBox(
                        title = "",
                        width = 12,
                        collapsible = TRUE,
                        maximizable = TRUE,
                        elevation = 1,
                        solidHeader = FALSE,
                        status = "lightblue",
                        tabPanel("Study Overview",
                                 fluidRow(
                                   column(12,
                                          withSpinner(
                                            DT::dataTableOutput("study_details")
                                          )
                                   )
                                 )
                        ),
                        tabPanel("Metadata",
                                 fluidRow(
                                   column(12,
                                          selectInput("metadata_select",
                                                      "Select a Metadata",
                                                      choices = NULL,
                                                      selected = NULL),
                                          br(),
                                          tags$b("Data description"),
                                          hr(),
                                          uiOutput("meta"),
                                          tags$b("Data Schema"),
                                          hr(),
                                          uiOutput("schema")
                                   )
                                 )
                        ),
                        tabPanel("Data exploration",
                                 fluidRow(
                                   column(12,
                                          selectInput("dataset_select",
                                                      "Select a dataset",
                                                      choices = NULL,
                                                      selected = NULL),
                                          br(),
                                          bs4Dash::tabsetPanel(id = "datafiles",
                                                               tabPanel("View all datafile",
                                                                        value = "view_alldata",
                                                                        br(),
                                                                        DT::dataTableOutput("view_alldata")),
                                                               tabPanel("Data summary",
                                                                        value = "filtered_data1",
                                                                        br(),
                                                                        box(
                                                                          title = "View data",
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
                                                                        value = "filtered_data1",
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

  # Reactive expression for fetching all data in OAC dataverses
  all_data <- reactive ({

    req(OAC_info)

    allIDs <- as.character(OAC_info$id)

    # Initialize an empty data frame to store the results
    allIDs_details <- tibble()

    # Create a named vector for ID to Title mapping
    id_to_title <- setNames(OAC_info$title, OAC_info$id)

    # Loop through each ID to fetch and append data
    for (id in allIDs) {
      # Fetch dataverse data for the current ID
      alldataverse_data <- fetch_dataverse_data(id)

      # Fetch study details for the current dataverse data
      allstudy_details <- fetch_study_details(alldataverse_data)

      # Append the dataverse name to the data instead of the ID
      dataverse_name <- id_to_title[id]

      allstudy_details <- mutate(allstudy_details, DataverseName = dataverse_name)

      # Combine with previous results
      allIDs_details <- bind_rows(allIDs_details, allstudy_details)
    }

    return(allIDs_details)
  }) %>%
    bindCache(OAC_info) # Cache all_data() based on changes in OAC_info


  # Reactive expression for fetching detailed data based on selected dataverse
  detailed_data <- reactive({

    req(OAC_info, input$select_dataverse)

    # IDs selected by the user
    selectedIDs <- input$select_dataverse

    # Initialize an empty data frame to store the results
    all_details <- tibble()

    # Create a named vector for ID to Title mapping
    id_to_title <- setNames(OAC_info$title, OAC_info$id)

    # Loop through each selected ID to fetch and append data
    for (id in selectedIDs) {
      # Fetch dataverse data for the current ID
      dataverse_data <- fetch_dataverse_data(id)

      # Fetch study details for the current dataverse data
      study_details <- fetch_study_details(dataverse_data)

      # Append the dataverse name to the data instead of the ID
      dataverse_name <- id_to_title[id]

      study_details <- mutate(study_details, DataverseName = dataverse_name)

      # Combine with previous results
      all_details <- bind_rows(all_details, study_details)
    }

    return(all_details)
  }) %>%
    # Cache detailed_data() based on changes OAC_info and input$select_dataverse
    bindCache(OAC_info, input$select_dataverse)


  # Update study_select i.e., select a study dropdown menu when select_dataverse is changed
  observeEvent(input$select_dataverse, {
    data <- if (is.null(input$select_dataverse) || length(input$select_dataverse) == 0) {
      all_data()
    } else {
      detailed_data()
    }

    choices <- lapply(split(
      data$Title,
      data$DataverseName),
      as.list)

    if ("Title" %in% names(data)) {
      updateSelectizeInput(session, "study_select", choices = choices, selected = NULL)
    }
  }, ignoreNULL = FALSE)


  # Reactive expression for extracting and processing keywords
  keywords <- reactive({

    req(detailed_data())

    data <- detailed_data()

    if ("Keywords" %in% names(data)) {
      keywords <- data$Keywords %>%
        str_split(";\\s*") %>%
        unlist() %>%
        unique() %>%
        na.omit() %>%
        sort()
    } else {
      print("Keywords column not found")
      NULL
    }
  })

  # Reactive expression for extracting and processing authors
  authors <- reactive({

    req(detailed_data())

    data <- detailed_data()

    if ("Authors" %in% names(data)) {
      authors <- data$Authors %>%
        str_split(";\\s*") %>%
        unlist() %>%
        unique() %>%
        na.omit() %>%
        sort()
    } else {
      print("Authors column not found")
      NULL
    }
  })

  # Reactive expression to prepare network data (i.e., nodes and edges)
  network_data_reactive <- reactive({
    req(detailed_data(), input$event_type, input$select_dataverse)

    # Only detailed_data() is needed for the network plot
    data <- detailed_data()

    # Generate dynamic colors for selected dataverses
    unique_dataverses <- unique(data$DataverseName)
    dataverse_colors <- setNames(rainbow(length(unique_dataverses),
                                         alpha = 0.85,
                                         start = 0.85,
                                         end = 0.1),
                                 unique_dataverses)

    # Conditional statement to toggle between keywords and authors
    if (input$event_type == "Keywords") {
      events <- keywords()
      event_column <- "Keywords"
    } else {
      events <- authors()
      event_column <- "Authors"
    }

    # Node processing
    nodes <- map_df(events, function(event) {

      matched_papers <- data %>%
        filter(grepl(tolower(event),
                     tolower(.data[[event_column]]),
                     fixed = TRUE))

      studies_count <- nrow(matched_papers)
      year_range <- if (studies_count > 0) {
        paste(min(matched_papers$PublicationDate, na.rm = TRUE),
              "to",
              max(matched_papers$PublicationDate, na.rm = TRUE))
      } else {
        "No found studies"
      }

      # Assign color based on the dataverse name
      dataverse_name <- matched_papers$DataverseName[1] # Assuming each paper has a DataverseName
      color <- if (!is.na(dataverse_name) && dataverse_name %in% names(dataverse_colors)) {
        dataverse_colors[dataverse_name]
      } else {
        "lightblue"  # Default color if no dataverse name is found
      }


      tibble(
        id = which(events == event),
        label = event,
        group = input$event_type,
        title = paste("Study Count:", studies_count, "<br>", "Year Range:", year_range),
        color = color  # Assign dynamically determined color
      )
    })


    # Edge processing
    edges <- expand.grid(from = nodes$id, to = nodes$id) %>%
      filter(from != to) %>%
      mutate(count = map2_dbl(from,
                              to,
                              ~ sum(grepl(nodes$label[.x],
                                          data[[event_column]],
                                          ignore.case = TRUE) &
                                      grepl(nodes$label[.y],
                                            data[[event_column]],
                                            ignore.case = TRUE),
                                    na.rm = TRUE))) %>%
      filter(count > 0) %>%
      select(from, to)

    list(nodes = nodes, edges = edges)

  }) %>% bindCache(input$event_type, detailed_data())  # Cache based on detailed_data() and event type

  # Render the network plot
  output$networkPlot <- renderVisNetwork({
    req(network_data_reactive()) # Ensures required data is available

    network_data <- network_data_reactive()

    visNetwork(network_data$nodes,
               network_data$edges,
               width = "100%",
               height = "800px") %>%
      visNodes(shape = " circle",
               color = list(background = "field:color", border = "black", highlight = "#ff0"),
               scaling = list(label = list(enabled = TRUE,
                                           min = 10,
                                           max = 30)),
               font = list(color = "black", face = "bold", size = 20)) %>%
      visEdges(smooth = FALSE) %>%
      visOptions(highlightNearest = TRUE,
                 nodesIdSelection=list(enabled = TRUE,
                                       main = paste("Select by ", input$event_type),
                                       style = 'width: 300px; height: 26px;')) %>%
      visLayout(randomSeed = 123) %>%  # Consistent layout
      visPhysics(solver = "forceAtlas2Based",
                 timestep = 0.3,
                 minVelocity = 60) %>%
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
    req(input$event_type, input$select_dataverse, detailed_data())

    data <- detailed_data()

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
        dplyr::filter(stringr::str_detect(.data[[event_column]],
                                          stringr::regex(paste0(event),
                                                         ignore_case = TRUE)))

      # Retrieve studies and DOIs from filtered data
      studies <- filtered_data %>% dplyr::pull(Title)
      dois <- filtered_data %>% dplyr::pull(DOI) %>% unique()

      # Generate appropriate text for DOIs
      if (length(dois) == 0) {
        doi_text <- "No DOI found."
      } else {
        doi_links <- sapply(dois, function(doi) {
          paste0('<a class="badge badge-info" href="https://doi.org/', doi, '" target="_blank">', doi, '</a>')
        })
        doi_text <- paste(doi_links, collapse = ", ")
      }

      # Determine which information to display based on event type
      if (input$event_type == "Authors") {
        collaborators <- filtered_data %>%
          dplyr::pull(Authors) %>%
          stringr::str_split(",") %>%
          unlist() %>%
          stringr::str_trim() %>%
          unique() %>%
          setdiff(event) %>%
          length()

        # Prepare the output information with HTML for Authors
        info_html <- tags$div(
          tags$p(tags$b("Number of studies: "), length(unique(studies))),
          tags$p(tags$b("Number of collaborators: "), collaborators),
          tags$p(tags$b("DOIs of studies: "), HTML(doi_text))
        )
      } else {
        # Prepare the output information with HTML for Keywords
        info_html <- tags$div(
          tags$p(tags$b("Number of studies: "), length(unique(studies))),
          tags$p(tags$b("DOIs of studies: "), HTML(doi_text))
        )
      }

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
    updateSelectizeInput(session, "study_select", choices = unique(studies), selected = NULL)
  })

  # Observe changes in the tabPanel selection
  observeEvent(input$study_select, {
    # Check if a study is selected
    if (!is.null(input$study_select) && input$study_select != "") {
      # Show the box
      shinyjs::show("overviewBox")
    } else {
      # Hide the box if no study is selected
      shinyjs::hide("overviewBox")
    }
  })

  #  From here, dynamically choose between all_data() and detailed_()
  # Show study details based on selection
  output$study_details <- DT::renderDataTable({

    req(input$study_select)  # Ensure that a selection has been made and detailed_data is available


    # Dynamically choose between all_data() and detailed_() to display study details
    data <- if (is.null(input$select_dataverse)) all_data() else detailed_data()

    # Filter data first before transposing
    filtered_data <- data %>%
      mutate(PeriodCovered = str_replace_all(PeriodCovered, "/", " - ")) %>%
      filter(Title == input$study_select) %>% # Filter data first before transposing
      t() # Transpose the filtered data

    # Render the datatable
    datatable(filtered_data,
              rownames = TRUE,
              colnames = c("Parameter", "Value"),
              options = list(dom = 'tp',
                             autoWidth = FALSE,
                             scrollX = TRUE,
                             pageLength = nrow(filtered_data))  # Set page length to number of rows
    )
  })


  # Code to filter and populate the metadata and datafile dropdown list using the DOI and access_data()
  full_paths <- reactiveVal(list()) # Initialize a reactive value to store the files full file path

  # This observe should trigger once a study is selected
  observe({
    # Ensure a selection has been made
    req(input$study_select)

    # Dynamically choose between all_data() and detailed_() to display study details
    data <- if (is.null(input$select_dataverse)) all_data() else detailed_data()

    # Fetch DOI, file lists, and update inputs
    selected_title <- input$study_select
    selected_doi <- data %>%
      filter(Title == selected_title) %>%
      pull(DOI) %>%
      unique()

    # Ensure that selected_doi is a single value
    if (length(selected_doi) != 1) {
      # Simply reset input fields and return early
      updateSelectInput(session, "metadata_select", choices = NULL, selected = NULL)
      updateSelectInput(session, "dataset_select", choices = NULL, selected = NULL)
      full_paths(list())  # Ensure any previous paths are cleared
      return()  # Exit the observer without further action
    }

    # Fetch the file list
    file_list <- access_data(selected_doi)

    # Update full_paths right after file_list is validated and stored
    full_paths(setNames(file_list, basename(file_list)))

    metadata <-filter_filelist(file_list, is_txt = TRUE)
    datafile <-filter_filelist(file_list, is_txt = FALSE)

    #  Update metadata_select
    updateSelectInput(session, "metadata_select", choices = basename(metadata), selected = NULL)

    # Process update dataset_select  based on the active tab dynamically
    if (input$datafiles == "view_alldata") {
      updateSelectInput(session, "dataset_select", choices = basename(file_list), selected = NULL)
    }
    else {
      updateSelectInput(session, "dataset_select", choices = basename(datafile), selected = NULL)
    }

  })

  # ----- Metadata tabPanel -----

  # Reactive expression for loading and handling metadata
  loaded_metadata <- reactive({
    req(input$metadata_select)  # Ensure a file is selected

    # Use the full path for reading the file
    selected_file_path <- full_paths()[input$metadata_select]

    req(file.exists(selected_file_path))  # Ensure the file exists

    # Read the file and suppress warning
    raw_lines <- suppressWarnings(readLines(selected_file_path))

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


  # UI output for metadata
  output$meta <- renderUI({
    # Attempt to fetch the metadata
    req(loaded_metadata())

    meta_data <- loaded_metadata()

    if (is.null(meta_data)) {

      # Return a UI element displaying the error message
      tagList(
        h3("The schema file is either not there or restricted.")
      )

    } else {

      # Process and display the DataTable if metadata is available
      separate_by_tab <- grep("^\\t", meta_data$V1)
      if (length(separate_by_tab) > 0) {
        first_tab <- separate_by_tab[1]
        datatable_data <- meta_data[1:(first_tab - 1), , drop = FALSE]
      } else {
        datatable_data <- meta_data
      }

      DT::datatable(datatable_data,
                    rownames = FALSE,
                    colnames = c("Key", "Value"),
                    options = list(dom = "tp", autoWidth = FALSE, scrollX = TRUE))
    }
  })

  # UI output for schema
  output$schema <- renderUI({
    # Attempt to fetch the metadata
    req(loaded_metadata())

    # Fetch the metadata again
    meta_data <- loaded_metadata()

    # Check if the metadata is NULL
    if (is.null(meta_data)) {
      # Output a message if schema is NULL or not available
      tagList(
        h3("The schema file is either not there or restricted.")
      )
    } else {
      # Render DataTable if schema is available
      separate_by_tab <- grep("^\\t", meta_data$V1)

      if (length(separate_by_tab) > 0) {
        first_tab <- separate_by_tab[1]  # Use only the first tab if there are multiple
        datatable_data <- meta_data[first_tab:nrow(meta_data), , drop = FALSE]
      } else {
        datatable_data <- meta_data
      }

      DT::datatable(datatable_data,
                    rownames = FALSE,
                    colnames = c("Key", "Value"),
                    options = list(dom = "tp",
                                   autoWidth = FALSE,
                                   scrollX = TRUE))
    }
  })

  # output for all file types as a data table
  output$view_alldata <- renderDataTable({
    # Ensure a file is selected
    req(input$dataset_select)
    selected_file <- full_paths()[input$dataset_select]

    # Determine the file extension and read accordingly
    file_ext <- tolower(tools::file_ext(selected_file))
    tryCatch({
      data <- switch(file_ext,
                     'txt' = {
                       lines <- readLines(selected_file)
                       data.frame(Text = lines, stringsAsFactors = FALSE)
                     },
                     'csv' = {
                       read.csv(selected_file, stringsAsFactors = FALSE)
                     },
                     'xlsx' = {
                       read_excel(selected_file)
                     },
                     'pdf' = {
                       text <- pdf_text(selected_file)
                       data.frame(Text = unlist(text), stringsAsFactors = FALSE)
                     },
                     'sas' = {
                       read.sas7bdat(selected_file)
                     },
                     'tab' = {
                       read.delim(selected_file, header = TRUE, sep = "\t")
                     },
                     'xml' = {
                       xml_data <- xmlParse(selected_file)
                       xml_df <- xmlToDataFrame(xml_data)
                       xml_df
                     },
                     data.frame(Message = paste("Unsupported file type:", file_ext))  # Default case
      )
      datatable(data,
                options = list(dom = 'tp', autoWidth = FALSE, scrollX = TRUE),
                rownames = FALSE)  # Set rownames to FALSE
    }, error = function(e) {
      datatable(data.frame(Error = "Error reading file", Message = e$message),
                options = list(dom = 'tp', autoWidth = FALSE, scrollX = TRUE),
                rownames = FALSE)
    })
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
                   height = "600px")
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
