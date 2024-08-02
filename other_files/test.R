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
#library(pdftools)
library(sas7bdat)
library(XML)
library(haven)
library(shinyalert)

# install.packages("pdftools")

# Source the functions
source("app_function2.R")

## tell shiny to log all reactivity
library(reactlog)
reactlog_enable()
# shiny::reactlogShow()
reactlogReset()

# Fetch OAC dataverse content
OAC_info <- fetch_oac_info()


# Create a folder to store the cached information
shinyOptions(cache = cachem::cache_disk("./cache_folder/cache/"))

ui <- dashboardPage(
  skin = "light",
  scrollToTop = TRUE,
  fullscreen = TRUE,
  help = NULL,
  dark = NULL,

  dashboardHeader(
    skin = "light",
    status = "lightblue",
    #   navbarMenu(
    #     navbarTab(
    #       tags$b("Home"),
    #       tabName = "home_tab"
    #     ),
    #     navbarTab(
    #       tags$b("About"),
    #       tabName = "about_tab"
    #     ),
    #     navbarTab(
    #       tags$b("Explore Borealis"),
    #       tabName = "explore_borealis_tab",
    #       navbarTab(
    #         dropdownHeader(tags$b("Study Network")),
    #         tabName = "network_tab"
    #       ),
    #       navbarTab(
    #         dropdownHeader(tags$b("Explore Borealis")),
    #         tabName = "borealis_tab"
    #       )
    #     )
    #   )
    # ),

    tags$head(tags$style(
      "
  .nav-pills .nav-link {
    color: black;
    background-color: white;
  }
  .nav-pills .nav-link.active {
    color: white;
    background-color: #3c8dbc;
  }
  .nav-pills .nav-link:hover {
    color: #3c8dbc;
  }
  "
    )),
    navbarMenu(
      id = "navmenu",
      tags$head(tags$style(".nav-pills .nav-link.active {color: #fff; background-color: #3c8dbc;}")),
      tags$head(tags$style(".nav-pills .nav-link:not(.active):hover {color: #3c8dbc !important;}")),
      navbarTab(tabName = "home_tab", text = tags$b("Home")),
      navbarTab(tabName = "about_tab", text = tags$b("About")),
      navbarTab(
        text = tags$b("Explore Borealis"),
        navbarTab(tabName = "network_tab", text = tags$b(tags$span(style = "color: black;", "Study Network"))),
        navbarTab(tabName = "borealis_tab", text = tags$b(tags$span(style = "color: black;", "Explore Borealis")))
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
                  "allows you to view the networks of studies in the OAC historical reproducible
            project repository on borealis based on keywords and author's name. Users can
            then select a particular keyword or author to further explore the associated studies.
            The study overview, metadata and explore data tab allows users to gain a deeper insight
            about the study and data. Click ",
                  tags$a(href = "https://borealisdata.ca/dataverse/oac",
                         tags$b("here"),
                         target = "_blank", style = "color:#3c8dbc;"),
                  " to access the OAC dataverse in the Borealis database."),
                style = "background-color:lightgrey; padding: 10px; width: 50%; text-align: center; margin: 0 auto;"
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
                tags$hr(style = "border: 1px solid #3c8dbc; width: 80px; text-align: left; margin-left: 0;"),
                p(
                  "Have you ever wondered where all the data from past research are?
            Have you considered the immense value these datasets hold,
            especially in the era of big data, machine learning, and AI?
            Integrating such valuable datasets into current models can drive
            innovation and automation, unlocking new possibilities and advancements."
                ),
                p(
                  "This app was developed as part of an initiative to enhance the value
            of research data by adhering to the FAIR data principles.
            These principles ensure that data is Findable, Accessible, Interoperable,
            and Reusable (FAIR). By simplifying the application of these principles,
            the app significantly improves the usability and value of research data,
            benefiting researchers, industry professionals, and stakeholders in the Agri-Food sector."
                ),
                p(
                  "The Agri-Food Data Canada at the University of Guelph,
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
                p(),  # Create a hyperlink,
                style = "padding: 10px; width: 100%; text-align: left; width: 50%"
              ),
              br(),
              tags$div(
                tags$h4(tags$b("Licensing"), style = "text-align: center;"),
                tags$hr(style = "border: 1px solid #3c8dbc; width: 60px; margin: auto;"),
                br(),
                p(),  # Create a hyperlink
                style = "background-color:lightgrey; padding: 10px; width: 60%; text-align: center; margin: 0 auto;"
              )
      ),

      tabItem(tabName = "network_tab",
              tags$div(
                tags$h3(strong("ABOUT")),
                br(),
                tags$h4(tags$b("How it started")),
                tags$hr(style = "border: 1px solid #3c8dbc; width: 80px; text-align: left; margin-left: 0;"),
                p(
                  "Have you ever wondered where all the data from past research are?
            Have you considered the immense value these datasets hold,
            especially in the era of big data, machine learning, and AI?
            Integrating such valuable datasets into current models can drive
            innovation and automation, unlocking new possibilities and advancements."
                ),
                p(
                  "This app was developed as part of an initiative to enhance the value
            of research data by adhering to the FAIR data principles.
            These principles ensure that data is Findable, Accessible, Interoperable,
            and Reusable (FAIR). By simplifying the application of these principles,
            the app significantly improves the usability and value of research data,
            benefiting researchers, industry professionals, and stakeholders in the Agri-Food sector."
                ),
                p(
                  "The Agri-Food Data Canada at the University of Guelph,
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
                p(),  # Create a hyperlink,
                style = "padding: 10px; width: 100%; text-align: left; width: 50%"
              ),
              br(),
              tags$div(
                tags$h4(tags$b("Licensing"), style = "text-align: center;"),
                tags$hr(style = "border: 1px solid #3c8dbc; width: 60px; margin: auto;"),
                br(),
                p(),  # Create a hyperlink
                style = "background-color:lightgrey; padding: 10px; width: 60%; text-align: center; margin: 0 auto;"
              )
      )
    )
  )
)






server <- function(input, output, session) {

}


# Run the application
shinyApp(ui = ui, server = server)
