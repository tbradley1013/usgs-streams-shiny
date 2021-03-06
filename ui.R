#==============================================================================
# This script will be the UI code for the USGS River Data web application
#
# Tyler Bradley
# 2018-01-14
#==============================================================================


shinyUI(
  tagList(
    useShinyjs(),
    useShinyalert(),
    
    
    includeCSS("www/styles.css"),
    navbarPage(
      id = "navPage",
      title = "USGS Stream Data",
      theme = shinytheme("united"),
      tabPanel(
        id = "site-map-tab",
        class = "tab-panel",
        div(
          "Site Map"
        ),
        div(
          class = "tab-body",
          id = "site-map-tab-body",
          div(
            tags$style(type = "text/css", "#site_map {height: calc(100vh - 80px) !important;}"),
            leafletOutput("site_map")
          )
        )
        
      ),
      tabPanel(
        id = "site-graph-panel",
        class = "tab-panel",
        value = "siteGraphs",
        div(
          "Water Quality Graphs"
        ),
        div(
          class = "tab-body",
          id = "wq-graphs-tab-body",
          fluidRow(
            column(
              4, 
              selectizeInput(
                inputId = "site",
                label = "Select Site:",
                choices = site_list,
                selected = site_list[[1]],
                multiple = TRUE
              )
            ),
            
            column(
              4, 
              dateRangeInput(
                inputId = "date",
                label = "Select Date Range:",
                start = Sys.Date() - lubridate::days(30),
                end = Sys.Date()
              )
            ),
            
            column(
              4,
              actionButton(
                inputId = "submit",
                label = "Apply Changes"
              )
            )
          ),
          
          fluidRow(
            div(
              id = "plot-container",
              
              uiOutput(
                outputId = "graphs_ui"
              )
              
            )
          )
        )
      )
    )
  )
)

