#==============================================================================
# This script contains the server code for the USGS River Data Web App
#
# Tyler Bradley
# 2018-01-14
#==============================================================================


shinyServer(
  function(input, output, session){
    # code to end shiny session if browser is closed
    session$onSessionEnded(stopApp)
    
    output$site_map <- renderLeaflet({
      leaflet() %>%
        setView(lat = 37.0902,
                lng = -95.7129,
                zoom = 4) %>%
        addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
        addCircleMarkers(
          data = usgs_site_info,
          ~dec_long_va,
          ~dec_lat_va,
          radius = 8,
          layerId = ~as.character(site_no),
          label = ~hover_text,
          clusterOptions = markerClusterOptions(),
          labelOptions = labelOptions(
            # offset = c(20,-100),
            direction='auto',
            textOnly = T,
            style=list(
              'background'='rgba(255,255,255,0.95)',
              'border-color' = 'rgba(0,0,0,1)',
              'border-radius' = '4px',
              'border-style' = 'solid',
              'border-width' = '4px'))
        )
    })

    map_click <- reactive({
      input$site_map_marker_click$id
    })

    observeEvent(input$site_map_marker_click, {
      updateTabsetPanel(session, "navPage", selected = "siteGraphs")
    })
    
    # map_click <- callModule(site_map, "site-map", sess = session)
    
    observe({
      updateSelectizeInput(
        session = session,
        inputId = "site",
        select = map_click()
      )
    })
    
    # site_parameters <- reactive({
    #   site_dv_info <- whatNWISdata(sites = "01463500", service = "dv")
    #   
    #   output <- unique(site_dv_info$parm_cd)
    #   
    #   return(output)
    # })
    
    # query data from USGS REST API
    data_raw <- eventReactive(input$submit, {
      output <- readNWISdata(siteNumbers = input$site, 
                           # parameterCd = site_parameters(),
                           startDate = input$date[[1]],
                           endDate = input$date[[2]])
      
      if (nrow(output) == 0) output <- NULL
      
      return(output)
      
    })
    
    observeEvent(input$submit, {
      if (is.null(data_raw())) {
        shinyalert(
          "Oops!", 
          "There is no daily data available for time range and site selected!",
          type = "error",
          html = TRUE
        )
      }
    })
    
    data_cleaned <- eventReactive(input$submit, {
      req(data_raw())
      
      
      # cleaning raw data
      output <- data_raw() %>% 
        select(-contains("_cd")) %>% 
        gather(key = parameter, value = result, contains("00003")) %>% 
        mutate(parameter = str_extract(parameter, "_[0-9]{5}_") %>% str_replace_all("_", "")) %>% 
        filter(!is.na(result), 
               result != "-999999") %>% 
        mutate(result = as.numeric(result)) %>% 
        left_join(parameter_df, by = c("parameter" = "parm_cd"))
      
      return(output)
    })
    
    
    # count the number of parameters for a given site
    parameter_count <- eventReactive(input$submit, {
      req(data_cleaned())
      nrow(data_cleaned() %>% distinct(parameter, site_no))
    })



    # getting parameter metadata
    parameter_info <- eventReactive(input$submit, {
      req(data_raw())
      output <- attr(data_raw(), "variableInfo") %>%
        select(parameter = variableCode,
               parameter_desc = variableDescription,
               unit)

      return(output)
    })

    # joining site and parameter metadata to raw data
    data <- eventReactive(input$submit, {
      req(data_cleaned(), parameter_info())
      output <- data_cleaned() %>%
        left_join(parameter_info(), by = "parameter") %>%
        rename(parameter_code = parameter) %>%
        left_join(usgs_site_info %>% select(site_no, station_nm), by = "site_no") %>% 
        as_tibble()

      return(output)
    })
    
    

    my_plots <- function(data){
      data %>%
        plot_ly(
          x = ~dateTime,
          y = ~result,
          type = "scatter",
          mode = "lines+markers",
          marker = list(
            size = 4
          ),
          hoverinfo = "text",
          text = ~paste(
            "Site:", station_nm,
            "<br>Parameter:", parameter_desc,
            "<br>Date Time:", format(dateTime, "%m/%d/%Y %H:%M"),
            "<br>Result:", result
          )
        ) %>%
        layout(
          title = paste(
            unique(data$station_nm), "<br>", unique(data$parameter_desc)
          ),
          titlefont = list(
            size = 10
          ),
          xaxis = list(
            title = ""
          ),
          yaxis = list(
            title = ""
          ),
          margin = list(
            t = 40
          )
        )
    }

    # create the different plotly graphs
    graphs <- eventReactive(input$submit, {
      req(data())
      data() %>%
        group_by(site_no, parm_nm, parameter_code, unit) %>%
        nest() %>%
        mutate(
          graphs = map(data, ~my_plots(.x))
        ) %>%
        arrange(site_no, parameter_code) %>% 
        pull(graphs)
    })

    observeEvent(input$submit, {
      req(graphs())
      map(seq_along(graphs()),
          .f = function(x){
            output_name <- paste0("plot_", x)
            output[[output_name]] <- renderPlotly(graphs()[[x]])
          })


    })
    


    output$graphs_ui <- renderUI({
      req(graphs())
      if (!is.null(data_raw())){

      plots_list <-  map(seq_along(graphs()),
            .f = function(x){
              plotlyOutput(
                outputId = paste0("plot_", x)
              )
            })
      
      tagList(plots_list)
      }
    })
    
    
    observeEvent(input$submit, {
      req(graphs())
      
      map(seq_along(graphs()), 
          function(x){
            addClass(paste0("plot_", x), "dyanmic_plot")
          })
    })
    
  }
)