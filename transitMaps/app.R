#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:srDE
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidytransit)
library(leaflet)
library(dplyr)

gtfs_config <- readRDS("data/gtfs_config")


# DOW selection vector.

selectedDOW <- function(selection) {
  dow <- double(7)
  for (i in seq(1, 7)) {
    if (i %in% selection) {
      dow[i] <- 1
    }
  }
  return(dow)
}
freqToolPanel <- tabPanel(
  "Tool",

    h1(
      "How often does the bus run?"
    ),
    p(
      "The frequency of bus service determines how long riders have to wait to travel to their destination,
     and impacts the overall length of their trip. It is one of the most important factors in determining
     whether public transportation is a good option for you."
    ),
    tags$i(h3(" See how frequent public transportation is in your city.")),
  
    p(
      "Use the tool below to see how frequent service is, on average, across your city. 
      You can customize your map by day of the week, or time of day, and use the filter 
      to narrow down your results."),  
  tags$b("Hit Create Map to get started!
    "),
  tags$hr(),
  
  sidebarLayout(
    sidebarPanel (
      wellPanel(
        h4("Data"),
        selectInput(
          "city",
          label = "Select city / agency",
          choices = c(
            "Houston - METRO" = "houston_metro",
            "Ft. Collins - Transfort" = "ftCollins_transfort"
          )
        ),
        h4("Analysis Parameters"),
        helpText(
          "Calculate average route frequency for the selected time frame, (i.e. show average frequencies for all bus routes Mon-Fri, from 6am-10pm)"
        ),
        checkboxGroupInput(
          "days",
          label = "Days of week",
          choiceNames = list(
            "Monday",
            "Tuesday",
            "Wednesday",
            "Thursday",
            "Friday",
            "Saturday",
            "Sunday"
          ),
          choiceValues = list(1, 2, 3, 4, 5, 6, 7),
          selected = c(1, 2, 3, 4, 5)
        ),
        numericInput(
          "todStart",
          value = 6,
          "Start time",
          min = 0,
          max = 24,
          step = 1
        ),
        numericInput(
          "todEnd",
          value = 22,
          "End time",
          min = 0,
          max = 24,
          step = 1
        ),
        actionButton("createMapButton", "Create Map!")
      ),
      
      wellPanel(
        h4("Filter"),
        helpText("Highlight routes that meet the selected criteria."),
        numericInput(
          "minHeadway",
          label = "Minimum frequency",
          value = 0,
          min = 0
        ),
        numericInput(
          "maxHeadway",
          label = "Maximum Frequency",
          value = 16,
          min = 0
        ),
        actionButton("updateFilterButton", "Update Filter")
      ),
      downloadLink("downloadData", "Download table")
    ),
    
    #
    
    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("summaryPlot"),
      dataTableOutput("displayedRoutes")
    )
  )
)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinythemes::shinytheme("united"),
                navbarPage("transitMaps",
                           freqToolPanel,
                           tabPanel("Demo", h4("lakdjf;alkdfj;dk")),
                           tabPanel("About", h4("other"))))

server <- function(input, output) {
  gtfs_data <- eventReactive(input$createMapButton, ignoreNULL = TRUE,
                             switch (
                               input$city,
                               "houston_metro" = readRDS("data/houston_gtfs"),
                               "ftCollins_transfort" = readRDS("data/ftcollins_gtfs")
                             ))
  
  freq_data <-
    eventReactive(input$createMapButton, ignoreNULL = TRUE,
                  {
                    gtfs_data_df <- gtfs_data()
                    freq <- get_route_frequency(
                      gtfs_data_df,
                      start_hour =  input$todStart,
                      end_hour = input$todEnd,
                      dow = selectedDOW(input$days),
                      service_ids = gtfs_config[which(gtfs_config$agency ==
                                                        input$city),]$service_ids
                    )
                    freq$.$routes_sf <- left_join(freq$.$routes_sf,
                                                  freq$routes)
                    freq
                  })
  
  
  # Render map
  output$summaryPlot <- renderLeaflet({
    # Dependency: run on click for createMapButton
    
    input$createMapButton
    
    route_frequencies <- freq_data()
    
    
    if (plyr::empty(route_frequencies$.$routes_frequency)) {
      showModal(modalDialog(
        title = "",
        paste(
          "Uh oh! It looks like something went wrong.",
          "There may not be enough data available across the entire time period you've selected.",
          "Try a different set of parameters (i.e. break up your time interval, or select a different combination of days).",
          "Click the Create Map button when you're ready to start.",
          sep = "\n"
        ),
        easyClose = TRUE,
        footer = NULL
      ))
      return(addTiles(leaflet()))
    }
    
    # Map formatting: color palette and legend
    binPal <- colorBin(
      "magma",
      domain = route_frequencies$.$routes_frequency$mean_headways,
      bins = c(8, 16, 24, 32, 60, 120, 360)
    )
    legendLabels <- labelFormat(suffix = " min")
    
    
    # Filter data on current criteria, display results on the map.
    filter_paths <-
      eventReactive(input$updateFilterButton, ignoreNULL = FALSE,
                    {
                      filter_results <- filter(
                        route_frequencies$.$routes_frequency,
                        between(
                          route_frequencies$.$routes_frequency$mean_headways,
                          input$minHeadway,
                          input$maxHeadway
                        )
                      )
                      
                      filter(route_frequencies[["."]][["routes_sf"]],
                             route_id %in% filter_results$route_id)
                    })
    
    # Create Map.
    
    
    m <- leaflet()
    addTiles(m) %>%
      addPolylines(
        data =    route_frequencies[["."]][["routes_sf"]],
        color = ~ binPal(route_frequencies$.$routes_frequency$mean_headways),
        label = ~ route_frequencies[["."]][["routes_sf"]][["route_long_name"]],
        group = "mean_headway"
      ) %>%
      addPolylines(
        data = filter_paths(),
        color = "blue",
        group = "filter_results",
        label = filter_paths()$route_long_name
      ) %>%
      addLegend(
        "bottomright",
        pal = binPal,
        values = route_frequencies$.$routes_frequency$mean_headways,
        title = "Average Frequency",
        opacity = 1,
        labFormat = legendLabels
      ) %>%
      addLayersControl(
        overlayGroups = c("mean_headway", "filter_results"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
  })
  
  
  # Display data table
  output$displayedRoutes <- renderDataTable({
    input$createMapButton
    try({
      route_frequencies <- freq_data()
      # Generate table of route averages for display
      
      dplyr::left_join(route_frequencies$.$routes_frequency,
                       route_frequencies$routes) %>%
        dplyr::select(
          route_short_name,
          route_long_name,
          median_headways,
          mean_headways,
          st_dev_headways
        ) %>%
        dplyr::rename(
          `Route ID` = route_short_name,
          `Route Name` = route_long_name,
          `Median Frequency` = median_headways,
          `Average Frequency` = mean_headways,
          `Std. Dev` = st_dev_headways
        )
    })
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", input$city, ".csv", sep = "")
    },
    content = function(file) {
      route_frequencies <- freq_data()
      data_tbl <-
        dplyr::left_join(route_frequencies$.$routes_frequency,
                         route_frequencies$routes) %>%
        dplyr::select(
          route_short_name,
          route_long_name,
          median_headways,
          mean_headways,
          st_dev_headways
        )
      write.csv(data_tbl, file)
    }
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)
