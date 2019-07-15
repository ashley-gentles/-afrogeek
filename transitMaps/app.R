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

gtfs_config <- readRDS("gtfs_config")

# Create string for time.
timeString <- function(hour, tString) {
  if (hour <= 12) {
    period <- "am"
  } else {
    period <- "pm"
  }
  return(paste(hour %% 12, " ", period))
} 

# DOW selection vector.

selectedDOW <- function(selection) {
  dow <- double(7)
  for(i in seq(1,7)) {
    if (i %in% selection){
      dow[i] <- 1
    }
  }
  return(dow)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage("transitMaps", tabPanel("About")),

    # Application title
  titlePanel("transitMaps"),
  
  p("Welcome to transitMaps.Use this tool to see where frequent public transportation is available in your city."),
  p("How it works:"),
  p("transitMaps calculates the frequency with which vehicles are scheduled to pass through routes and stops.
    The map displays average headway, or the average amount of time between stops, for each route."),

   sidebarLayout(
    sidebarPanel (
      
      wellPanel(
        h4("Data"),
          selectInput("city",label = "Select city / agency", choices = c("Houston - METRO" = "houston_metro", "Ft. Collins - Transfort" = "ftCollins_transfort")),
        h4("Analysis Parameters"),
          helpText("Calculate average route frequency for the selected time frame, (i.e. show average frequencies for all bus routes Mon-Fri, from 6am-10pm)"),
          checkboxGroupInput("days",
                             label="Days of week",
                             choiceNames = list("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday", "Sunday"), 
                             choiceValues = list(1,2,3,4,5,6,7),
                             selected = c(1, 2, 3, 4, 5)),
          numericInput("todStart", value = 6, "Start time", min = 0, max = 24, step = 1),
          numericInput("todEnd", value = 22, "End time", min = 0, max = 24, step =1),
          actionButton("createMapButton", "Create Map!")
        ),
      
      wellPanel(
        h4("Filter"),
          helpText("Highlight routes that meet the selected criteria."),
          numericInput("minHeadway", label = "Minimum frequency", value = 0, min = 0),
          numericInput("maxHeadway", label = "Maximum Frequency", value = 16, min = 0),
          actionButton("updateFilterButton", "Update Filter")
        )
    ),
    
  # 
  
  # Show a plot of the generated distribution
  mainPanel(
  textOutput("parameterInfo"),
    leafletOutput("summaryPlot"),
  dataTableOutput("displayedRoutes")

    
  ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Loads Houston Metro GTFS data by default.
  # houston_gtfs <- readRDS("houston_gtfs")
  gtfs_data <- eventReactive(input$createMapButton, ignoreNULL = TRUE,
                            switch (input$city,
                              "houston_metro" = readRDS("houston_gtfs"),
                              "ftCollins_transfort" = readRDS("ftcollins_gtfs")
                            )
                             )
  
  houston_freq <- eventReactive(input$createMapButton,ignoreNULL = TRUE,
    # try({
    {
      gtfs_data_df <- gtfs_data()
      get_route_frequency(gtfs_data_df, 
                          start_hour =  input$todStart, 
                          end_hour = input$todEnd,
                          dow = selectedDOW(input$days),
                          service_ids = gtfs_config[
                            which(gtfs_config$agency==input$city),]$service_ids
      )}
    # })
  )
    

  # Render map
  output$summaryPlot <- renderLeaflet({
    
    # Dependency: run on click for createMapButton

    input$createMapButton
    cat(file=stderr(),"Begin rendering map for output$SummaryPlot\n")
  
    
    route_frequencies <- houston_freq()
    # cat(file=stderr(),class(route_frequencies$.$routes_sf), nrow(route_frequencies$.$routes_frequency),paste("service_id = ", gtfs_data_df$calendar$service_id[1] ),  "\n")
    # validate(need(nrow(route_frequencies$.$routes_frequency) == 0, "Frequency table is empty"), "Create a new map")
    cat(file=stderr(),"Before validation")
    
     if(plyr::empty(route_frequencies$.$routes_frequency) ){
      showModal(modalDialog(
        title = "",
        "Uh oh! It looks like something went wrong. \n
         There may not be enough data available across the entire time period you've selected.
        Try a different set of parameters (i.e. break up your time interval, or selecct a different combination of days)/",
        easyClose = TRUE,
        footer = NULL
      ))
      return(addTiles(leaflet()))
    } 
    # Create map color pallete
    binPal <- colorBin("viridis", 
                       domain = route_frequencies$.$routes_frequency$mean_headways,
                       bins = c(8, 16, 24, 32, 60, 120, 360))
  
    # Filter data on current criteria, display results on the map.
    # TODO: separate this from the RUN ANALYSIS  task.  should be reactive.  
    filter_paths <- eventReactive(input$updateFilterButton,ignoreNULL = FALSE, 
      { filter_results <- filter(route_frequencies$.$routes_frequency,
                                      between(route_frequencies$.$routes_frequency$mean_headways, input$minHeadway, input$maxHeadway))

        filter(route_frequencies[["."]][["routes_sf"]], route_id %in% filter_results$route_id )
      })


    

    # Create Map.

      m <- leaflet()
      
     legendLabels<- labelFormat(suffix = " min") 
    addTiles(m) %>%
      # Display Data
      addPolylines(data=    route_frequencies[["."]][["routes_sf"]],
                   color = ~binPal(route_frequencies$.$routes_frequency$mean_headways),
                   label = ~route_frequencies[["."]][["routes_sf"]][["route_id"]],
                   group = "mean_headway") %>% 
      addPolylines(data = filter_paths(), group = "filter_results") %>%
      addLegend("bottomright",
                pal= binPal, 
                values = route_frequencies$.$routes_frequency$mean_headways,
                title = "Average Frequency",
                opacity= 1, labFormat = legendLabels) %>%
      addLayersControl(overlayGroups = c("mean_headway", "filter_results"),options = layersControlOptions(collapsed = FALSE))

      })
  
  # Display data table
    output$displayedRoutes <- renderDataTable({
      input$createMapButton
      try({
      route_frequencies <- houston_freq()
      # Generate table of route averages for display
 
      dplyr::left_join(route_frequencies$.$routes_frequency, route_frequencies$routes)%>%
        dplyr::select(route_short_name, route_long_name, median_headways, mean_headways, st_dev_headways )%>%
        dplyr::rename(`Route ID` = route_short_name, `Route Name` = route_long_name, `Median Frequency` = median_headways, `Average Frequency` = mean_headways, `Std. Dev` = st_dev_headways)
      })})
 
}

# Run the application 
shinyApp(ui = ui, server = server)

