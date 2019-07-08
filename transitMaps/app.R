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

# # map display functions
# 
# displayEverything <- function(map) {
#   addPolylines(data=    houston_freq[["."]][["routes_sf"]],
#                color = ~binPal(houston_freq$.$routes_frequency$mean_headways),
#                label = ~houston_freq[["."]][["routes_sf"]][["route_id"]],
#                group = "mean_headway") %>% 
#     addLegend("bottomright",
#               pal= binPal, 
#               values = houston_freq$.$routes_frequency$mean_headways,
#               title = "Mean Frequency",
#               opacity= 1, labFormat = legendLabels)
#   
# }
# 
# displaySelected <- function(map) {
#   # frequent_routes<- dplyr::filter(houston_freq[["."]][["routes_sf"]], route_id %in% results$route_id)
#   
#   addPolylines(data=    frequent_routes,
#                #color = ~binPal(houston_freq$.$routes_frequency$mean_headways),
#                label = ~houston_freq[["."]][["routes_sf"]][["route_id"]],
#                group = "mean_headway") %>% 
#     addLegend("bottomright",
#              # pal= binPal, 
#               values = houston_freq$.$routes_frequency$mean_headways,
#               title = "Mean Frequency",
#               opacity= 1, labFormat = legendLabels)
#   
# }
# 
# displaySelectedPlus{
#   # frequent_routes<- dplyr::filter(houston_freq[["."]][["routes_sf"]], route_id %in% results$route_id)
#   
#   addPolylines(data=    frequent_routes,
#                #color = ~binPal(houston_freq$.$routes_frequency$mean_headways),
#                label = ~houston_freq[["."]][["routes_sf"]][["route_id"]],
#                group = "mean_headway") %>% 
#   addPolylines(
#     data = dplyr::filter(houston_freq[["."]][["routes_sf"]], route_id %in% results$route_id)
#     
#   )
#     addLegend("bottomright",
#               # pal= binPal, 
#               values = houston_freq$.$routes_frequency$mean_headways,
#               title = "Mean Frequency",
#               opacity= 1, labFormat = legendLabels)
#   
# }





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
          selectInput("city",label = "Select city / agency", choices = c("Houston - METRO", "Ft. Collins")),
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
          helpText("Display only routes that meet the selected criteria."),
          checkboxInput("filterToggle", label = "Filters on"),
          numericInput("minHeadway", label = "Minimum frequency", value = 0, min = 0),
          numericInput("maxHeadway", label = "Maximum Frequency", value = 15, min = 0)
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
  houston_gtfs <- readRDS("houston_gtfs")
  
  houston_freq <- get_route_frequency(houston_gtfs, 
                        service_ids = c(2))
  filter_results <- NULL
  
  
  # Render map
  output$summaryPlot <- renderLeaflet({
    
    # Dependency: run on click for createMapButton
    input$createMapButton
    
    # Use current analysis parameters
    houston_freq <- isolate({
      get_route_frequency(houston_gtfs, 
                          start_hour =  input$todStart, 
                          end_hour = input$todEnd,
                          dow = selectedDOW(input$days),
                          service_ids = c(2)
                          )})
    # Create map color pallete
    binPal <- colorBin("viridis", 
                       domain = houston_freq$.$routes_frequency$mean_headways,
                       bins = c(8, 16, 24, 32, 60, 120, 360))
  
    # Filter data on current criteria, display results on the map.
    # TODO: separate this from the RUN ANALYSIS  task.  should be reactive.  
    filter_paths <-eventReactive(input$minHeadway, {
      filter_results <- dplyr::filter(houston_freq$.$routes_frequency,
                                      mean_headways <= input$maxHeadway)
      
     dplyr::filter(houston_freq[["."]][["routes_sf"]], route_id %in% filter_results$route_id )
      
    })
    # filter_results <- dplyr::filter(houston_freq$.$routes_frequency,
    #                          mean_headways <= input$maxHeadway)
    # 
    # filter_paths <- dplyr::filter(houston_freq[["."]][["routes_sf"]], route_id %in% filter_results$route_id )
    # 

    # Generate table of route averages for display
    # TODO: Separate this from the Map output function.
    # - Uses same data, but probs shouldn't be in the same block
  
      tbl <- dplyr::left_join(houston_freq$.$routes_frequency, houston_freq$routes)%>%
      dplyr::select(route_short_name, route_long_name, median_headways, mean_headways, st_dev_headways )
    
    # Create Map.
    # TODO: PROBS don't need to do this on every run. Move out of block.
  
      m <- leaflet()
    # houston_freq[["."]][["routes_sf"]]
    # frequent_routes<- dplyr::filter(houston_freq[["."]][["routes_sf"]], route_id %in% results$route_id)
    legendLabels<- labelFormat(suffix = " min") 
    addTiles(m) %>%
      # Display Data
      addPolylines(data=    houston_freq[["."]][["routes_sf"]],
                   color = ~binPal(houston_freq$.$routes_frequency$mean_headways),
                   label = ~houston_freq[["."]][["routes_sf"]][["route_id"]],
                   group = "mean_headway") %>% 
      addPolylines(data = filter_paths(), group = "filter_results") %>%
      addLegend("bottomright",
                pal= binPal, 
                values = houston_freq$.$routes_frequency$mean_headways,
                title = "Mean Frequency",
                opacity= 1, labFormat = legendLabels) %>%
      addLayersControl(overlayGroups = c("mean_headway", "filter_results"),options = layersControlOptions(collapsed = FALSE))

      })
  
  # Display data table
    output$displayedRoutes <- renderDataTable({
      input$createMapButton
      dplyr::left_join(houston_freq$.$routes_frequency, houston_freq$routes)%>%
        dplyr::select(route_short_name, route_long_name, median_headways, mean_headways, st_dev_headways )%>%
        dplyr::rename(`Route ID` = route_short_name, `Route Name` = route_long_name, `Median Frequency` = median_headways, `Average Frequency` = mean_headways, `Std. Dev` = st_dev_headways)
      }, options = list(pageLength = 7))
 
}

# Run the application 
shinyApp(ui = ui, server = server)

