#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
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



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("transitMaps"),
  sidebarLayout(
    sidebarPanel (
                    helpText("Select your city"),
                    selectInput("city",label = "select your city", choices = c("Houston", "Ft. Collins")),

                    checkboxGroupInput("days",
                                       label="Days of week",
                                       choiceNames = list(
                                         "Monday", "Tuesday", "Wednesday", "Thursday",
                                         "Friday", "Saturday", "Sunday"),
                                       choiceValues = list(1,2,3,4,5,6,7),
                                
                      selected = c(1, 2, 3, 4, 5)),
                    numericInput("todStart", value = 6, "Start time", min = 0, max = 24, step = 1),
                    numericInput("todEnd", value = 22, "End time", min = 0, max = 24, step =1),
                    checkboxInput("highlightToggle", label = "Select route", value = FALSE),
                    numericInput("minHeadway", label = "minimum headway", value = 0, min = 0),
                    numericInput("maxHeadway", label = "maximum headway", value = 15, min = 0),
                    actionButton("createMapButton", "Create Map!"),
                    actionButton('updateMapButton', "Update Map")
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
  

  # hardcode to select first result for Houston data
  houston_gtfs <- readRDS("/data/houston_gtfs_6_19.rds")
  output$parameterInfo <- renderText({ 
    paste(
      "You're viewing average route frequencies for ",
      input$city, ", from ", timeString(input$todStart), " to ", timeString(input$todEnd), ".",
      sep = ""
      ) })
  output$summaryPlot <- renderLeaflet({
    input$createMapButton
    
    houston_freq <- isolate({
      get_route_frequency(houston_gtfs, 
                          start_hour =  input$todStart, 
                          end_hour = input$todEnd,
                          dow = selectedDOW(input$days),
                          service_ids = c(2)
                          )})
    binPal <- colorBin("viridis", 
                       domain = houston_freq$.$routes_frequency$mean_headways,
                       bins = c(8, 16, 24, 32, 60, 120, 360))

    results <- dplyr::filter(houston_freq$.$routes_frequency,
                             mean_headways <= 15)
    freq_network <- dplyr::filter(houston_gtfs$routes, route_id %in% results$route_id)
    tbl <- dplyr::left_join(houston_freq$.$routes_frequency, houston_freq$routes)%>%
      dplyr::select(route_short_name, route_long_name, median_headways, mean_headways, st_dev_headways )
    
    m <- leaflet()

    # frequent_routes<- dplyr::filter(houston_freq[["."]][["routes_sf"]], route_id %in% results$route_id)
    legendLabels<- labelFormat(suffix = " min") 
    addTiles(m) %>%
      addPolylines(data=houston_freq[["."]][["routes_sf"]],
                   color = ~binPal(houston_freq$.$routes_frequency$mean_headways)) %>% 
      addLegend("bottomright",
                pal= binPal, 
                values = houston_freq$.$routes_frequency$mean_headways,
                title = "Mean Frequency",
                opacity= 1, labFormat = legendLabels)

      })
    output$displayedRoutes <- renderDataTable({
      input$createMapButton
      dplyr::left_join(houston_freq$.$routes_frequency, houston_freq$routes)%>%
        dplyr::select(route_short_name, route_long_name, median_headways, mean_headways, st_dev_headways )
      
      })
    
    
    

}

# Run the application 
shinyApp(ui = ui, server = server)

