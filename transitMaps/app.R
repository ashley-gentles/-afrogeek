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
library(tidyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("transitMaps"),
  
  
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("summaryPlot")
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  tidytransit::get_feedlist()
  View(feedlist)
  # hardcode to select first result for Houston data
  houston_feedlist <- dplyr::filter(tidytransit::feedlist, loc_t == 'Houston, TX, USA')
  houston_gtfs_url <- houston_feedlist[1, "url_d"]
  
  houston <- tidytransit::read_gtfs(houston_gtfs_url,geometry=TRUE)
  
  output$summaryPlot <- renderPlot({
    
    plot(houston)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

