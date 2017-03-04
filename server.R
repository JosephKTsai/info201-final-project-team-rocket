# Loading in the appropriate libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(maps)
library(plotly)

# Loading in the relevant data 
state.data <- read.csv("data/Outpatient_Imaging_Efficiency_-_State.csv")
hospital.data <- read.csv("data/Outpatient_Imaging_Efficiency_-_Hospital.csv")
measures <- unique(state.data$Measure.Name)
us.map <- map_data("state")
interactive.graph.data <- reactiveValues()
interactive.graph.data$top5 <- ""
function(input, output) {
  
  # Creating the USA map by state
  output$map <- renderPlotly({
    states <- us.map %>%
              group_by(region)
    
    # Specifying the map scope
    map.specifications <- list(
      scope = 'usa'
    )
    
    # Creating the plot with plotly so that it will be interactive between the state borders
    plot_ly(z = us.map$region, text = state.name, locations = state.abb,
            type = 'choropleth', locationmode = 'USA-states') %>%
      
      # Choosing the layout based off of our map specifications 
      layout(geo = map.specifications)
  })


}