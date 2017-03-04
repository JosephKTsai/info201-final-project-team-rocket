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
radiologist.data <- read.csv("data/Physician_Compare_National_Downloadable_File.csv", stringsAsFactors = FALSE)

interactive.graph.data <- reactiveValues()
interactive.graph.data$top5 <- ""

function(input, output) {
  
  # Creating the USA map by state
  output$map <- renderPlotly({
    # Finding the number of radiologists by state
    num.radiologists.by.state <- radiologist.data %>%
                                 group_by(State) %>%
                                 summarise(n = n()) %>%
                                 select(n)

    # Creating the output for when somebody hovers over a state
    num.radiologists.by.state$hover <- with(num.radiologists.by.state, 
                                            paste("Number of radiologists:", n))
    # Specifying the map scope
    map.specifications <- list(
      scope = 'usa',
      projection = list(type = "albers usa")
    )
    
    radiologists.and.states <- plot_geo(num.radiologists.by.state, locationmode = "USA-states") %>%
      add_trace(z = ~n, text = ~hover, color = ~n, colors = "Purples"
      ) %>%
      
      # Choosing the layout based off of our map specifications 
      layout(title = "Number of radiologists by state",
             geo = map.specifications)
  })


}