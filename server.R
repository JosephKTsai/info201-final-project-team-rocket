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
    
    # Finding the number of radiologists by state by the different types listed above
    num.radiologists.by.state <- radiologist.data %>%
                                 group_by(State) %>%
                                 summarise(n = n())

    # Creating the output for when somebody hovers over a state
    num.radiologists.by.state$hover <- with(num.radiologists.by.state, 
                                            paste0(State))
    # Specifying the map scope
    map.specifications <- list(
      scope = 'usa',
      projection = list(type = "albers usa")
    )
    
    # Setting the render specifications for the map of the states
    render.specifications <- list(
      l = 50,
      r = 50,
      b = 100,
      t = 100,
      pad = 4
    )
    # Creating the map of the states that shows the number of radiologists when someone hovers over a stte
    radiologists.and.states <- plot_geo(num.radiologists.by.state, locationmode = "USA-states") %>%
      add_trace(z = ~n,
                text = ~hover,
                locations = ~State,
                color = ~n, 
                colors = "Blues"
      ) %>%
      colorbar(title = "Number of Radiologists") %>%
      
      # Choosing the layout based off of our map specifications 
      layout(title = "Number of radiologists by state (hover for exact number)",
             geo = map.specifications,
             autosize = F,
             width = 800,
             height = 500, 
             margin = render.specifications)
  })


}