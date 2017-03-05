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
    
    # Abbreviations within the dataset that are not the 50 states that need to be removed
    non.state.abbreviations <- c("DC", "GU", "PR")
    
    # Finding the number of radiologists by state by the different types listed above
    num.radiologists.by.state <- radiologist.data %>%
                                 # Filtering out state abbreviations that are not the 50 states shown in the plot
                                 filter(!(State %in% non.state.abbreviations)) %>%
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
  
  # Returns a data table of the hospitals within the given state
  output$click <- renderDataTable({
    data.from.click <- event_data("plotly_click")
    if(is.null(data.from.click)) {
      "Click to get detailed hospital information"
    } else {
      
      # Getting the corresponding row number for the state that they clicked on
      # need to add 1 because pointNumber starts from 0
      corresponding.row.number <- event_data$pointNumber + 1
      
      # Getting the state corresponding to the row number 
      corresponding.state <- num.radiologists.by.state[responding.row.number, ] %>%
                             select(State) %>%
        
      ###### Need to update this to work - it's not recognizing the corresponding.state$State call properly and
        #### not getting the state name
                             corresponding.state$State
      
      # Returning the hospitals of the clicked state (need to change to get top 5 for specified category)
      hospitals.of.clicked.state <- hospital.data %>%
                                    filter_("State" %in% corresponding.state)
      return(hospitals.of.clicked.state)
    }
  })


}