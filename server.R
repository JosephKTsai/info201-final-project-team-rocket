library(shiny)
library(dplyr)
library(ggplot2)
library(maps)

state.data <- read.csv("data/Outpatient_Imaging_Efficiency_-_State.csv")
hospital.data <- read.csv("data/Outpatient_Imaging_Efficiency_-_Hospital.csv")
measures <- unique(state.data$Measure.Name)
us.map <- map_data("state")

function(input, output) {
  output$map <- renderPlot({
    ggplot(data = us.map) +
      geom_polygon(mapping = aes(x = long, y = lat, group = group))
  })
}