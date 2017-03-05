library(shiny)
library(dplyr)
library(ggplot2)

state.data <- read.csv("data/Outpatient_Imaging_Efficiency_-_State.csv")
hospital.data <- read.csv("data/Outpatient_Imaging_Efficiency_-_Hospital.csv")
physician.data <- read.csv("data/Physician_Compare_National_Downloadable_File.csv")
measures <- unique(state.data$Measure.Name)

server <- function(input, output) {
  filtered <- reactive({
    data.hospital <- hospital.data %>% 
      filter(Measure.Name == input$measure) %>%
      select(State, Measure.Name, Score)
    
    data <- full_join(data.hospital, physician.data) %>%
      group_by(state)
    
    return (data)
  })
  
  output$Plot <- renderPlot({
    plot <- ggplot(data = filtered()) +
      geom_point(mapping = aes(x = nrow(physician.data), y = Score)) +
      geom_smooth(mapping = aes(x = nrow(physician.data), y = Score))
    return(plot)
  })
  
  
}

shinyServer(server)