library(shiny)
library(dplyr)
library(ggplot2)

state.data <- read.csv("data/Outpatient_Imaging_Efficiency_-_State.csv")
hospital.data <- read.csv("data/Outpatient_Imaging_Efficiency_-_Hospital.csv")
measures <- unique(state.data$Measure.Name)

server <- function(input, output) {
  filtered.data <- reactive({
    state <- input$state
    measure <- input$measure
    best.hospital <- filter(hospital.data, State == state) %>% 
                     filter(Measure.Name == measure) %>% 
                     arrange(desc(Score))
    best.hospital <- best.hospital[!(best.hospital$Score == "Not Available"), ]
    best.hospital <- best.hospital[c(1:5), c("Hospital.Name", "City", "Address", "ZIP.Code", "Phone.Number")]
    return(best.hospital)
  })
  
  output$best.hospitals <- renderDataTable({
    return(filtered.data())
  })
  
}