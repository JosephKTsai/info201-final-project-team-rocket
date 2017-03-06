library(shiny)
library(dplyr)
library(ggplot2)

state.data <- read.csv("data/Outpatient_Imaging_Efficiency_-_State.csv")
hospital.data <- read.csv("data/Outpatient_Imaging_Efficiency_-_Hospital.csv")
physician.data <- read.csv("data/Physician_Compare_National_Downloadable_File.csv")
measures <- unique(state.data$Measure.Name)

server <- function(input, output) {
  filtered <- reactive({
    data.state <- state.data %>% 
      filter(Measure.Name == input$measure) %>%
      select(State, Measure.Name, Score) %>%
      group_by(State) 
    
    data <- data.state
    
    return (data)
  })
  
  # state <- data.frame(State = c("AK", "AL", "AZ","AS", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", 
  #                       "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"), number = c(nrow(physician.data[physician.data$State == "AK",]), nrow(physician.data[physician.data$State == "AL",]), 
  #             nrow(physician.data[physician.data$State == "AZ",]), nrow(physician.data[physician.data$State == "AS",]), nrow(physician.data[physician.data$State == "AR",]), 
  #             nrow(physician.data[physician.data$State == "CA",]), nrow(physician.data[physician.data$State == "CO",]), 
  #             nrow(physician.data[physician.data$State == "CT",]), nrow(physician.data[physician.data$State == "DE",]), 
  #             nrow(physician.data[physician.data$State == "FL",]), nrow(physician.data[physician.data$State == "GA",]), 
  #             nrow(physician.data[physician.data$State == "HI",]), nrow(physician.data[physician.data$State == "ID",]), 
  #             nrow(physician.data[physician.data$State == "IL",]), nrow(physician.data[physician.data$State == "IN",]), 
  #             nrow(physician.data[physician.data$State == "IA",]), nrow(physician.data[physician.data$State == "KS",]), 
  #             nrow(physician.data[physician.data$State == "KY",]), nrow(physician.data[physician.data$State == "LA",]), 
  #             nrow(physician.data[physician.data$State == "ME",]), nrow(physician.data[physician.data$State == "MD",]), 
  #             nrow(physician.data[physician.data$State == "MA",]), nrow(physician.data[physician.data$State == "MI",]), 
  #             nrow(physician.data[physician.data$State == "MN",]), nrow(physician.data[physician.data$State == "MS",]), 
  #             nrow(physician.data[physician.data$State == "MO",]), nrow(physician.data[physician.data$State == "MT",]), 
  #             nrow(physician.data[physician.data$State == "NE",]), nrow(physician.data[physician.data$State == "NV",]), 
  #             nrow(physician.data[physician.data$State == "NH",]), nrow(physician.data[physician.data$State == "NJ",]), 
  #             nrow(physician.data[physician.data$State == "NM",]), nrow(physician.data[physician.data$State == "NY",]), 
  #             nrow(physician.data[physician.data$State == "NC",]), nrow(physician.data[physician.data$State == "ND",]), 
  #             nrow(physician.data[physician.data$State == "OH",]), nrow(physician.data[physician.data$State == "OK",]), 
  #             nrow(physician.data[physician.data$State == "OR",]), nrow(physician.data[physician.data$State == "PA",]), 
  #             nrow(physician.data[physician.data$State == "RI",]), nrow(physician.data[physician.data$State == "SC",]), 
  #             nrow(physician.data[physician.data$State == "SD",]), nrow(physician.data[physician.data$State == "TN",]), 
  #             nrow(physician.data[physician.data$State == "TX",]), nrow(physician.data[physician.data$State == "UT",]), 
  #             nrow(physician.data[physician.data$State == "VT",]), nrow(physician.data[physician.data$State == "VA",]), 
  #             nrow(physician.data[physician.data$State == "WA",]), nrow(physician.data[physician.data$State == "WV",]), 
  #             nrow(physician.data[physician.data$State == "WI",]), nrow(physician.data[physician.data$State == "WY",])))
    
  
  output$plot <- renderPlot({
    ggplot() +
      geom_point(data = filtered(), mapping = aes(x = State, y = Score)) +
      geom_smooth()
  }, height = 800, width = 1000)
  
  
}

shinyServer(server)