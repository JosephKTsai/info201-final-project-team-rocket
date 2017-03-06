library(shiny)
library(dplyr)
library(ggplot2)

state.data <- read.csv("data/Outpatient_Imaging_Efficiency_-_State.csv")
hospital.data <- read.csv("data/Outpatient_Imaging_Efficiency_-_Hospital.csv")
radiologist.data <- read.csv("data/Physician_Compare_National_Downloadable_File.csv", stringsAsFactors = FALSE)
measures <- unique(state.data$Measure.Name)

server <- function(input, output) {
  filtered <- reactive({
    data.state <- state.data %>% 
      filter(Measure.Name == input$measure) %>%
      select(State, Measure.Name, Score) %>%
      group_by(State) 
    
    non.state.abbreviations <- c("DC", "GU", "PR")
    num.radiologists.by.state <-  radiologist.data %>%
      
    # Filtering out state abbreviations that are not the 50 states shown in the plot
    filter(!(State %in% non.state.abbreviations)) %>%
    group_by(State) %>%
    summarise(n = n())
    
    data <- full_join(data.state, num.radiologists.by.state)
    
    return (data)
  })
  
    
  
  output$plot <- renderPlot({
    ggplot(data = filtered()) +
      geom_point(mapping = aes(x = State, y = Score, size = n, color = n)) +
      scale_color_gradient(low = "blue") +
      labs(title = "Score of Specified Imaging Procedure in Each State", color = "# of Radiologists", size = "# of Radiologists")
  }, height = 700, width = 1500)
  
  
}

shinyServer(server)