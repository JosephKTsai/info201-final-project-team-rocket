# Loading in the appropriate libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(maps)
library(plotly)

# Loading in the relevant data 
state.data <- read.csv("data/Outpatient_Imaging_Efficiency_-_State.csv")
hospital.data <- read.csv("data/Outpatient_Imaging_Efficiency_-_Hospital.csv")
radiologist.data <- read.csv("data/Physician_Compare_National_Downloadable_File.csv", stringsAsFactors = FALSE)
measures <- unique(state.data$Measure.Name)
us.map <- map_data("state")
radiologist.data <- read.csv("data/Physician_Compare_National_Downloadable_File.csv", stringsAsFactors = FALSE)

server <- function(input, output) {
  
  # Finding the best 5 hospitals in the selected state for the selected scanning.
  filtered.data <- reactive({
    state <- input$state
    measure <- input$measure
    best.hospital <- filter(hospital.data, State == state) %>% 
                      filter(Measure.Name == measure) %>% 
                        arrange(desc(Score))
    best.hospital <- best.hospital[!(best.hospital$Score == "Not Available"), ]
    best.hospital <- best.hospital[c(1:5), c("Provider.ID", "Hospital.Name", "City", "Address", "ZIP.Code", "Phone.Number")]
    # Changing the column names to more readable names.
    colnames(best.hospital) <- c("Provider ID", "Hospital Name", "City", "Address", "ZIP Code", "Phone Number")
    return(best.hospital)
  })
  
  # Render the data table for the top 5 hospitals in the selected state
  output$best.hospitals <- renderDataTable({
    return(filtered.data())
  })
  
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
  
  # Abbreviations within the dataset that are not the 50 states that need to be removed
  non.state.abbreviations <- c("DC", "GU", "PR")
  
  # Finding the number of radiologists by state by the different types listed above
  num.radiologists.by.state <-  radiologist.data %>%
                                # Filtering out state abbreviations that are not the 50 states shown in the plot
                                filter(!(State %in% non.state.abbreviations)) %>%
                                group_by(State) %>%
                                summarise(n = n())
  
  # Creating the output for when somebody hovers over a state
  num.radiologists.by.state$hover <- with(num.radiologists.by.state, 
                                          paste0(State))
  
  # Creating the USA map by state
  output$map <- renderPlotly({

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
      
      # Setting the color bar title
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
      corresponding.row.number <- data.from.click$pointNumber + 1
      
      # Getting the state corresponding to the row number 
      corresponding.state <- num.radiologists.by.state[corresponding.row.number, ] %>%
                             select(State)
      
      # Obtaining the corresponding state in vector form
      corresponding.state <- corresponding.state$State
      
      # Returning the top hospitals of the clicked state for the specified scan
      hospitals.of.clicked.state <- hospital.data %>%
                                    filter(State == corresponding.state,
                                           # Filtering for the measure name chosen by the user
                                           Measure.Name == as.name(input$measure),
                                           Score != "Not Available") %>%
                                    # Arranging from highest score to lowest score
                                    arrange(desc(Score)) %>%
                                    select(Provider.ID, 
                                           Hospital.Name, 
                                           Address, City, 
                                           State, 
                                           ZIP.Code,
                                           County.Name,
                                           Phone.Number,
                                           Measure.Name,
                                           Score)
      
      # Getting the top 5 hospitals
      top.5.hospitals.of.clicked.state <- hospitals.of.clicked.state[1:5, ]
      
      # Making "readable" column names
      colnames(top.5.hospitals.of.clicked.state) <- c("Provider ID",
                                                       "Hospital Name",
                                                       "Address",
                                                       "City",
                                                       "State",
                                                       "ZIP Code",
                                                       "County Name",
                                                       "Phone Number",
                                                       "Measure Name",
                                                       "Score")
                                    
      return(top.5.hospitals.of.clicked.state)
    }
  })

  # Output for radiologists plot vs. specified imaging
  output$plot <- renderPlot({
    ggplot(data = filtered()) +
      geom_point(mapping = aes(x = State, y = Score, size = n, color = n)) +
      scale_color_gradient(low = "blue") +
      labs(title = "Score of Specified Imaging Procedure in Each State", color = "# of Radiologists", size = "# of Radiologists")
  }, height = 700, width = 1500)
  
  # Map Decription
  output$map.description <- renderText({
    plot.description <- paste0("Map Description:\n",
                               "The map below shows of the number of radiologists per state. ",
                               "The darker a state is, the more radiologists are present within that state. ",
                               "If a state is hovered over, it will display the exact number of radiologists within the state. ", 
                               "If a state is clicked, a table will appear that shows the top 5 hospitals within that state ", 
                               "for the selected measure."
                               )
  })
}
shinyServer(server)
