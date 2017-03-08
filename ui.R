# Loading in the required libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(maps)
library(plotly)

# Reading in the relevant data
state.data <- read.csv("data/Outpatient_Imaging_Efficiency_-_State.csv")
hospital.data <- read.csv("data/Outpatient_Imaging_Efficiency_-_Hospital.csv")

# Obtaining the measure names
measures <- unique(state.data$Measure.Name)

# Obtaining the state names
states <- unique(state.data$State)

# Creating the UI using css bootstrap as the theme and sidebar layout with tabs in the main panel
ui <- fluidPage(
  theme = "bootstrap.css",
  titlePanel("Outpatient Efficiency for Selected Measures"),
  sidebarLayout(
    # The sidebar panel for the user to choose the different measures
    sidebarPanel(
      selectInput("measure", "Choose a measure:", 
                  choices = measures),
      width = 5
    ),
    
    # Creating the main panel with tabs
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Introduction/About", br(), h4(textOutput("intro.description")), br(), h4(p("This data was obtained from data.medicare.gov")) ) ,
                  tabPanel("Results",
                           selectInput('state', label = "Select state", choices = states),
                           h3(textOutput('results.intro')), br(),
                           dataTableOutput('best.hospitals')),
                  tabPanel("Map",
                           br(),
                           h3(textOutput("map.description")),
                           br(),
                           plotlyOutput("map"), 
                           dataTableOutput("click")),
                  tabPanel("Inquiry/Plot", h3(textOutput("plot.description")),p(), plotOutput('plot'), click = 'plot.click')
                  
      )
    )
  )
)

shinyUI(ui)