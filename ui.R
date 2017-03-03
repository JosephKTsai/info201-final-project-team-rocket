library(shiny)
library(dplyr)
library(ggplot2)
library(maps)

state.data <- read.csv("data/Outpatient_Imaging_Efficiency_-_State.csv")
hospital.data <- read.csv("data/Outpatient_Imaging_Efficiency_-_Hospital.csv")
measures <- unique(state.data$Measure.Name)

ui <- fluidPage(
  titlePanel("Outpatient Efficiency for Selected Measures"),
  sidebarLayout(
    sidebarPanel(
      selectInput("measure", "Choose a measure:", 
                  choices = measures),
      width = 5
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
        tabPanel("Results"),
        tabPanel("Map", plotOutput("map")),
        tabPanel("Plot")
      )
    )
  )
)