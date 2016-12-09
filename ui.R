#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(ggplot2)
library(maps)
library(mapproj)
library(leaflet)

shinyUI(navbarPage("Natural Disaster Information",
  
  # Application title
  #titlePanel("Natural Disaster Information"),
  #br(),
  tabPanel(
    "United States",
    sidebarLayout(
      sidebarPanel(
        dateRangeInput("DateRange", "Show Data From", "1950-01-01"),
        actionButton("Uncheck", label="Clear Filters"),
        br(),
        br(),
        uiOutput("DisasterTypes")
      ),
      mainPanel(
        h1("Quick Info"),
        br(),
        textOutput("summary1"),
        br(),
        textOutput("summary2"),
        br(),
        h1("Number of Disasters by State"),
        br(),
        plotlyOutput("heatmap"),
        br(),
        h1("State Information"),
        br(),
        uiOutput("States"),
        br(),
        plotlyOutput("bargraph"),
        br(),
        br()
      )
  )
  ),
  tabPanel(
    "World",
    sidebarLayout(
      sidebarPanel(
        uiOutput("countrys"),
        uiOutput("countrys2"),
        hr(),
        helpText("Pick two country to compare statistics")
      ),
      mainPanel(
        h1("Disaster Statistics by Country 1900-Present"),
        plotlyOutput("worldbarplot"),
        h1("World Disaster Scatter Plot"),
        br(),
        plotlyOutput("worldscatterplot")
      )
    )
  ),
  tabPanel(
    "Disasters",
    sidebarLayout(
      sidebarPanel(
        sliderInput("bins",
                    "Number of bins:",
                    min = 1,
                    max = 50,
                    value = 30),
        selectInput("indicatorInput", "Disaster Type",choices = levels(dat$disaster.type)[-c(1,7,8)])
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        h1("Histogram"),
        plotOutput("distPlot"),br(),
        h1("Summary"),
        verbatimTextOutput("summary"), width = 7
      )
    )
  )
  )
)
