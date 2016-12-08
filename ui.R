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

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Natural Disaster Information"),
  br(),
  

  sidebarLayout(sidebarPanel(
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
    plotOutput("heatmap"),
    br(),
    h1("State Information"),
    uiOutput("States"),
    plotlyOutput("bargraph"),
    br(),
    br()
  )
  
  ))
)
