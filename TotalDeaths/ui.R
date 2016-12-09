library(shiny)
dat <- read.csv('natdisdata.csv')
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Disaster Data"),
  
  # Sidebar with a slider input for the number of bins
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
))