
fluidPage(    
  titlePanel("Natural Disasters"),
  sidebarLayout(      
    sidebarPanel(
      selectInput("country", "Country:", choices=world.data[['country.names']]),
      hr(),
      helpText("Pick a country")
    )
  ),
  
  mainPanel(
    plotOutput("plot")  
  )
)