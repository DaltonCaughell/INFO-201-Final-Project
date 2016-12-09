library(shiny)
library(ggplot2)
library(dplyr)
dat <- read.csv('natdisdata.csv')
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  
  output$summary<- renderPrint({
    filtered <- dat %>% 
      filter(disaster.type == input$indicatorInput)
    print(summary(filtered$Total.deaths))
    #ggplot(filtered, aes(value))+geom_histogram()
    
  })
  output$distPlot <- renderPlot({
    # if(input$indicatorInput == "Animal accident"){
    # x    <- dat[dat$disaster.type=='Animal accident',]
    # bins <- seq(min(x), max(x), length.out = input$bins + 20)
    # # draw the histogram with the specified number of bins
    # #hist(x, breaks = bins, col = 'darkblue', border = 'white', xlab = "Total Deaths", main = "Histogram of Total deaths in Animal accident")
    # ggplot(data = x, aes(x))+geom_histogram(binwidth=100, colour="black", fill="white")
    # }
    if(input$indicatorInput == "Drought"){
      x    <- dat[dat$disaster.type=="Drought",]$Total.deaths 
      bins <- seq(min(x, na.rm = T), max(x, na.rm = T), length.out = input$bins + 20)
      # draw the histogram with the specified number of bins
      hist(x, col = 'darkblue', border = 'white', xlab = "Total Deaths", main = "Histogram of Total deaths in Drought") 
      #      ggplot(x,aes(x))+geom_histogram(binwidth=100, colour="black", fill="white")
    }
    
    else if(input$indicatorInput == "Earthquake"){
      x    <- dat[dat$disaster.type=="Earthquake",]$Total.deaths 
      bins <- seq(min(x, na.rm = T), max(x, na.rm = T), length.out = input$bins + 20)
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkblue', border = 'white', xlab = "Total Deaths", main = "Histogram of Total deaths in Earthquake") 
    }
    
    else if(input$indicatorInput == "Epidemic"){
      x    <- dat[dat$disaster.type=="Epidemic",]$Total.deaths 
      bins <- seq(min(x, na.rm = T), max(x, na.rm = T), length.out = input$bins + 20)
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkblue', border = 'white', xlab = "Total Deaths", main = "Histogram of Total deaths in Epidemic") 
    }
    
    else if(input$indicatorInput == "Extreme temperature "){
      x    <- dat[dat$disaster.type=="Extreme temperature ",]$Total.deaths 
      bins <- seq(min(x, na.rm = T), max(x, na.rm = T), length.out = input$bins + 20)
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkblue', border = 'white', xlab = "Total Deaths", main = "Histogram of Total deaths in Extreme temperature ") 
    }
    
    else if(input$indicatorInput == "Flood"){
      x    <- dat[dat$disaster.type=="Flood",]$Total.deaths 
      bins <- seq(min(x, na.rm = T), max(x, na.rm = T), length.out = input$bins + 20)
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkblue', border = 'white', xlab = "Total Deaths", main = "Histogram of Total deaths in Flood") 
    }
    
    # else if(input$indicatorInput == "Fog"){
    #   x    <- dat[dat$disaster.type=="Fog",6] 
    #   bins <- seq(min(x), max(x), length.out = input$bins + 20)
    #   # draw the histogram with the specified number of bins
    #   hist(x, breaks = bins, col = 'darkblue', border = 'white', xlab = "Total Deaths", main = "Histogram of Total deaths in Fog") 
    # }
    
    # else if(input$indicatorInput == "Impact"){
    #   x    <- dat[dat$disaster.type=="Impact",6] 
    #   bins <- seq(min(x), max(x), length.out = input$bins + 20)
    #   # draw the histogram with the specified number of bins
    #   hist(x, breaks = bins, col = 'darkblue', border = 'white', xlab = "Total Deaths", main = "Histogram of Total deaths in Impact") 
    # }
    
    else if(input$indicatorInput == "Insect infestation"){
      x    <- dat[dat$disaster.type=="Insect infestation",]$Total.deaths 
      bins <- seq(min(x, na.rm = T), max(x, na.rm = T), length.out = input$bins + 20)
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkblue', border = 'white', xlab = "Total Deaths", main = "Histogram of Total deaths in Insect infestation") 
    }
    
    else if(input$indicatorInput == "Landslide"){
      x    <- dat[dat$disaster.type=="Landslide",]$Total.deaths 
      bins <- seq(min(x, na.rm = T), max(x, na.rm = T), length.out = input$bins + 20)
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkblue', border = 'white', xlab = "Total Deaths", main = "Histogram of Total deaths in Landslide") 
    }
    
    else if(input$indicatorInput == "Mass movement (dry)"){
      x    <- dat[dat$disaster.type=="Mass movement (dry)",]$Total.deaths 
      bins <- seq(min(x, na.rm = T), max(x, na.rm = T), length.out = input$bins + 20)
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkblue', border = 'white', xlab = "Total Deaths", main = "Histogram of Total deaths in Mass movement (dry)") 
    }
    
    else if(input$indicatorInput == "Storm"){
      x    <- dat[dat$disaster.type=="Storm",]$Total.deaths 
      bins <- seq(min(x, na.rm = T), max(x, na.rm = T), length.out = input$bins + 20)
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkblue', border = 'white', xlab = "Total Deaths", main = "Histogram of Total deaths in Storm") 
    }
    
    else if(input$indicatorInput == "Volcanic activity"){
      x    <- dat[dat$disaster.type=="Volcanic activity",]$Total.deaths 
      bins <- seq(min(x, na.rm = T), max(x, na.rm = T), length.out = input$bins + 20)
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkblue', border = 'white', xlab = "Total Deaths", main = "Histogram of Total deaths in Volcanic activity") 
    }
    
    else{
      x    <- dat[dat$disaster.type=="Wildfire",]$Total.deaths 
      bins <- seq(min(x, na.rm = T), max(x, na.rm = T), length.out = input$bins + 20)
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkblue', border = 'white', xlab = "Total Deaths", main = "Histogram of Total deaths in Wildfire") 
    }
    
    
  })
})

