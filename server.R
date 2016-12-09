#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(dplyr)
library(ggplot2)
library(maps)
library(mapproj)
library(zipcode)
library(leaflet)
library(ggmap)

# Disasters Data INIT
DisasterData <- data.frame(read.csv("data/disasters.csv"))

shinyServer(function(input, output, session) {

  # FEMA Data INIT
  femaData <- data.frame(read.csv("data/fema_data.csv"))
  femaData$EventDate <- as.Date(femaData$Incident.Begin.Date, format="%m/%d/%Y")
  
  # World Data INIT
  world.data <- data.frame(read.csv("data/world.csv"))
  
  ### United States Page ###
  
  # Observe for Clear Checkbox
  observe({
    if (input$Uncheck > 0) {
      data <- subset(femaData, EventDate > input$DateRange[1] & EventDate < input$DateRange[2]) %>% 
        group_by(Incident.Type) %>% 
        summarise(count=n()) %>%
        filter(Incident.Type != "") %>%
        select(Incident.Type)
      
      updateCheckboxGroupInput(session, "Types", "Disaster Type(s)", choices=data$Incident.Type)
    }
  })
  
  output$States <- renderUI({
    
    data <- subset(femaData, EventDate > input$DateRange[1] & EventDate < input$DateRange[2]) %>% 
      group_by(State) %>% 
      summarise(count=n()) %>%
      filter(State != "") %>%
      select(State)
    
    return(selectInput("State", "Choose State", choices=data$State))
  })
  
  output$DisasterTypes <- renderUI({
    
    data <- subset(femaData, EventDate > input$DateRange[1] & EventDate < input$DateRange[2]) %>% 
      group_by(Incident.Type) %>% 
      summarise(count=n()) %>%
      filter(Incident.Type != "") %>%
      select(Incident.Type)
    
    return(checkboxGroupInput("Types", "Disaster Type(s)", choices=data$Incident.Type, selected=data$Incident.Type))
  })
  
  output$summary1 <- renderText({
    data <- subset(femaData, EventDate > input$DateRange[1] & EventDate < input$DateRange[2]) %>%
      filter(Incident.Type %in% input$Types) %>%
      group_by(State) %>% 
      summarise(count=n()) %>% 
      filter(State != "")
    max <- filter(data, count==max(count))
    if(nrow(max) == 1) {
      return(paste0("The state of ", paste(max$State), " experiences the most of these types of disasters."))
    } else if(nrow(max) > 1) {
      return(paste0("The states: ", paste(max$State,collapse = ", "), " experience the most of these types of disasters."))
    } else {
      return()
    }
  })
  
  output$summary2 <- renderText({
    data <- subset(femaData, EventDate > input$DateRange[1] & EventDate < input$DateRange[2]) %>%
      filter(Incident.Type %in% input$Types) %>%
      group_by(State) %>% 
      summarise(count=n()) %>% 
      filter(State != "")
    min <- filter(data, count==min(count))
    if(nrow(min) == 1) {
      return(paste0("The state of ", paste(min$State), " experiences the least of these types of disasters."))
    } else if(nrow(min) > 1) {
      return(paste0("The states: ", paste(min$State,collapse = ", "), " experience the least of these types of disasters."))
    } else {
      return()
    }
  })
  
  output$heatmap <- renderPlotly({
    
    femaDataState <- subset(femaData, EventDate > input$DateRange[1] & EventDate < input$DateRange[2]) %>%
      filter(Incident.Type %in% input$Types) %>%
      group_by(State) %>% 
      summarise(count=n()) %>% 
      filter(State != "")
    
    femaDataState$lState <- tolower(femaDataState$State)
    
    states <- map_data("state")
    
    stateNames <- data.frame(state.name, state.abb)
    
    map.df <- merge(stateNames,femaDataState, by.x="state.abb", by.y="State", all.x=T)
    
    map.df$state.name <- tolower(map.df$state.name)
    
    map.df <- merge(states,map.df, by.x="region", by.y="state.name", all.x=T)
    
    # Need to re-order the data to prevent polygons from being drawn twisted
    map.df <- map.df[order(map.df$order),]
    
    heatmap <- ggplot(map.df, aes(x=long,y=lat,group=group))+
      geom_polygon(aes(fill=count))+
      geom_path()+ 
      scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90")+
      coord_map()
    
    return(ggplotly(heatmap))
  })
  
  output$bargraph <- renderPlotly({
    
    data <- subset(femaData, EventDate > input$DateRange[1] & EventDate < input$DateRange[2]) %>%
      filter(State==input$State) %>%
      group_by(Incident.Type) %>% 
      summarise(count=n()) %>%
      filter(Incident.Type != "") %>%
      filter(count != 0)
    
    p <- plot_ly(data=data,
      x = ~Incident.Type,
      y = ~count,
      name = paste0("Disasters in ", input$State),
      type = "bar"
    ) %>%
    layout(yaxis=list(title="# Of Occurrences"),xaxis=list(title="Incident Type"))
    
    return(p)
  })
  
  ### World Page ###
  
  output$countrys <- renderUI({
    return(selectInput("country", "1st Country:", choices=world.data[['country.names']]))
  })
  
  output$countrys2 <- renderUI({
    return(selectInput("country2", "2nd Country:", choices=world.data[['country.names']]))
  })
  
  output$worldbarplot <- renderPlotly({
    
    data <- world.data %>%
      filter(country.names %in% c(paste(input$country), paste(input$country2))) %>%
      select(2, 3, 4, 6)
    
    p <- plot_ly(data =  data,
                 x = colnames(data),
                 y = unname(unlist(data[1,])),
                 name = input$country,
                 type = "bar") %>%
                 add_trace(y = unname(unlist(data[2,])), name = input$country2) %>%
                 layout(yaxis=list(title="Number Of Occurrences"),xaxis=list(title="Affects"))
    return(p)
  })
  
  output$worldscatterplot <- renderPlotly({
    
    p <- plot_ly(type = "scattergeo",
            lon = world.data$location.lon,
            lat = world.data$location.lat,
            text = paste(world.data$country.names, paste("Total Affected:", world.data$total.affected), paste("Total Damage:", world.data$total.damage), sep = "<br />"),
            mode = 'markers') 
    return(p)
  })
  
  ### Disasters Page ###
  
  output$DDisasterTypes <- renderUI({
    return(selectInput("indicatorInput", "Disaster Type",choices = levels(DisasterData$disaster.type)[-c(1,7,8)]))
  })
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  
  output$summary<- renderPrint({
    filtered <- DisasterData %>% 
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
      x    <- DisasterData[DisasterData$disaster.type=="Drought",]$Total.deaths 
      bins <- seq(min(x, na.rm = T), max(x, na.rm = T), length.out = input$bins + 20)
      # draw the histogram with the specified number of bins
      hist(x, col = 'darkblue', border = 'white', xlab = "Total Deaths", main = "Histogram of Total deaths in Drought") 
      #      ggplot(x,aes(x))+geom_histogram(binwidth=100, colour="black", fill="white")
    }
    
    else if(input$indicatorInput == "Earthquake"){
      x    <- DisasterData[DisasterData$disaster.type=="Earthquake",]$Total.deaths 
      bins <- seq(min(x, na.rm = T), max(x, na.rm = T), length.out = input$bins + 20)
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkblue', border = 'white', xlab = "Total Deaths", main = "Histogram of Total deaths in Earthquake") 
    }
    
    else if(input$indicatorInput == "Epidemic"){
      x    <- DisasterData[DisasterData$disaster.type=="Epidemic",]$Total.deaths 
      bins <- seq(min(x, na.rm = T), max(x, na.rm = T), length.out = input$bins + 20)
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkblue', border = 'white', xlab = "Total Deaths", main = "Histogram of Total deaths in Epidemic") 
    }
    
    else if(input$indicatorInput == "Extreme temperature "){
      x    <- DisasterData[DisasterData$disaster.type=="Extreme temperature ",]$Total.deaths 
      bins <- seq(min(x, na.rm = T), max(x, na.rm = T), length.out = input$bins + 20)
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkblue', border = 'white', xlab = "Total Deaths", main = "Histogram of Total deaths in Extreme temperature ") 
    }
    
    else if(input$indicatorInput == "Flood"){
      x    <- DisasterData[DisasterData$disaster.type=="Flood",]$Total.deaths 
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
      x    <- DisasterData[DisasterData$disaster.type=="Insect infestation",]$Total.deaths 
      bins <- seq(min(x, na.rm = T), max(x, na.rm = T), length.out = input$bins + 20)
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkblue', border = 'white', xlab = "Total Deaths", main = "Histogram of Total deaths in Insect infestation") 
    }
    
    else if(input$indicatorInput == "Landslide"){
      x    <- DisasterData[DisasterData$disaster.type=="Landslide",]$Total.deaths 
      bins <- seq(min(x, na.rm = T), max(x, na.rm = T), length.out = input$bins + 20)
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkblue', border = 'white', xlab = "Total Deaths", main = "Histogram of Total deaths in Landslide") 
    }
    
    else if(input$indicatorInput == "Mass movement (dry)"){
      x    <- DisasterData[DisasterData$disaster.type=="Mass movement (dry)",]$Total.deaths 
      bins <- seq(min(x, na.rm = T), max(x, na.rm = T), length.out = input$bins + 20)
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkblue', border = 'white', xlab = "Total Deaths", main = "Histogram of Total deaths in Mass movement (dry)") 
    }
    
    else if(input$indicatorInput == "Storm"){
      x    <- DisasterData[DisasterData$disaster.type=="Storm",]$Total.deaths 
      bins <- seq(min(x, na.rm = T), max(x, na.rm = T), length.out = input$bins + 20)
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkblue', border = 'white', xlab = "Total Deaths", main = "Histogram of Total deaths in Storm") 
    }
    
    else if(input$indicatorInput == "Volcanic activity"){
      x    <- DisasterData[DisasterData$disaster.type=="Volcanic activity",]$Total.deaths 
      bins <- seq(min(x, na.rm = T), max(x, na.rm = T), length.out = input$bins + 20)
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkblue', border = 'white', xlab = "Total Deaths", main = "Histogram of Total deaths in Volcanic activity") 
    }
    
    else{
      x    <- DisasterData[DisasterData$disaster.type=="Wildfire",]$Total.deaths 
      bins <- seq(min(x, na.rm = T), max(x, na.rm = T), length.out = input$bins + 20)
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkblue', border = 'white', xlab = "Total Deaths", main = "Histogram of Total deaths in Wildfire") 
    }
  })
  
})
