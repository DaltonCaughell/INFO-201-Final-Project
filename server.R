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

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  femaData <- data.frame(read.csv("data/fema_data.csv"))
  
  femaData$EventDate <- as.Date(femaData$Incident.Begin.Date, format="%m/%d/%Y")
  
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
  
  output$heatmap <- renderPlot({
    
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
    
    heatmap <- ggplot(map.df, aes(x=long,y=lat,group=group))+
      geom_polygon(aes(fill=count))+
      geom_path()+ 
      scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90")+
      coord_map()
    
    return(print(heatmap))
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
  
})
