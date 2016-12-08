library(shiny)
library(plotly)
library(dplyr)
library(ggplot2)
library(maps)
library(mapproj)
library(zipcode)
library(leaflet)
library(ggmap)
library(jsonlite)

world.data <- data.frame(read.csv("data/natdisdata.csv", col.names = c("year", "iso", 'country.name', 'disaster.type', 'occurrences', 'deaths', 'injured', 'affected', 'homeless', 'total.affected', 'total.damage')))
world.data <- world.data[-c(1),]
world.data <- world.data %>% group_by(country.name) %>%
  summarise(occurrences = sum(occurrences), deaths = sum(deaths), injured = sum(injured), affected = sum(affected), homeless = sum(homeless), total.affected = sum(total.affected), total.damage = sum(total.damage))
world.data <- world.data[-c(1),]
world.data$country.names <- sub(" \\(.*", " ", world.data$country.name)
world.data <- world.data[-c(1)]
world.data <- world.data[-c(13, 32, 37, 74, 75, 159, 182, 218, 219),]
world.data$location <- geocode(world.data$country.names)

write.csv("data/world.csv", flatten(world.data))