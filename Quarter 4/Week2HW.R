##################
# HW Week 2
# Data 410
# Zai Rutter
# March 30 2022
##################
library(shinythemes)
library(shiny)
library(rgdal)
library(leaflet)
library(tidyverse)
library(readr)
library(RColorBrewer)
library(sp)
library(raster)
library(rgdal)
setwd("~/Documents/Upenn/Data 410/Week 1/Homework")
##################

#-------------------
# Server Code
#------------------

server2 <- function(input, output) {
  place<-read.csv("~/Documents/Upenn/Data 410/Week 1/Homework/place_data.csv")
  place<-as.data.frame(place)
  zipcodepoly <- readOGR("Zipcodes_Poly", layer = "Zipcodes_Poly", encoding = "UTF-8")
  zipcodepoly@data <- data.frame(zipcodepoly@data,
                                 place[match(zipcodepoly@data$CODE, place$CODE),])
  
  colorpal<- colorQuantile("YlOrRd", zipcodepoly@data$Risk, n = 4)
  
  nj_popup <- paste("<strong> Zipcode: </strong>", 
                    zipcodepoly@data$CODE, 
                    "<br><br><strong>Risk Index:</strong>", 
                    zipcodepoly@data$Risk,
                    "<br><strong>Poverty:</strong>", 
                    zipcodepoly@data$Poverty,
                    "<br><strong>Education:</strong>", 
                    zipcodepoly@data$Education,
                    "<br><strong>nemployment:</strong>", 
                    zipcodepoly@data$Unemployment,
                    "<br><strong>Crime:</strong>", 
                    zipcodepoly@data$Crime,
                    "<br><strong>ACEs:</strong>", 
                    zipcodepoly@data$ACEs
  )
  
  week1map<- leaflet(zipcodepoly) %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView( -75.1489258697744,39.952409661807366, zoom = 10.7) %>%
    addPolygons( weight = 1, smoothFactor = 0.5,
                 opacity = 1.0, fillOpacity = .8,
                 color = ~colorpal (Risk),
                 popup = nj_popup,
                 highlightOptions = highlightOptions(color = "white", weight = 2,
                                                     bringToFront = TRUE)) %>%
    
    addLegend("bottomright", 
              colors = c("#ffffb2","#fecc5c","#fd8d3c","#e31a1c"),
              labels = c("0-24","25-49","50-74","75-98"),  
              title = "Risk: Lowest to Highest",
              opacity = 1) 

  
  output$week1map <- renderLeaflet(week1map)
}
  
#-------------------
# User Code
#------------------

ui2<- shinyUI(fluidPage(theme = shinytheme("flatly"),
                  
                  # Set the style for the header
                  tags$head(
                    tags$style("h2 {color: #ee5500; }
                                                   h1 {color: #04B4AE}; }
                                                   ")),
                  # Create a title
                  headerPanel("Mapping Risk in Philly"),
                  br(), # br() is borrowed from html. It creates a space, called a break
                  h2(""), # Another way to create a space is to add an empty text space. If there were 
                  # text inside those parenthesis, it would be printed in the app. Try it!
                  
                  # This line controls the size of the map. I have set the width to 100% - this will
                  # adjust the map to the size of any screen it is displayed on. 
                  # The height is measured in px because I do want to control that length 
                  # Most importantly, notice "map" is coming from the server script
                  
                  leafletOutput("week1map", width = "100%", height = "800px")
))    
  
shinyApp(ui=ui2, server = server2)



