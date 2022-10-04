library(shinythemes)
library(shiny)
library(rgdal)
library(leaflet)
library(tidyverse)
library(readr)
library(RColorBrewer)
library(sp)
library(raster)
library(stargazer)
library(rgdal)
library(survey)
setwd("~/Documents/Upenn/Data 410/Week 3")


# make streamlined ACEs app

# The lecture video has got all of the bells and whistles you could possibly
# have for the regression

# In the synch session, we're going to scale it back and talk about
# the 'key features'

# for the homework, you can start with the synch version as a guide
# and then borrow from lecture as you see fit. 

# Let's start by looking at the full version of the app - with the bells
# and whistles, and see if we can pick out the different 'parts'

# LAUNCH FULL APP

# LAUNCH REDUCED APP-
# these are the pieces we're going to work with
# call out what they are

# Want to test yourse?
# - pull out the relevant parts of the UI
# and then the server into a new script
# and then compare to this script!


# Loading in my dataframe
place<-read.csv("~/Documents/Upenn/Data 410/Week 1/Homework/place_data.csv")
place<-as.data.frame(place)
place$row_num <- seq.int(nrow(place))

# svydesign from survey package adds weights to my observations
comp.design <- svydesign(id= ~row_num, data= place)


server <- function(input, output) {
  
  #creating a reactive regression forumula that uses inputs from the check list
  #as independent variables to predict the variable ACE_BI
  regFormula <- reactive({
    as.formula(paste("Risk"," ~ ",paste(input$iv1,collapse="+")))
    
  }) 
  
  
  # then, put that formula into the svyglm() from survey package which outputs
  # a weighted regression
  model <- reactive({
    lm(regFormula(), family= quasibinomial, place)
    
  })
  
  #Create nice regression table output
  #stargazer() comes from the stargazer package
  output$regTab <- renderText({
    stargazer(model(), type = "html", dep.var.labels = "Risk Prediction")
  })
  
  
}




# Notice there is no function here, just the fluidPage()
ui <- shinyUI(fluidPage(tabPanel(
  "Analyzing Risk",
  headerPanel("Risk Prediction Model"),
  #The sidebar allows me to select
  #multiple independent variables
  sidebarLayout(
    position = "right",
    sidebarPanel(
      h2("Build your model"),
      br(),
      checkboxGroupInput(
        "iv1",
        label = "Select any of the independent variables below to calculate your model. You can change your selection at any time.",
        list("Poverty"="Poverty", "Education"="Education", "Unemployment"="Unemployment","Crime"="Crime",
             "ACEs"="ACEs"), selected="Poverty"
        #Note Race is selected when the app starts
      ) # checkboxGroupInput
    ), #sidebarpanel
    
    
    
    mainPanel(br(),
              #create a 1st tab panel
              tabsetPanel(
                type = "tabs",
                #first panel shows regression table
                tabPanel(
                  "Regression Table",
                  h3("Table of Regression Coefficients"),
                  HTML('</br>'),
                  tableOutput("regTab"),
                  HTML('</br>'),
                  helpText("Describe the model")
                )# tab panel
              )# tabset
    )# mainpanel
  ) #sidebarlayout
) # tab panel
) # fluidpage
) #shinyUI
# This is our new code to merge the server and ui objects into an app!
shinyApp(ui = ui, server = server)
