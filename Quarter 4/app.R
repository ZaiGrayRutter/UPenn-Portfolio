##########################################################################
## Final  App
## Data 410
## Zai Rutter
## May 2022
###########################################################################



#-------------------------------------------------------------------------------
## Loading 
#-------------------------------------------------------------------------------
library(stargazer)
library(wesanderson)
library(shinythemes)
library(rmapshaper)
library(shiny)
library(rgdal)
library(leaflet)
library(tidyverse)
library(readr)
library(RColorBrewer)
library(sp)
library(raster)
library(survey)
library(scales)
library(ggmap)
library(sf)
library(tigris)
library(viridis)
library(rsconnect)


#-------------------------------------------------------------------------------
## Server
#-------------------------------------------------------------------------------

server <- function(input, output){
 
  #-------------------------------------------------------------------------------
  ## Loading and Cleaning Data
  #-------------------------------------------------------------------------------
  
  
# Loading in my dataframe'lea<- read_csv("LEA_data.csv")
  lea<- read.csv("LEA_data.csv")
  phillyfips <- read.csv("fipscodephilly.csv")
  countycensus <- readOGR("tl_2021_us_county", layer = "tl_2021_us_county", stringsAsFactors = F)
  
  lea<- lea %>%
    dplyr::select(c(schoolname,county,safesch_oss_violence,safesch_exp_violence,
           safesch_oss_weapon,safesch_exp_weapon,enroll_allgrades_total,
           spp_act_22plus,dropout_rate,gradrate_4year_total,enrollment_lowinc_pct))

  
# Cleaning the Data
phillyfips$county<-gsub(" County","",phillyfips$county)
phillyfips$`state abbv`<-NULL
phillyfips$`state num`<-NULL
finaldata<-left_join(lea,phillyfips, by="county")
finaldata$GEOCODE<-formatC(finaldata$fips, width = 5, format = "d", flag = "0")
finaldata$COUNTYFP<-formatC(finaldata$fips, width = 3, format = "d", flag = "0")

finaldata$dropout_rate<-as.numeric(finaldata$dropout_rate)
finaldata$gradrate_4year_total<-as.numeric(finaldata$gradrate_4year_total)
finaldata$spp_act_22plus<-as.numeric(finaldata$spp_act_22plus)
finaldata$enrollment_lowinc_pct<-as.numeric(finaldata$enrollment_lowinc_pct)


countycensus$INTPTLAT <- as.numeric(countycensus$INTPTLAT)
countycensus$INTPTLON <- as.numeric(countycensus$INTPTLON)
countycensus@data <- subset(countycensus@data, select=-c(NAME:AWATER))

## Creating the violence variable 
finaldata$incidents <- (finaldata$safesch_oss_violence + finaldata$safesch_exp_violence + 
                          finaldata$safesch_oss_weapon +finaldata$safesch_exp_weapon)
aggregateincident <-aggregate(finaldata$incidents, list(finaldata$COUNTYFP), FUN=sum) 
finaldata<- left_join(finaldata,aggregateincident, by=c("COUNTYFP"="Group.1"))
finaldata$CountyViolence<-finaldata$x
finaldata$HighCountyViolence<-ifelse(finaldata$CountyViolence>1305,1,0) 
mapdata<- finaldata %>%
  dplyr::select("COUNTYFP","schoolname", "county", "incidents","HighCountyViolence","CountyViolence")
finaldata$highviolence<-ifelse(finaldata$incidents>46.5,1,0)
finaldata<-finaldata %>%
  mutate(logincidents=log(incidents))
finaldata$logincidents<-ifelse(finaldata$logincidents<0,NA,finaldata$incidents)




# Preparing data for the map merge



countycensus<-countycensus[countycensus@data$STATEFP %in% c("42"), ]
countycensus <- subset(countycensus, STATEFP %in% c("42") )
countycensus <- rmapshaper::ms_simplify(countycensus)
countycensus@data <- data.frame(countycensus@data,
                                mapdata[match(countycensus@data$COUNTYFP,mapdata$COUNTYFP ),])
countycensus@data$COUNTYFP.1<-NULL
colorpal<- colorFactor(
  "YlOrRd",
  countycensus@data$CountyViolence,
  levels = NULL,
  ordered = FALSE,
  na.color = "#808080",
  alpha = FALSE,
  reverse = FALSE
)

## Create the pop ups
County_popup<-  paste0("<strong>County: </strong>", 
                       countycensus@data$county, 
                       "<br><strong>Number of Incidents:</strong> ",
                       countycensus@data$CountyViolence)

# High violence data set
HighV<- finaldata %>%
  filter(highviolence==1)

#low violence dataset
LowV<-finaldata %>%
  filter(highviolence==0)


LowViolenceModel<- lm(logincidents ~enrollment_lowinc_pct +spp_act_22plus +gradrate_4year_total+dropout_rate, data=LowV)
HighViolenceModel<- lm(logincidents ~enrollment_lowinc_pct +spp_act_22plus +gradrate_4year_total+dropout_rate, data=HighV)
Violence_incidents<-lm(logincidents ~enrollment_lowinc_pct +spp_act_22plus +gradrate_4year_total+dropout_rate, data=finaldata)

finaldata<-finaldata %>%
  mutate(logHighCountyViolence=log(HighCountyViolence))
# High violence county data set
HighCountyV<- finaldata %>%
  filter(HighCountyViolence==1)
#low violence county dataset
LowCountyV<-finaldata %>%
  filter(HighCountyViolence==0)


#finaldata$logincidents<-ifelse(finaldata$logincidents<0,NA,finaldata$incidents)


#LowCountyViolenceModel<- lm(HighCountyViolence ~enrollment_lowinc_pct +spp_act_22plus 
#                            +gradrate_4year_total+dropout_rate, data=LowCountyV)
#HighCountyViolenceModel<- lm(HighCountyViolence ~enrollment_lowinc_pct +spp_act_22plus
#                             +gradrate_4year_total+dropout_rate, data=HighCountyV)
#CountyViolenceModel<-lm(HighCountyViolence ~enrollment_lowinc_pct +spp_act_22plus
#                        +gradrate_4year_total+dropout_rate, data=finaldata)


# high violent school in a low incident county
#HighschoolLowCounty<- finaldata %>%
  #filter( finaldata$highviolence==1 & finaldata$HighCountyViolence == 0 )
## Low incident school, high incident county
## There is now low violent school in a high incident county
#LowschoolHighCounty<- finaldata %>%
 # filter( finaldata$highviolence==0 & finaldata$HighCountyViolence == 1 )

## This is saying that there is not enough data to make any claim on the effect of 
## a school being in a county with high violent incidents, and in fact it is not probable
## for there to be a  violent school in a non violent county


## Do schools in HighschoolLowCounty perform better than HighschoolHighCounty

#HighschoolHighCounty
#HighschoolHighCounty<- finaldata %>%
#  filter( finaldata$highviolence==1 & finaldata$HighCountyViolence == 1 )
#
#HighschoolLowCounty<- lm(HighCountyViolence ~enrollment_lowinc_pct +spp_act_22plus 
#                         +gradrate_4year_total+dropout_rate, data=HighschoolLowCounty)
#HighschoolHighCounty<- lm(HighCountyViolence ~enrollment_lowinc_pct +spp_act_22plus
#                          +gradrate_4year_total+dropout_rate, data=HighschoolHighCounty)



#-------------------------------------------------------------------------------
## Server
#-------------------------------------------------------------------------------
  
  #-------------------------
  # Map
  #-------------------------

map <- leaflet(countycensus) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(smoothFactor = 0.2, fillOpacity = .7,
              color = ~colorpal (countycensus@data$CountyViolence),
              popup = County_popup,
              highlightOptions = highlightOptions(color = "gray", weight = 2,
                                                  bringToFront = TRUE)) # %>%
#  addLegend("topright", 
#            colors =c("#ffffb2","#fecc5c","#fd8d3c","#e31a1c"),
#            labels= c("First Quartile", "Second Quartile", "Third Quartile","Fourth Quartile"), 
#            title= "Aggregated total of Violent Incidents per County",
#            opacity = 1) 

output$map <- renderLeaflet(map)

#-------------------------
# Plotting Reactive Bar Charts about County Map Data
#-------------------------

finaldata$TextCountyViolence<- ifelse(finaldata$HighCountyViolence == 1, "High Incidents","Low Incidents")

FilteredCountydata <- reactive({
  if(input$CODE1 == "Low Incidents")
  {
    cplot <- filter(finaldata, HighCountyViolence == 0)
  }
  else if(input$CODE1 == "High Incidents")
          {
    cplot <- filter(finaldata, HighCountyViolence == 1)
  }
  else{cplot <- finaldata}
  return(cplot)
  
})

#FilteredCountydata <- reactive({
#  if(input$CODE1 == "Low Incidents")
#  {
#    cplot <- filter(finaldata, TextCountyViolence == "Low Incidents")
#  }
#  else if(input$CODE1 == "High Incidents")
#  {
#    cplot <- filter(finaldata, TextCountyViolence == "High Incidents")
#  }
#  else{cplot <- finaldata}
#  return(cplot)
#  
#})

# And finally lets make the plot
output$countyplot <- renderPlot({
  ggplot(FilteredCountydata()) +
  geom_col(aes(x=county, y=CountyViolence, alpha=.9),
           fill = "lightblue",
           position  = "identity") +
    labs(y="Number of Incidents",
         x="") +
    theme_classic()+
    theme(
      #axis.text.x = element_blank(),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
      axis.ticks.y = element_blank(),
      legend.position = "none")
  
})
#ggplot(finaldata) +
#  geom_col(aes(x=county, y=CountyViolence, alpha=.9),
#           fill="lightblue",
#           position  = "identity") +
#  labs(y="Number of Incidents",
#       x="") +
#  theme_classic()+
#  theme(
#    axis.text.x = element_blank(),
#    axis.ticks.y = element_blank(),
#    legend.position = "none")



      #--------------------
      # Plotting  Bar Charts about School Map Data
      #--------------------


# School Plot
output$schoolincidentplot <- renderPlot({
  ggplot(finaldata, aes(x=schoolname, y=incidents,color=factor(HighCountyViolence))) +
  geom_point(alpha=.8) +
  labs(y="Number of Incidents",
       x="") +
  theme(axis.text.x = element_blank(),
       legend.title=element_blank()) +
  scale_color_manual(values = wes_palette("Darjeeling2"),
                     labels = c("Low Incident County","High Incidents County"))
})

#--------------------
# Reactive Stargazer
#--------------------
modellow <- reactive({
  
  lm(paste("logincidents"," ~ ",paste(input$iv1,collapse="+")), LowV)
  
})
modelhigh <- reactive({
  
  lm(paste("logincidents"," ~ ",paste(input$iv1,collapse="+")), HighV)
  
})
modelbase <- reactive({
  
  lm(paste("logincidents"," ~ ",paste(input$iv1,collapse="+")), finaldata)
  
})

# Creating pretty labels for the stargazer table
# Here, we are using if statements to build a vector, covars, that is dependent on the inputs
#from the beck list. 
covar.label <- reactive({
  covars<-character()
  if ('enrollment_lowinc_pct' %in% input$iv1){
    covars <- c(covars,"Low Income")
  } 
  
  if ('spp_act_22plus' %in% input$iv1){
    covars <- c(covars,"Avg. ACT Score")
  } 
  
  if ('gradrate_4year_total' %in% input$iv1){
    covars <- c(covars,"Graduation Rate")
  } 
  
  if ('dropout_rate' %in% input$iv1){
    covars <- c(covars,"Drop Out Rate")
  } 
  if ('enroll_allgrades_total' %in% input$iv1){
    covars <- c(covars,"School Size")
  }
  return(covars)
})

#Create nice regression table output
#stargazer() comes from the stargazer package
output$regTab <- renderText({
  covars <- covar.label()
  stargazer(modellow(),modelhigh(), modelbase(), 
            type="html", covariate.labels = covars, omit.stat = c("f","ser","aic","adj.rsq"))
})
  
  
} #Main Shiny Server Code
                   
#-----------------------------------
# UI
#-----------------------------------


ui <- shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  # This applies preset aesthetic decisions to the entire app
  # Check out this page for more themes: https://rstudio.github.io/shinythemes/
  navbarPage(
    "410",
    # navbarPage lets us set up a navigation bar to toggle between tabPanels
    # First tab
    
    
    tabPanel( "Findings",
      # This is just a blank space - trying typing in the quotes and see what happens!
      # this is a space
      headerPanel("Examining Violence in Schools"),
      br(),
    
        #create a 1st tab panel
        tabsetPanel(type = "tabs", 
                    #first panel shows regression table
                    tabPanel("Explanation",
                             h3("Explanation of Findings"),
                             br(),
                             h4("Premis"),
                             h5("In the past decade school violence has increased. While it’s generally thought that schools with better funding have greater student outcomes, there is less data about the impact that violence has on student outcomes. Through regression analysis I ask what impact violence has in schools, by examining data on student drop out rates, test scores, and graduation rates. "),
                             br(),
                             h4("Variables"),
                             h5("This analysis was created using the Census Bureau geographical data and PA School Data Project data set."),
                             h5("The following variables were totaled and used to create the variable “incidents” which represents total number of violent incidents in a school. This variable was then aggregated by county to create “CountyViolence”.  I then created Boolean indicators for a high incident school by selecting those schools which where in the fourth quartile to be “high incident” and all others to be “low incident”.  I did the same for counties. This data can be found in the “Plots” and “Map” tabs."),
                             h5("Because of the fact that black students may receive harsher punishments for similar acts as their white peers, I made no distinction between suspensions and expulsions."),
                             h5(tags$ul(
                               tags$li("safesch_oss_violence - suspensions for violent incidents"),
                               tags$li("safesch_exp_violence - expulsions for violent incidents"),
                               tags$li("safesch_oss_weapon -suspensions for weapons"),
                               tags$li("safesch_exp_weapon  - expulsions for weapons"))),
                               
                             br(),
                             h4("Regression Analysis "),
                             h5("The linear regression is created with three Modules:"),
                             h5(tags$ul(
                               tags$li("Module 1 - The effect of violence in Low Incident Schools"),
                               tags$li("Module 2 - The effect of violence in High Incident Schools"),
                               tags$li("Module 3 - The effect of violence in all schools")

                             )),
                             h5("Examining the dataset with all the covariants except for School Size shows that when violent incidents increase, it has negative effects for schools regardless if they are are in a high incident or not. Some exceptions occur in Model 1 or, low incident schools. Here, violent incidents only appear to be statistically significant in regards to ACT scores and drop out rates. While the latter makes sense, understanding how ACT scores may improve is confusing. This speaks to the fact that this model does not encapsulate most of the variance, hence the extremely low R squared value. By including the School Size covariate, we see the ACT score become negative and the R squared value improve across the different modules. "),
                             h5("Examining the regression with all the covariants, we can begin to compare the different relationships violence has on schools that are categorized as High/Low Incident. The impact violence has on ACT scores and drop out rates are more statistically significant in Module 1 than in Module 2. However, Module 1 has a far lower R squared value than its counterpart. While there may be a correlation between violent incidents and these covariants, there is not enough data to determine causality."),
                             h5(" Looking at Module 2 we notice that most covariants except for Low Income and School Size, are no longer statistically significant. The R squared value is high, so we could attribute an increase of violent incidents with an increase in school size and percentage of low income students.  In Module 3, we can see a very strong relationship between a decrease in graduation rates, an increase in low income students, drop out rates, and school size, as violent incidents increase."),
                             h5("To summarize, we can state, by looking at Module 3, that there is an overarching relationship between poor performance in schools and violent incidents. We see that low and high incident schools are likely to be impacted differently by violent incidents. It’s not possible to make any conclusions on the causality between violence and poor performance in low incident schools since the R squared value is so low. We can attest, however, that violent incidents affect low incident and high incident schools much differently. Being in a high incident schools means that violence is more likely to occur with an increase in school size and low income percentage. Finally, when looking at all schools in Module 3, we see a strong relationship in the decease in graduation rates, and increase in drop out rates when violent incidents occur."),
                             br(),
                             br(),
                             br()
                             
                             ),
        
                    tabPanel("Regression Table",
                             fluidRow(

                               column(12,
                          sidebarLayout(
                                       sidebarPanel( width = 4, position = "right",
                                         h2("Build your model"),
                                         br(),
                                         checkboxGroupInput("iv1", 
                                                            label = "Select any of the independent variables below to calculate your model.", 
                                                                         list("Low Income"="enrollment_lowinc_pct", "Avg. ACT Score"="spp_act_22plus", "Graduation Rate "="gradrate_4year_total",
                                                                              "Drop Out Rate"="dropout_rate","School Size"="enroll_allgrades_total"), 
                                                                         selected ="enrollment_lowinc_pct")
                     ),                                                    #Note race is selected when the app starts
                      mainPanel(width = 8,
                               tableOutput("regTab"),
                               helpText("The table displays the coefficients of your model in the log-odds scale. Larger numbers indicate that a variable has a greater effect. The P value determines statistical significance. P values below 0.05 are commonly accepted as significant."),
                     
                               h5("Module 1 - The effect of violence in Low Incident Schools"),
                               h5("Module 2 - The effect of violence in High Incident Schools"),
                               h5("Module 3 - The effect of violence in all schools")
                               
                                     ))) )))),

    
    # Second tab
    tabPanel(
      "Mapping Violence",
      headerPanel("Mapping Violence by County"),
      br(),
      # the map is called here
      leafletOutput("map",
                    width = "100%",
                    height = "600px"),
      br(),
      h5("This map represents the aggreagated the sums of violent incidents by county.")
      ),
      
  
      tabPanel(
        # third tab
        "Plots",
        headerPanel("Distribution of Incidents"),
        br(),
        br(),
        br(),
        br(),
    
        h4("Distribution of Incidents per County"),
        
        selectInput("CODE1",
                    "County Type:",
                    c("All",
                      "High Incidents",
                      "Low Incidents")),
        
        # Create the table.
        plotOutput("countyplot"),
        
        br(),
        br(),
        br(),
        br(),
        h4("Distribution of Incidents per School"),
        
        
        plotOutput("schoolincidentplot")
      )
    )
  )) ## Keep these
  shinyApp(ui = ui, server = server)
  
  
  
  
  
  
                   