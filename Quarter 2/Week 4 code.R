##########
# Homework Week 4
# Zai Rutter
##########

## Set Up
library(dplyr)
library(tidyverse)
library(readr)
library(readxl)

setwd("~/Documents/Upenn/Data 210/Week 4/homework")

### Question 1

## A
ed<- read_csv("education_long.csv")
ed.w<- read_csv("education_long.csv")

## B
# Reshape the data into ‘wide format’

ed.w <- pivot_wider(ed.w, names_from = Year, values_from = College_Completion_Rate)

## C
#Read in sheet 2 of the file ‘PovertyReport.xlsx’

PovertyReport <- read_excel("PovertyReport.xlsx", 
                            sheet = "PovertyReport", col_types = c("text", 
                                                                   "skip", "skip", "skip", "numeric", 
                                                                   "skip", "numeric", "numeric", "numeric", 
                                                                   "numeric", "numeric"), skip = 5)

## D
# The poverty data was collected in 2017. Select rows from the education dataset from 2017.

ed.w <- ed.w %>%
  select(Name,`2013-2017_Total`,`2013-2017_Urban`,`2013-2017_Rural`)


## E
# Merge these two data sets so that all matched observations are retained.
###### DOUBLE CHECK HE'S ASKING FOR INNER JOIN

PovertyReport.w <- PovertyReport
PovertyReport.w <- PovertyReport.w[-c(53,54),]

PovertyReport.w <- rename(PovertyReport.w,
                          pct.Adults.in.poverty.2017=Percent...2,
                          LowerBound.Adults.in.poverty.2017='Lower Bound...3',
                          UpperBound.Adults.in.poverty.2017='Upper Bound...4',
                          pct.Children.in.poverty.2017='Percent...5',
                          LowerBound.Child.in.poverty.2017='Lower Bound...6',
                          UpperBound.Child.in.poverty.2017='Upper Bound...7')

Poverty.education <- merge(x = PovertyReport.w, 
               y = ed.w, 
               by = "Name")

## F
#Graphically investigate the relationship between general poverty levels 
# and the college completion rates for each demographic group. What do you notice?
#### CHILDRN OR ADULT POVERTY LVELS

long.poverty.education <- pivot_longer(Poverty.education,
             cols=c(pct.Adults.in.poverty.2017, pct.Children.in.poverty.2017),
             names_to = "Poverty.group",
               values_to = "value"  )

long.poverty.education$total<-long.poverty.education$`2013-2017_Total`

ggplot(long.poverty.education, mapping=aes(x= value, y= total, color=Poverty.group )) +
  geom_smooth()


##### Question 2

## A
Snapshot6_12 <- read_csv("2006_-_2012_School_Demographics_and_Accountability_Snapshot.csv") # he calls this profile data
SAT_2010 <- read_csv("SAT__College_Board__2010_School_Level_Results.csv")

### B
# For this problem, we want to merge the 2010 SAT dataset with the the NYC school 
# profiles for that year. 
# First, subset the NYC school profile so that it only includes data from the 
# 2009/2010 school year.

Snapshot6_12.w <- subset(Snapshot6_12, schoolyear == 20092010)

### B
# If you look at the dimensions of the data, it seems that there are substantially 
# more schools included in the profile2010 data. Look through both datasets 
# carefully to see why this might be 
# (Hint: search for areas where there are missing values)

## This is because the profile includes some elementary schools and such where as the SAT data does not.

### C
# Remove the values that do not belong.
Snapshot6_12.w$dummy <- Snapshot6_12.w$DBN %in% SAT_2010$DBN

Snapshot6_12.w <- subset(Snapshot6_12.w, dummy == T)


## D
# Merge the datasets. Which variable is the most appropriate to use as a unique identifier?

SAT_Snapshot <- merge(x = SAT_2010, 
      y = Snapshot6_12.w, 
      by = c("DBN"),
      all.x = T)

### E
## Now, let’s explore this data a bit. 
# Say we want to see how the racial demographics of a school impact the school’s 
# average critical reading score on the SAT. 
# Plot these two variables for each race on four different graphs and discuss findings.

SAT_Snapshot.w <- pivot_longer( SAT_Snapshot,
                                cols = c(asian_per,black_per,hispanic_per,white_per),
                                names_to = "POC",
                                values_to = "poc.per")
SAT_Snapshot.w$CR <- SAT_Snapshot.w$`Critical Reading Mean`


SAT_Snapshot.w %>%
  group_by(POC) %>%
  ggplot(SAT_Snapshot.w, mapping = aes(x=poc.per, y=CR, color=POC)) +
  geom_point(alpha =.7) +
  facet_wrap(~ POC) +
  geom_smooth(alpha=.3)



