#### 
# Midterm Data 210
# Zai Rutter
# November 26th 2021
#####

#### 
# Load data
setwd("~/Documents/Upenn/Data 210/Week 5/Midterm")
library(tidyverse)
library(fastLink)
library(dplyr)
install.packages("haven")
genforward_sept_2017 <- read_sav("genforward sept 2017.sav")
View(genforward_sept_2017)

Gs17<- genforward_sept_2017
glimpse(genforward_sept_2017)

### Question 3
### A
# 
# What percent of the sample strongly approved or somewhat approved of the way 
#  that President Trump is handling his job as president (using question Q1)?

# strong approved is 1 , somewhat is 2
attributes(Gs17$Q1)

# number of people sampeled # 1741
sum(length(unique(Gs17$GenF_ID)))


Gs17$approve.trump <- ifelse( Gs17$Q1 <=2, T, F)
table(Gs17$approve.trump == T)

# Percent
256/1741

###B
# What percentage of Republican men “strongly approve” or “somewhat approve” 
# of the way Trump is handling his job as president? 

# Male = 1, Female = 2
attributes(Gs17$gender)

# Republican = 6,5,7
attributes(Gs17$PartyID7)
Gs17$republican <- ifelse(Gs17$PartyID7 >=5,T,F)

(sum(Gs17$approve.trump[Gs17$gender == 1 & Gs17$republican == T]))/(sum(length(unique(Gs17$GenF_ID))))


# What is this percentage for Republican women? 
(sum(Gs17$approve.trump[Gs17$gender == 2 & Gs17$republican == T]))/(sum(length(unique(Gs17$GenF_ID))))

# What percentage of Republican men and Republican women 
# (separately) “somewhat disapprove” or “strongly disapprove” of Trump?

#Somewaht disapprove / strongly disapprove = 5 and 4
attributes(Gs17$Q1)

## rep men = 0
(sum(Gs17$approve.trump[Gs17$Q1 == 4 & Gs17$Q1 == 5 & Gs17$republican == T & 
                          Gs17$gender == 1]))/(sum(length(unique(Gs17$GenF_ID))))

## rep women = 0
(sum(Gs17$approve.trump[Gs17$Q1 == 4 & Gs17$Q1 == 5 & Gs17$republican == T & 
                          Gs17$gender == 2]))/(sum(length(unique(Gs17$GenF_ID))))

######### C
# Which two issues did 2016 Trump voters indicate were the most 
# important problems facing the country? What percentage of Trump voters 
# listed each of these two issues as the top issue?

#Q13
#Trump Voters == 2
attributes(Gs17$Q0)

Trumpvoters<- Gs17[Gs17$Q0 == 2,]

# most important = 1
attributes(Gs17$Q13_1)

Q13 <- Trumpvoters[ , grepl( "Q13" , names( Gs17 ) ) ]
glimpse(Gs17)


for(i in seq_along(Q13)){
  Q13totals[[i]]<-sum(Q13[[i]])
}

# Question 21 and Question 6 are the most important 
# 21) 43 6) 25
#21
43/236
25/236


######### D
# What percentage of 2016 Clinton voters listed these two issues are the most 
#  important problem facing the country?

ClintonVoters<- Gs17[Gs17$Q0 == 1,]
ClintonQ13 <- ClintonVoters[ , grepl( "Q13" , names( Gs17 ) ) ]

C13 <- vector("double", ncol(ClintonQ13))

for(i in seq_along(ClintonQ13)){
  C13[[i]]<-sum(ClintonQ13[[i]])
}

View(C13)

38/853
116/853

####### E
# What are the top three issues that women over 30 years old care about? 
# Are these top issues the same for women aged 30 and under?

## top 3 over 30 = 14,6,3 
## top 3 under 30 = 14,6,3

glimpse(Gs17)
attributes(Gs17$AGE4)
attributes(Gs17$AGE7)
attributes(Gs17$age)
table(Gs17$age)

Wunder30 <- Gs17[Gs17$gender == 2 & Gs17$AGE4 == 1,]

# top 3 over 30
Wover30<- Gs17[Gs17$gender == 2 & Gs17$AGE4 > 1,]
Wover30Q13 <- Gs17[ , grepl( "Q13" , names( Gs17 ) ) ]

Wover30Total <- vector("double", ncol(Wover30Q13))

for(i in seq_along(Wover30Q13)){
  Wover30Total[[i]]<-sum(Wover30Q13[[i]])
}
View(Wover30Total)

# top 3 under 30
Wunder30 <- Gs17[Gs17$gender == 2 & Gs17$AGE4 == 1,]
Wunder30Q13 <- Wunder30[ , grepl( "Q13" , names( Wunder30 ) ) ]

Wunder30Total <- vector("double", ncol(Wunder30))

for(i in seq_along(Wunder30Q13)){
  Wunder30Total[[i]]<-sum(Wunder30Q13[[i]])
}
View(Wunder30Total)

####### Question 4

# A
library(readr)
nyc_central_park_temps <- read_csv("nyc-central-park-temps.csv")

nyc<- separate(nyc_central_park_temps,
         col = DATE,
         into = c('Year', 'Month', 'Day'),
           sep="-")

## Which years are missing at least one day of temperature data, and 
# how many days are missing?
## none??

sum(is.na(nyc$TMAX))
sum(is.na(nyc$TMIN)) 

nyc_central_park_temps %>%
  filter(is.na(TMAX))

### B
# Create a variable that tells us the 
# difference between the highest and lowest temperature for each day.

nyc <- nyc %>%
  group_by(Year, Month, Day) %>%
  mutate(DailyDifference = lag(TMAX,0)-lag(TMIN,0)) %>%
  filter(DailyDifference ==max(DailyDifference)) 

# Across the full dataset, what the average of this difference?

mean(nyc$DailyDifference)

# Which day during this 150 year window had the biggest difference between the 
# highest and lowest temperature? 

# 1921, march 28 had highest difference of 48 degrees
nyc %>%
  filter(DailyDifference == 48) 


#Averaging across years, which month tends to 
# have the highest average difference in daily high and low temperatures?

# May 

nycsummary <- nyc %>%
  group_by(Year, Month) %>%
  mutate(average.monthly = mean(DailyDifference)) 

aggregate(DailyDifference ~ Month,
          nycsummary,
          mean)

####### C
#  Load and merge in the precipitation data. 
# What type of merge does it mark 
# sense to perform? Which variable(s) will you merge on? Perform the merge, 
# then use the results to figure out how many days in the past 150 years had a 
# high temperature of at least 50 degrees and received at least 1 inch of snowfall.

nyc_central_park_precipitation <- read_csv("nyc-central-park-precipitation.csv")

nyc_central_park_precipitation<- separate(nyc_central_park_precipitation,
               col = DATE,
               into = c('Year', 'Month', 'Day'),
               sep="-")

weather <- merge(x = nyc_central_park_precipitation, 
               y = nyc, 
               by = c("STATION","Year","Month","Day"),
               all = T)

# how many days in the past 150 years had a high temperature of at least 50 
# degrees and received at least 1 inch of snowfall

weather %>%
  filter(TMAX >= 50) %>%
  filter ( SNOW >= 1 ) %>%
  summarise(n())

###### D
# Aggregate the data by month to figure out what percentage of days have had 
# preciptation since 1869. 
# Your resulting dataset should have 12 rows (one per month). 
# You should use the PRCP variable (and ignore the SNOW variable).

weather <- weather %>%
  mutate(Dummy = ifelse(PRCP >0,T,F))

aggregate(Dummy ~ Month,
          weather,
          mean)

aggregate(Dummy ~ Day,
          weather,
          mean)

# Which month tends to have the most rainy days in New York City?
# March



# What percentage of days does it usually rain in this month? 

# 36.43011%

# And which month tends to be the dryest (i.e. fewest days with precipitation)? 
# What percentage?

# October, 27.20430%


####### E
# Use aggregation to figure out how many days in each year since 1869 
# had a low temperature of 32 degree or below. 

weather <- weather %>%
  mutate(QuestionE = ifelse(TMIN <= 32, T, F))
  
QE <-aggregate(QuestionE ~ Year,
          weather,
          sum)

# Use the plot() function or ggplot2 to make a simple graph of the relationship 
# between the year (on the x-axis) and the number of cold days in 
# Central Park (on the y-axis).
QE %>%
  plot("Year", "QuestionE" )

# What pattern do you notice in this graph

#There is a steady decline of days below 32 degrees since 1869




(sum(Gs17$approve.trump[Gs17$gender == 1 & Gs17$republican == T]))/
  (sum(Gs17$republican == T))
