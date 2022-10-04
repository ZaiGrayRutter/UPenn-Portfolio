## UPenne Final (edited)
# Zai Rutter

##Load
setwd("~/Documents/Upenn/Data 101/Final")
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggthemes)
library(scales)
library(devtools)
library(readxl)
library(maps)
library(mapdata)
library(RColorBrewer)
Senate <- read.csv("1976-2020-senate.csv")

## Clean the data
Senateclean <- Senate %>%
  select(year, state, state_po, stage, candidate, party_detailed,
         candidatevotes, totalvotes) %>%
  rename(party = "party_detailed") %>%
  na_if("") %>%
  drop_na(candidate, party)

## Recode for party affiliation
# D - dem | R - reb | I - all else

Senateclean2 <- Senateclean %>%
  mutate(party3 = "I") %>%
  mutate(party3 = ifelse(grepl("^REPUBLICAN$",party), "R","I")) %>%
  mutate(party3 = ifelse(grepl("^DEMOCRAT$",party), "D",party3)) 

# Plotting totals
ggplot(Senateclean2, mapping=aes(x=party3, posistion= "dodge")) +
  geom_bar()
Senateclean2 %>%
  group_by(party3) %>%
  summarise(n=n())

# remove the independent candidates from the data. 
# remove all the rows where "stage" is not equal to "gen". 
# This ensures that we only get results from the general election.

Question6 <- Senateclean2 %>%
  filter(party3 != "I") %>%
  filter(stage == "gen")

## 7
#How many races were contested between more than two candidates? 
# Which state had the most of these races?
# This graph shows the number of races with more than 2 candidates throughout the years. 
# Louisiana had the most.


Question7 <- Question6 %>%
  group_by(state,year) %>%
  summarise(party3 = n ())

Question7.a <- Question7 %>%
  mutate(contested = ifelse(party3>2, 1, 0)) %>%
  filter(contested == 1)

ggplot(Question7.a, mapping=aes(x=state, posistion = "dodge", fill=state)) +
  geom_bar() +
  coord_flip() +
  theme(plot.title = element_text(size = 20, face = "bold", color = "pink",
                                  hjust = 0.5))+
  labs(title="Number of Contested Races")

## 8
# For Democratic and Republican candidates create a figure that displays year on 
# the x-axis and each candidate's percent of the vote on the y-axis. 
# Be sure to color code each candidate by their respective party. 
# Add two lines -- one for each party -- that represents the trend in that parties' support overtime.

Question8 <- Question6 %>%
  mutate(perc_vote = (candidatevotes/totalvotes))

ggplot(Question8, mapping=aes(x=year,y=perc_vote, group=party3,color=party3, alpha=.1)) +
  scale_color_manual(values =c("blue", "red")) +
  theme(plot.title = element_text(size = 20, face = "bold", color = "black",
                                  hjust = 0.5)) +
  labs(title="Party Popularity Trend", y ="Vote Percentage", x = "Year", color = "Party") +
  geom_jitter() +
  geom_smooth() 


## 9
# Let's take a look at the races from 2012. 

# Filter your dataset so that it only 
# contains the results for 2012, and only the columns `year`, `state`, `party3`, 
# and the candidate percent you calculated in the previous question. 

# Reshape this data so that there is only one row per state, and two columns that
# represent the percent of the vote won by the Republican candidate and the percent 
# of the vote won by the Democratic candidate. Note that you will not have 50 rows 
# because not all states have a Senate election in an election year.

Question9 <- Question8 %>%
  filter(year == 2012) %>%
  select(year, state, party3, perc_vote) %>%
  spread(key = party3, value = perc_vote) %>%
  
  Question9[is.na(Question9)] <- 0

#10
# Create a variable "demwin" that records if the Democrat received a higher vote share than the Republican in each race in 2012.

Question10 <- Question9 %>%
  mutate(demwin = ifelse( D > R, 1, 0))
                 
                 
#11
# Create a variable "demdiff" that records the difference between the Democratic 
# and Republican share of the vote in each race in 2012.

Question11 <- Question10 %>%
  mutate(demdiff = (D - R))
  
  
  #12
  #Next, we're going to do some analysis to map this data. Load in the state-level 
  # mapping data that we've worked with from the package mapdata
  
  counties <- map_data("county")
states <-map_data("state")
  
  
  # 13
  #Join the 2012 Senate election data to this mapping data. Be cautious about the format of the state names
  #state + question 11
  
  Question11$state = tolower(Question11$state)
map.states<-rename(states, "state" = region)

Question13 <- Question11 %>%
  full_join(map.states, by="state")

# Create a map that shows the winner of each Senate contest in 2012, 
ggplot() +
  geom_polygon(data = Question13, aes(x=long, y=lat, group=group, 
                                      fill=as.factor(demwin)), col="white", lwd=0.15) +
  coord_quickmap() +
  theme_void() +
  scale_fill_manual(values =c("red", "blue")) +
  theme(plot.title = element_text(size = 14, face = "bold", color = "darkgrey",
                                  hjust = 0.5)) +
  labs(title="2012 Senate Election Results", fill = "Party")

#15
# Create a map that shades each state by the Democratic vote difference you created above.
# Again, If there was no Senate contest in a state (or if a party other than Democrats or 
# Republicans won the seat), leave the state blank

ggplot() +
  geom_polygon(data = Question13, aes(x=long, y=lat, group=group, 
                                      fill=demdiff), col="white", lwd=0.15) +
  coord_quickmap() +
  theme_void() +
  scale_fill_gradient(high="blue", low="red") +
  theme(plot.title = element_text(size = 14, face = "bold", color = "purple",
                                  hjust = 0.5)) +
  labs(title="2012 Senate Election Lead by Party", fill = "Party")

















