## Zai Rutter HW week 6
#Mapping
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
counties <- map_data("county")
states <-map_data("state")
colors <- c("#D56062", "#067BC2")
setwd("~/Documents/Upenn/Data 101/Week 6/Intro to Maps Data")
Census_Poverty <- read.csv(file="Census_Poverty.csv")
data(county.fips)
county_with_fips <- counties %>%
  mutate(polyname = paste(region, subregion, sep=",")) %>%
  left_join(county.fips, by="polyname")
census_income <- read_csv(file="Census_Poverty.csv")

county_with_income <- inner_join(county_with_fips, census_income, 
                                 by=c("fips"="fips_code"))

## Question 1

county_returns <- read.csv(file="county_returns.csv")

# 1.a
#a.	Merge the county-level mapping data with this electoral data. 
#   Think carefully about how we should merge it!

Question1_A <- county_with_income %>%
  full_join(county_returns, by = "fips")

# b. b.	Create new variables: 
# (1) clinton_prop (the proportion of the vote won by Hillary Clinton in the 2016 election), 
# (2) trump_prop (the proportion of the vote won by Donald Trump in the 2016 election), 
# (3) clinton_won (an indicator variable telling you whether Hillary Clinton won a 
#     majority of the votes cast in this county).


Question1_B <- Question1_A %>%
  mutate(clinton_prop = (clinton/total_votes_2016), trump_prop = (trump/total_votes_2016), 
clinton_won = ifelse((clinton/total_votes_2016)>.50,"yes","no"))

# Question 2 - help with colors
# 	Next, make two simple maps that show 
#  (1) counties that Hillary Clinton won in blue, and counties that Donald Trump won in red
#  (2) states that Clinton won in blue and Trump won in red 
#      (so a county-level and state-level electoral choropleth map). You’ll often see these maps on election night, and now you can make them! You will need to do some additional work to get this data at the state level from your county-level data.

ggplot() +
  geom_polygon(data = Question1_B, aes(x=long, y=lat, group=group, 
                                       fill=(clinton_won)), col="dark grey", lwd=0.115) +
  coord_quickmap() +
  theme_void() +
  scale_fill_manual(values =c("red", "blue")) +
  labs(title = "2016 Election Outcomes per County")

States_Election <- county_returns %>%
  group_by(state.name) %>%
  summarise(Clinton_State_Winner = (sum(clinton)/(sum(clinton) + sum(trump)))) %>%
  mutate(State_Winner = ifelse(Clinton_State_Winner>.5,1,0)) %>%
  left_join(states, by=c("state.name" = "region"))

ggplot() +
  geom_polygon(data = States_Election, aes(x=long, y=lat, group=group, 
                                       fill=as.factor(State_Winner)), col="dark grey", lwd=0.115) +
  coord_quickmap() +
  theme_void() +
  scale_fill_manual(values =c("red", "blue"))
  
  
  
  scale_fill_brewer(palette="RdYlGn",
                    name="Percentage of Clinton Votes")


  
  
  scale_fill_manual(values =c("red", "blue")) +
  labs(title = "2016 Election Outcomes per County")


  scale_fill_manual(values =c("red", "blue")) +
  labs(title = "State Outcomes")
  
  
  scale_fill_brewer("red","blue")

?scale_color_brewer
  scale_fill_manual(values =c("red", "blue")) +
  labs(title = "State Outcomes")




total votes 

States_Election <- Question1_B %>%
  
  mutate(region = state) %>%
  left_join(county_returns, by="state")
  
   
  county_returns  %>%
  group_by(states,) %>%
  summarise(State = state.name)

county_returns %>% unite(col=State c(century,year), sep=“”)





ggplot() +
  geom_polygon(data = Question1_B, aes(x=long, y=lat, group=group, 
                                       fill=(clinton_won)), col="dark grey", lwd=0.115) +
  coord_quickmap() +
  theme_void() +
  scale_fill_manual(values =c("red", "blue")) +
  labs(title = "2016 Election Outcomes per County")



# Question 3
# Now make a county-level map that shows the proportion of votes won by Hillary Clinton 
# in the 2016 election. Counties should be colored on a sliding scale, with counties where 
# Clinton won 0% of the vote in the brightest red, and counties where Clinton won 100% 
# of the vote in the brightest blue. Make these counties fade to white in the middle, 
# so that the color fades as the margin of victory decreases. 
# Hint: Check out the scale_fill_gradient2 option in ggplot.

Question3 <- Question1_B %>%
  group_by(fips, long, lat, group) %>%
  summarise(clinton_prop = clinton_prop)
                            

ggplot() +
  geom_polygon(data = Question1_B, aes(x=long, y=lat, group=group, fill=(clinton_prop))) +
  coord_quickmap() +
  theme(legend.position="none") +
  theme_void() +
  scale_fill_gradient2(high = "blue", mid = "white", low = "red", midpoint = .5) +
  ggtitle("Clinton Percentage")



?scale_fill_gradient



?theme
ggplot() +
  geom_polygon(data = Question1_B, aes(x=long, y=lat, group=group, fill=as.factor(clinton_prop))) +
  coord_quickmap() +
  theme(legend.position="none") +
  theme_void() +
  scale_fill_brewer(palette="RdYlGn",
                    name="Percentage of Clinton Votes")

display.brewer.all()

# Question 3 A
# Now draw the same map, except us the more familiar red –> purple –> blue maps 
# that you often see in election analyses. So heavily Trump areas are bright red, 
# politically mixed areas are purple, and heavily Democratic areas are deep blue. 
# Which of these maps do you prefer, and why?


ggplot() +
  geom_polygon(data = Question1_B, aes(x=long, y=lat, group=group, fill=(clinton_prop))) +
  coord_quickmap() +
  theme(legend.position="none") +
  theme_void() +
  scale_fill_gradient2(high = "blue", mid = "purple", low = "red", midpoint = .5) +
  ggtitle("Clinton Percentage")



# Question 4
# Now use the 2012 data we included to calculate the swing from 2012 to 2016. 
# Where did Clinton improve on Obama’s performance, and where did it fall off? 
# Places where Clinton did better than Obama should be more blue and places where 
# she did worse should be more red. Comment on your results and where you see trends. 
# Note: You might see a gray county in South Dakota where the data didn’t map right. 
# Just ignore this, there’s an issue with the underlying data that we won’t worry about here.


Question4 <- Question1_B %>%
  mutate(obama_prop = (obama/total_votes_2012)) %>%
  mutate(presidential_performance = (obama_prop - clinton_prop))

ggplot() +
  geom_polygon(data = Question4, aes(x=long, y=lat, group=group, fill=(presidential_performance))) +
  coord_quickmap() +
  theme(legend.position="none") +
  theme_void() +
  scale_fill_gradient(high = "red", low = "blue") +
  ggtitle("Presidential Performance")






















