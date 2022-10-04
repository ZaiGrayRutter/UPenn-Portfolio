### Homework Week 1
### Jan 13 2022
## Zai Rutter

######################

setwd("~/Documents/Upenn/Data 310/Week 1/Homework")

library(readr)
library(stargazer)
library(scales)
library(tidyverse)
#####################
###### Question 1

### A
AlabamaCourt <- read_csv("AlabamaCourt.csv")
AC<-AlabamaCourt

### B
AC$black <- ifelse(AC$race == "B", 1,0)
AC$amountremain <- (AC$amountdue - AC$amountpaid)

### C

## African-American
  AC.Black<- AC %>%
  filter(black == 1)
  stargazer(as.data.frame(AC.Black[c("amountremain", "amountdue","amountpaid")]),
             type = "text")
## Non-Black
  AC.NonBlack <- AC %>%
    filter(black != 1)
  stargazer(as.data.frame(AC.NonBlack[c("amountremain", "amountdue","amountpaid")]),
            type = "text")  
  
### D
## Create three kernel density plots that compare the distributions 
  ## of the variables 
## amountremain", \amountdue", \amountpaid", 
## respectively, for African-Americans and non-African-Americans. 
  
ggplot(AC, aes(x=amountremain, colour = (black==1))) +
  geom_density() +
  scale_x_continuous(trans = 'log10',
                     breaks=c(1,10,100,1000,10000,100000),
                     labels=comma,name="Money Owed to the State") +
  ylab("Density of Cases") +
  scale_color_discrete(name="Race",
                       labels=c("African-American", "Non African-American"))

ggplot(AC, aes(x=amountpaid, colour = (black==1))) +
  geom_density() +
  scale_x_continuous(trans = 'log10',
                     breaks=c(1,10,100,1000,10000,100000),
                     labels=comma,name="Money Paid to the State") +
  ylab("Density of Cases") +
  scale_color_discrete(name="Race",
                       labels=c("African-American", "Non African-American"))

ggplot(AC, aes(x=amountdue, colour = (black==1))) +
  geom_density() +
  scale_x_continuous(trans = 'log10',
                     breaks=c(1,10,100,1000,10000,100000),
                     labels=comma,name="Money Due to the State") +
  ylab("Density of Cases") +
  scale_color_discrete(name="Race",
                       labels=c("African-American", "Non African-American"))

### E

# In the graph "Paid", it appears that there is significantly higher peak that African Americans
# have paid higher sums. The lines are slighly dissimilar prior to that mark with more non blacks
# have paid slightly less sums. After that $1000 mark African Americans seem to pay more than the rest of the sample
# In the graph "Due" the graphs are generally similar with slight differences. 
# Around $1000 non-blacks have been charged more where as Afrian Americans have more been charged around the
# 5,000 mark.
# In the graph "owed" non blacks owe more than African Americans.

# According to these numbers it appears that while non-blacks owe more, they have also paid less.
# The most telling graph would "Money due" where there is a slight difference in amount charged to African Americans.
# I think more data about income disparity could build a more telling story about racist policies in LFOs.

###### Question 2
CollegeBasketball <- read_csv("CollegeBasketball.csv")
basketball<-CollegeBasketball

### A

basketball$super <- (basketball$PredictedDifference - basketball$ActualDifference)
mean(abs(basketball$super))
abs(mean(basketball$super))

ggplot(basketball, aes(x=super)) +
  geom_histogram()

### B

basketball$Differential <- rep(NA, length(basketball$Favorite))

for(i in 1:length(basketball$Differential)){
  
  basketball$Differential[i] <-if(basketball$super[i] < 0 ) {
    "W"
  } else if (basketball$super[i] == 0)  {
    "E"
  } else if (basketball$super[i] > 0 )  {
    "L"
  } else {}
  
}

mean(basketball$Differential=="W")
mean(basketball$Differential=="E")
mean(basketball$Differential=="L")

### C
basketball$PointDiffer <- (basketball$PredictedPoints - basketball$ActualPoints)

### D

basketball$ptsdummy <- rep(NA, length(basketball$Favorite))

for(i in 1:length(basketball$ptsdummy)){
  
  basketball$ptsdummy[i] <- if(basketball$PointDiffer[i] == 0 ) {
    "T"
  } else if(basketball$PointDiffer[i] > 0 )  {
    "F"
  } 
  else if(basketball$PointDiffer[i] < 0) {
    "M"
  }
  else {}
}

mean(basketball$ptsdummy=="F")
mean(basketball$ptsdummy=="T")
mean(basketball$ptsdummy=="M")

mean(basketball$PointDiffer == 0)

### E

# Prob W | M
# the prob. that fav won by more points than expected *when* More pionts were scored than expected
mean(basketball$ptsdummy=="M" & basketball$Differential=="W")/mean(basketball$ptsdummy=="M")

# Prob L | M
# Fav earned less points than expected *when* more points were scored than expected
mean(basketball$ptsdummy=="M" & basketball$Differential=="L")/ mean(basketball$ptsdummy=="M")

# Prob W | F
# the prob. that fav won by more points than expected *when* less points were scored than expected

mean( basketball$ptsdummy=="F"& basketball$Differential=="W") / mean(basketball$Differential=="W")

# Prob L | F
# Fav earned less points than expected *when* fav won by more points than expected

mean(basketball$ptsdummy=="F" & basketball$Differential=="L") / mean(basketball$ptsdummy=="F")

### F
# Write a paragraph or two, in which you make conclusions about whether the 
# evidence is consistent with my theory based on the data being summarized in 
# the previous parts of this question.

# There is some consistency, as shown below by the numbers.

# Fav get more pionts when less combined points
0.6363636

# the prob. that fav won by more points than expected *when* More pionts were scored than expected
0.4301075

# Fav earned less points than expected *when* fav won by more points than expected
0.4057971



# W = Fav won by more points than expected

# E = Fav won by exact points they were expected

# L = Fav earned fewer points than expected

# M = More combined points were scored than expected

# T = combined points were exact as expected

# F = Less combined pionts were scored than expected







## unerdogs will preform better when the expected score is lower than the one they actually get
## underdogs will preform better when thye outpreform


mean(basketball$Differential=="W" & )


basketball$W & basketball$M

mean(basketball$Differential=="W")


mean(basketball$)
mean(MinterW)/m


prop.table(table((basketball$PredictedPoints < basketball$ActualPoints) ==
                   (basketball$PredictedDifference < basketball$ActualDifference))) # intersection

(107/241)/0.3858921

Wtable <- (basketball$PredictedDifference < basketball$ActualDifference) 
Mtable <- (basketball$PredictedPoints < basketball$ActualPoints) # M

MinterW <- Mtable==Wtable






(m*w)/m


basketball$index <- seq.int(1:nrow(basketball))

basketball %>%
  filter(basketball$PredictedDifference < basketball$ActualDifference) %>%
  
basketball[basketball$PredictedDifference < basketball$ActualDifference]


basketball$Dummy <- rep(NA, nrow(basketball))

for (i in 1:length(basketball$Dummy)){
  basketball$Dummy[i] <- if(
    basketball$PredictedPoints[i] < basketball$ActualPoints[i] &
    (basketball$PredictedDifference[i] < basketball$ActualDifference[i])){
      "intersection"
    }
    else{}
}



prop.table(table((basketball$PredictedPoints < basketball$ActualPoints) ==
  (basketball$PredictedDifference < basketball$ActualDifference)))

table(basketball$PredictedPoints < basketball$ActualPoints)
table(basketball$PredictedDifference < basketball$ActualDifference)


