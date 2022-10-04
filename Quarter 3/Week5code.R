#######################
## HOMEWORK
## WEEK 5
## FEB 11 2022
## ZAI RUTTER
#######################

library(tidyverse)
library(readxl)

#######################

## Question 1

##### A
## Consider a sample of 20 people for a random variable X which is a feeling 
## thermometer for President Biden. The variable can take on values 0-100, where 
## 0 is “cold” feelings and 100 is “hot”. In the particular sample that we 
## received the mean of this variable is x ̄ = 53 and the 
## sample standard deviation sx = 7.

## Perform a hypothesis test for the null hypothesis that Americans on average 
## are ambivalent about Biden, feeling neither warm nor cool on average 
## (i.e. the population mean, μ, equals 50). In this test you should perform each
## of the 5 steps laid out in lecture, and make use of the qt()/pt() functions.

## average = 53, size = 20, SD = 7, 

## 1) Null mu =50 | 1/2

## 2) Alternate is probability that mu not 50

## 3) Reject null if the observed mean is outside 95% CI
## 95% CI is:

qt(.025, 19) 
qt(.975, 19)

50 + ((7/sqrt(20))) * qt(.025, 19) 
50 + ((7/sqrt(20))) * qt(.975, 19) 

## 4) Select Test Regression
# 53-50/semx
# semx == ((7/sqrt(20))
sem<-(7/sqrt(20))

3/sem

(1-pt(1.91663, df=19)) *2

## 5) Reject, fail or accept null
## Accept null

##### B & C
load("~/Documents/Upenn/Data 310/Week 5/Homework/ACSCountyData.Rdata")

summary(lm(median.income ~ percent.college, data =acs))
plot(acs$percent.college, acs$median.income)
abline(lm(median.income ~ percent.college, data =acs))

# The estimated coefficient for pre-college is a one unit increase in percent college the median income is increasing  $1,015 




#What null hypothesis is being tested automatically for percent.college? 
## That there is no cchance in tthier relatioshps 

## What is the alternative hypothesis? What is the result of that hypothesis test?
# There is a relationship

#####  E
## Using the output of the model 
## (i.e. the coefficient, the standard error, and the degrees of freedom) 
## and the qt()/pt() functions, confirm the automatic hypothesis test is correct 
## by completing the same 5-step hypothesis testing sequence you completed in (a).

## 1) Null no relationship ~ 0

## 2) Alternate is probability that not 0 

## 3) Reject null if the observed mean is outside 95% CI
qt(.025, 3217) 
qt(.975, 3217)

0 + 20.51 * qt(.025, 3217) 
0+ 20.51 * qt(.975, 3217) 

## Reject the null

#######################

## Question 2
## A
# Create a new variable ‘x‘ using the command rnorm(100, mean=0,sd=7). 
# This creates a new sample that has an n of 100. Note that the expected value 
## of the population we are drawing from is μ = 0 and a population variance σ2 = 49.

Q2.x<- rnorm(100, mean=0,sd=7)

## B
# Use the t.test function to perform a t.test on this new variable for the null 
# hypothesis that μ = 0. Report the p-value for this hypothesis test. 
# (Note, the defualt for the t.test function is to test whether μ = 0.

t.test(Q2.x)
0.9181

## C
## Next, run a loop that completes this same process 1000 times: sampling from 
## rnorm(100, mean=0, sd=1), performing a t.test, and comptuing the p.value for 
## the null hypothesis that μ = 0. Each time through the loop save the p-value 
## that was calculated. (To do this, I first save the results of the t.test to an 
## object, and then access the p value that’s in that object: t < − t.test(x); t$p.value)

QC<- rep(NULL, length(1000))
for (i in 1:1000) {
  QC.2<-rnorm(100, mean=0,sd=7)
  QC.2T<- t.test(QC.2)
  QC[i]<-QC.2T$p.value
  
}

##D
## How many of the 1000 p-values you calculated were “statistically significant” 
## (i.e. less than .05)? What does this tell you about α, the probability of 
## getting a false positive?

sum(QC < .05)/1000

## This is rejection region 

##E

QE<- rep(NULL, length(1000))
for (i in 1:1000) {
  QE.2<-rnorm(100, mean=2,sd=7)
  QE.2T<- t.test(QE.2)
  QE[i]<-QE.2T$p.value
  
}
t.test(QE)
## F
sum(QE >.05)/1000


# when p value is greater than .05, we aer in the central curve when we know the null is true
# but we do know that null is not true beaause we have the mean of 2
# this result tells us that we have about 200 false positives, ie. 200 times we 
# get a mean that is not 'true'. 

# We want to be careful of claiming causality when there is none- ie claiming a false pos 


not stat signifacnt, b/c we know that the mean is 2 and we are testing that the null of 0, but
204 we get that mean is actually notthe case, teh pvalues are not siginifant. 
The mean is close is zero, we should not reject the null

its better to say there is an effect insatead of missing a true pos.to.env
conseviate 


## G
## Run the loop one more time, but modify the sampling command 
## (rnorm(100,mean = 2,sd=7)) in one way that results in a lower β. 
## Describe the change you made and why it leads to a lower false negative rate.

QG<- rep(NULL, length(1000))
for (i in 1:1000) {
  QG.2<-rnorm(500, mean=2,sd=7)
  QG.2T<- t.test(QG.2)
  QG[i]<-QG.2T$p.value
  
}
sum(QG >.05)/1000

type two correleated with power of the test, to change that more powerful is to either change effect sizse
change sample size 




33 
.05/1000

how many times we didnt get x / 
  
  2D - amoutn times got pvalue not sig / 1000

less than .05/1000 (49 is close) we want alpha  to be small,
  
  
  2f (how many statistic sif)/ 1000

how manh of those create than .05, and (higher than 49..) (close to 200)
  
   

