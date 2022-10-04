## Week 7 Homework
# Zai Rutter

########

setwd("~/Documents/Upenn/Data 210/Week 7/homework")
load("~/Documents/Upenn/Data 210/Week 7/homework/felons.RData")

felons.untouched <- felons
felons<- felons.untouched

library(tidyverse)
library(broom)
library(purrr)

###### Question 1

## A
# Imagine you wanted to understand the effect that felony convictions and 
# incarceration have on voter registration and turnout (after the sentence has been served). 
# Somebody suggests that you 

# compare the voter registration rates and turnout rates of former felons to 
# people who never have served time for a felony. 

# Discuss why this research design could or could not help you to get a good 
# estimate of the causal effect that you are interested in.

## Answer
# Comparing turn out rates for felons versus non felons is a bad research design
# since there are serveral factors within turnout that need to be addressed piece by piece. 
# For example, white people (without a felony) may turn out more than another race, yet 
# there may be far less whites with felony. Even with randomization efforts you cannot
# compare these two populations without reckoning with the fact that on average the racial makeup
# of turnout will be different than the racial average of incarcerated people. 
# There could be many factors for example, education, that could impact voter turn out. 
#simply comparing the two against each other will not isolate the issue of felonies, rather 
# It could just exasperate other underlying socio-economic issues relating to voter turn out
# regardless if the demographics are randomized. You cannot make the demographc make up 
# of felons match the demographic make up of voters because this demogrpahic make up 
# will not accuratly respresent the population of felons.

## B
# Read the Experimental Design section of the “Can Incarcerated Felons be 
# (Re)integrated into the Political System?” (pages 915 through 917 in gerber, 
#  et al 2015.pdf ).

#  i. What are the causal effect(s) that the authors are interested in studying? 

# Does formal alerts increase voter parcipation of ex felons?

# ii. Describe the treatment and control conditions in the experiment.

# A subset of felony population in CT were tested, half were given a letter,
# The other half did not get a letter. Then thier voting was monitored. Additionally,
# prior voting particpation was examined to see if a past voting history had any
# indicator on participation

# iii. Describe the randomization strategy that the authors used.
# First they flushed certian felons out of the pool for if they were already registered or
# for other reasons regardign thier crime ect.
# Randomly assaigned ppl to three gropus; control, two different treatment groups, one of which
# recieved each treatment.
# Blocks were made by individual crime type to ensure balance across conditions. 
# leftover cases were randomly assigned. 
# comparisions of blocks showed no imbalances on age, time in prision, time from release
# or previous voting behavior.

# one treatment had standard language about registered, and the other appealed to
# concerns about fair treatment or humuliation at the polls because of thier incarceration
# the ppl in the constant group were not contacted.

## C
# Now we’re going to analyze the results from the experiment. 
# Begin by removing the 161 people in the dataset who returned to prison before 
# the experiment was conducted. 

# Then create a new variable called ‘treatment_collapsed’
# which tells us whether each observation in the data was in the control group 
# (FALSE) or a treatment group (TRUE).

felons<-felons[!(felons$returntoprison == 1),]
felons$treatment_collapsed <- ifelse(felons$treatment >1,
                                      TRUE,
                                      FALSE )
  
## D
# The first thing you should always do before analyzing the results of an 
# experiment is assess whether you have balance in your treatment and control groups. 
# In a well-balanced experiment, no pre-treatment covariates 
# (i.e. the variables that existed before you ran the experiment) would predict 
# whether or not somebody ended up in the treatment or control group.
# For the following questions, use the treatment_collapsed variable.

# i 
# Use 4 t-tests to assess whether the
# felons’ age, number of days served in prison, time since their release from prison, or 2008 vote turnout 
# is a statistically significant predictor of treatment. To do the t-tests, you’ll want to write 
# code that looks like this: t.test(felons$age ~ felons$treatment_collapsed). 
# Create a well-formatted table the present the average values for each of these 
# variables in the treatment and control groups, as well as the the p-value 
# associated with the difference between those averages. 
# You can pull out these values from the output of the t.test() object using the 
# $ operator. Is there significant imbalance for any of those four variables?

# felons’ age, number of days served in prison, time since their 
# release from prison, or 2008 vote turnout 

t.test(felons$age[felons$treatment_collapsed == T],
       felons$age[felons$treatment_collapsed == F])
t.test(felons$days_served[felons$treatment_collapsed == T],
       felons$days_served[felons$treatment_collapsed == F]) 
t.test(felons$yrs_since_release[felons$treatment_collapsed == T],
       felons$yrs_since_release[felons$treatment_collapsed == F])
t.test(felons$vote08[felons$treatment_collapsed == T],
       felons$vote08[felons$treatment_collapsed == F])



# ii
# Use linear regression to assess whether the type of crime predicts whether 
# somebody ended up in the treatment or control group. 
# Were any crimes strong predictors of the treatment?

summary(lm(treatment_collapsed ~ felony_type,
           data = felons))
# No , all of the P value is larger than .05 so there is no signifiant difference

# iii
# Use linear regression to assess balance for all the variables 
# (age, days in prison, time since release, 2008 turnout, crime type) simultaneously. 
# When you do this, do you find imbalance for any of the pre-treatment covariates?

summary(lm(treatment_collapsed ~ age + days_served + yrs_since_release + vote08 + felony_type,
           data = felons))
# No i do not, except that days served P value is .0746 which is much lower than the others,
# Still not below .05.

# D
# Did the experiment have an effect on whether or not ex-felons registered to vote? 
# Did it impact their turnout in 2012? 

# If so, how much did the treatment increase or decrease the probability that 
# they registered or turned out? 

# You can use linear regression and the ‘treatment_collapsed’ variable to answer this question.

summary(lm(treatment_collapsed ~ registered + vote12,
           data = felons))

# It appears that it did indicate that the treatment impacted registration but not voter turnout in 2012

mean(felons$vote12[felons$treatment_collapsed]) - mean(felons$vote12[!felons$treatment_collapsed])
mean(felons$registered[felons$treatment_collapsed]) - mean(felons$registered[!felons$treatment_collapsed])

# E
# Use linear regression to estimate these two treatment effects again. 
# This time, control for the five pre-treatment covariates 
# (the ones you checked for balance in part C in your regression. 
# What effect did the treatment have on registration and voting?



summary(lm(vote12 ~ treatment_collapsed + age + days_served + 
             yrs_since_release + registered + felony_type,
           data = felons))

# Treatment did not have an effect on voting

summary(lm(registered ~ treatment_collapsed + age + days_served + 
             yrs_since_release + felony_type,
           data = felons))

# Treatment did  have an effect on registration



summary(lm(treatment_collapsed ~ vote12 + registered + age + days_served + yrs_since_release + vote08 + felony_type,
           data = felons))




age + days_served + yrs_since_release + vote08 + felony_type



summary(lm(voted ~ treated + race, 
           data = full.experiment[full.experiment$bachelors == 1,])) ## ATE: not significant
summary(lm(voted ~ treated + race, 
           data = full.experiment[full.experiment$bachelors == 0,])) ## ATE: 0.09 percentage points























