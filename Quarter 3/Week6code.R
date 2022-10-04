#######################
## HOMEWORK
## WEEK 6
## FEB 19 2022
## ZAI RUTTER
#######################

library(tidyverse)
library(readxl)
load("~/Documents/Upenn/Data 310/Week 6/Homework/ACSCountyData.Rdata")
acs.w<-acs
acs.w$census.region <- as.factor(acs.w$census.region)
#######################

## Question 1

## A,B,C,D

acs.w$median.income <- acs$median.income/1000

plot(acs.w$median.income, acs.w$percent.child.poverty)
abline(lm(percent.child.poverty ~ median.income, data =acs.w))

# There appears to be a negative relationship, as median income increases, child
# poverty decreases
# R squared - 55% of the variation is explained in the data
p <- ggplot(acs.w, aes(x=median.income, y=percent.child.poverty)) + geom_point() +
  ylim(0,100) +
  labs(x="Median Income (Thousands)", y = "Percent Childen in Poverty") +
  geom_smooth(method = lm, formula = y ~ poly(x, 2), se = FALSE)


## E
## In this new regression with a second order polynomial term, what is the the 
## effect of an additional $1000 in median income when median income is at $30k? 

## What is the the effect of an additional $1000 in median income when median 
## income is at $100k? Does this make theoretical sense?

E<-lm(acs.w$percent.child.poverty ~ poly(acs.w$median.income,2, raw=T), data = acs.w)
coef(E)[2] + 2*(30 * coef(E)[3])


## An additional 1000 dollars in median income relates to a .6% decrease in child poverty
## but at 30k, a thousand dollars relates to a 1.2% decrease in child poverty
## This steepening relationship may be a result of the fact that at poorer levels, more money
## has more impact, where a 1k increase on higher median incomes makes up less a percentage of thier income
## leading to a lesser impact, where it averages out to be a total .6% decrease.



## F
## A possible confounding variable to this relationship is the unemployment rate, 
## which may affect both the median income of a county and the percent of children 
## living in poverty. 
## Use the cor() function to investigate the relationships 
## between median income, unemployment, and child poverty. Based on the pattern of 
## correlations, 

## what is likely to happen to the coefficient on median.income if you add
## unemployment rate to the first regression model (the one without the polynomial terms)?


cor(acs[,c('percent.child.poverty','median.income',
           'unemployment.rate')],use='pairwise.complete.obs')
# Unemployment apeears to have a positive relation with percent in poverty (as unemployment grows
# so does child poverty). Unemployment also has a negative relationship ~ -.5 with median income,
# ( As unemployment increases, median income decreases)

## G
# Run this regression with unemployment rate and median income (no polynomial terms),
# and determine the degree to which the coefficient on median.income changes. 
# Interpret the other coefficients in the model as well, being sure to adjust 
# your language to the fact that there are now multiple indpeendent variables.

summary(lm(percent.child.poverty ~ median.income, data =acs.w))
summary(lm(acs.w$percent.child.poverty ~ acs.w$median.income + acs.w$unemployment.rate, 
           data = acs.w))

G<- lm(acs.w$percent.child.poverty ~ acs.w$median.income + acs.w$unemployment.rate, 
   data = acs.w)

coef(G)

# With the first equation, child poverty had a -.6% realtion with Median income.
# when we add the unemployment rate we see that child poverty had a -.44% realtion with Median income,
# Indicating that holding all things constant when unemployment is considered, child poverty has less
# negative relationship with median income


## H
# Another possible confounding variable is the census region people are living in. 
# For example, living in the south could be associated with both lower average 
# incomes and more child poverty. Create an indicator variable for the 4 census 
# regions (or change the variable into a factor variable) and then 

# re-estimate the regression with median income and unemployment to take into 
# account what region each county is in. Interpret the coefficients from this regression.
table(acs.w$census.region)
summary(lm(acs.w$percent.child.poverty ~ acs.w$median.income + acs.w$unemployment.rate +
           acs.w$census.region, data=acs.w))


# ~64% of the variance can be described in our model
# Median income negatively affects child poverty in the midwest region by -.39%
# Unemployet positivley affects child poverty in the midwest region by 1.19%
# in relation to the midwest, child poverty is postively affected more by all other regions



# When all other factors are constant, child poverty is affected by the mid west region
# by 33%, Child poverty (cp) contineus to be less correlated by increase in median income - 
# as median income increases child poverty decreses, but less than the other regressions we ran.
# All othes constant, child poverty is positively affected by the north east region, but not as so by the south.



# first category becomes baseline. every other cat. cmpares agaisnt the first. 
# in relation to the midwest
# 

             
## I
## It’s possible that the effect of median income is different conditional on 
## whether a county is urban or not. 

## Create an indicator variable for whether a 
## county is urban (population density greater or equal to 1000) or not.

acs.w$urban <- acs.w$population.density >= 1000

## Interact this variable with median income in the regression with unemployment 
## rate and census region indicators. Interpret the coefficients on median income,
## the urban indicator, and the interaction term.


summary(lm(percent.child.poverty ~ median.income *urban + unemployment.rate + 
             census.region, data=acs.w))

## When rual median income has a negative .44 affect on child poverty.
## The 'urbanTrue' coef is not relevant because it is result when meidan income is NULL
# and that makes no sesne. So We use the last coef 'median.income:urbanTRUE' to see 
# the affect when meidan income and urban have affect on child poverty which is a pos .16%








1 + 0.44208

# median income is the diff between urban and rulal


# urban coef is a meaninflu number?


summary(lm(acs.w$percent.child.poverty ~ acs.w$urban + acs.w$median.income + acs.w$unemployment.rate + 
             acs.w$census.region, data=acs.w))
I<-lm(acs.w$median.income ~ acs.w$urban + acs.w$unemployment.rate + 
     acs.w$census.region, data=acs.w)
coef(I)







-0.41252 * 0.91293 


# diff between urban and not urban

1- 0.91293



# When not urban, and in the midwest and all others constant, median income is held at ~59k.

# however running the below equation, I know that this number is a bit off, and potentally, a 
# polynomial could get us closer to the actual number.

mean(acs.w$median.income[acs.w$urban == F & acs.w$census.region == "midwest"], na.rm=T)

# Being urban has a high positive impact on median income. Naturally unemployment rates
# negatively affect median income. Regioanlly, has some impact, where being not urban and 
# being in the northeast and west has positive affect, but in the south has a negative affect.
# the adjusted R squared value is a little low with 39% of variance described in the equation. P values
# are significant.


that percent more pos than not urban.
if you urban, increasing meidan income also increases the effect
income in iurban counties does have sign. effect
median icome in urban countie is sig vs than income from not urban county
