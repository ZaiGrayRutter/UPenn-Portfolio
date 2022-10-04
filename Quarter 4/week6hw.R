
#-----------------------------------
# IHW week 6
# zai rutter
# April 11 22
#-----------------------------------

library(tm)
library(SnowballC)
library(wordcloud)
library(stringr)
library(readr)


# Create a word cloud with the ANES 2016 open ended question about what 
# respondents LIKE about Donald Trump. 

setwd("~/Documents/Upenn/Data 410/Week 6/Homework")
liketrump <- read_csv("redacted_trump_2016.csv")
liketrump$response<-liketrump$`V161075 - PRE: What is it that R likes about Republican Pres cand`
responseLt<-Corpus(VectorSource(liketrump$response))
responseLt <- tm_map(responseLt, removePunctuation)
responseLt <- tm_map(responseLt, removeWords, c("think","his","says","hes","his",
                                                stopwords('en')))
responseLt <- tm_map(responseLt, stripWhitespace)
responseLtTDM<- TermDocumentMatrix(responseLt)
responseLtmatrix <- as.matrix(responseLtTDM)
responseLtmatrix<-sort(rowSums(responseLtmatrix), decreasing = T) 
responseLtdata <- data.frame(word = names(responseLtmatrix), freq = responseLtmatrix)
responseLtdata<-responseLtdata[-c(5),]
responseLtdata<-responseLtdata[-c(8),]
responseLtdata<-responseLtdata[-c(17),]
pal<-brewer.pal(6,"Dark2")
responseLtdata
wordcloud(responseLtdata$word, freq=responseLtdata$freq, max.words = 100, random.order = F,
          colors = pal)



grep("hes",responseLtdata$word,ignore.case = T,value = T)
grep("\\bdont\\b",responseLtdata$word,ignore.case = T)
head(responseLtdata)


# Create a word cloud with the ANES 2016 open ended question about what 
# respondents DISLIKE about Donald Trump. 
distrump <- read_csv("redacted_trump_2016_dislikes.csv")
distrump$response <- distrump$`V161078 - PRE: What is it that R dislikes about Republican Pres cand`
responseDt<-Corpus(VectorSource(distrump$response))
responseDt <- tm_map(responseDt, removeWords, stopwords("english"))
responseDt <- tm_map(responseDt, removePunctuation)
responseDtTDM<- TermDocumentMatrix(responseDt)
responseDtmatrix <- as.matrix(responseDtTDM)
responseDtmatrix<-sort(rowSums(responseDtmatrix), decreasing = T) 
responseDtdata <- data.frame(word = names(responseDtmatrix), freq = responseDtmatrix)
responseDtdata<- responseDtdata[-c(1,2,5,25,61),]
responseDtdata<- responseDtdata[-c(7),]
wordcloud(responseDtdata$word, freq=responseDtdata$freq, max.words = 100, random.order = F,
          colors = pal)

head(responseDtdata)
responseDtdata<- responseDtdata[-c(1,2,5,25,61),]
grep("\\bhes|his|think|and|just\\b",responseDtdata$word,ignore.case = T)

## Create a word cloud with the ANES 2016 open ended question about what 
## respondents LIKE about Hilary Clinton. 

Clintonlikes<- read_csv("redacted_2016_clinton_likes.csv")
Clintonlikes$response<-Clintonlikes$`V161069 - PRE:  What is it that R likes about Democratic Pres cand`
responseLC<-Corpus(VectorSource(Clintonlikes$response))
responseLC<-tm_map(responseLC, removeWords, stopwords("english"))
responseLC<-tm_map(responseLC, removePunctuation)
responseLCTDM<- TermDocumentMatrix(responseLC)
responseLCMX <- as.matrix(responseLCTDM)
responseLCMX<-sort(rowSums(responseLCMX), decreasing = T) 
responseLCdata <- data.frame(word = names(responseLCMX), freq = responseLCMX)
responseLCdata<-responseLCdata[-c(2,4,5,6,7,12,23,13,8),]
wordcloud(responseLCdata$word, freq=responseLCdata$freq, max.words = 100, random.order = F,
          colors = pal)


grep("\\bthe|her|think|and|just|she|say|too|like|things|that\\b",responseLCdata$word,ignore.case = T)
grep("\\bthe|her|think|and|just|she|say|too|like|things|that\\b",responseLCdata$word,ignore.case = T,
     value = T)
head(responseLCdata)


## Create a word cloud with the ANES 2016 open ended question about what 
## respondents DISLIKE about Hilary Clinton. 

ClintonDis<- read_csv("redacted_clinton_2016_dislikes.csv")
ClintonDis$response<-ClintonDis$`V161072 - PRE: What is it that R dislikes about Democratic Pres cand`
responseDC<-Corpus(VectorSource(ClintonDis$response))
responseDC<-tm_map(responseDC, removeWords, stopwords("english"))
responseDC<-tm_map(responseDC, removePunctuation)
responseDCTDM<- TermDocumentMatrix(responseDC)
responseDCMX <- as.matrix(responseDCTDM)
responseDCMX<-sort(rowSums(responseDCMX), decreasing = T) 
responseDCdata <- data.frame(word = names(responseDCMX), freq = responseDCMX)
responseDCdata<-responseDCdata[-c(3,5,7,8,11,13,15,17,21),]
wordcloud(responseDCdata$word, freq=responseDCdata$freq, max.words = 100, random.order = F,
          colors = pal)

#-----------------------------------
# Analysis
#-----------------------------------

#Compare the "like" word clouds for Trump and Obama (from lecture.) 
# Are there similarities/differences? What conclusions do you draw?

## Obama cloud
obama <- read_csv("~/Documents/Upenn/Data 410/Week 6/Lecture/obama_redacted_final.csv")
reasons <- Corpus(VectorSource(obama$reasontovote))
reasons <- tm_map(reasons, removeWords, stopwords('english'))
reasons <- tm_map(reasons, removePunctuation)
reasonsTDM <- TermDocumentMatrix(reasons) 
reasonsMT <- as.matrix(reasonsTDM) 
reasonsMT <- sort(rowSums(reasonsMT), decreasing = TRUE) 
reasonsDF <- data.frame(word = names(reasonsMT), freq = reasonsMT)
pal <- brewer.pal(6,"Dark2")
wordcloud(reasonsDF$word, freq= reasonsDF$freq, max.words = 100, 
              random.order = FALSE, colors = pal)

wordcloud(responseLtdata$word, freq=responseLtdata$freq, max.words = 100, random.order = F,
          colors = pal)
wordcloud(reasonsDF$word, freq= reasonsDF$freq, max.words = 100, 
          random.order = FALSE, colors = pal)

## Obama seemed to be more about ethos and a humancentric future, focusing on promise and hope
## while Trump has a power centric approach that feels very masculine. Ideals are also present here
## But they seem to be patriotic ideals. Where obama's message did not feel so gendered and
## rooted in patriotism.



# Compare the word clouds for Donald Trump and Hilary Clinton. Comment on your findings. 

# the like word clouds for Trump and Clinton
## The cloud for Hilary speaks really only to expierence and a litle about gender. The word Experience comes up
## as well as experienced and qualified and smart. For Trump we see alot more variety about business,
## economy and ethos.


wordcloud(responseLtdata$word, freq=responseLtdata$freq, max.words = 100, random.order = F,
          colors = pal)
wordcloud(responseLCdata$word, freq=responseLCdata$freq, max.words = 100, random.order = F,
          colors = pal)
## Dislikes
## The dislikes about clinton center about gender and honesty and her emails. Those are the largest
## and most common themes. In both instances, Hilary has a single theme and single idea driving 
## her likes and dislikes where trump is more varied in both.
## Conversly what people liked about Clinton, they did not like about Trump which makes sense. In trump's
## Word cloud 'lack' and 'expeirence' were central. As well as values about how he treats people. With people
## being a frequent word used as well as 'views' 'wall''racism'rude' ect. 
wordcloud(responseDCdata$word, freq=responseDCdata$freq, max.words = 100, random.order = F,
          colors = pal)
wordcloud(responseDtdata$word, freq=responseDtdata$freq, max.words = 100, random.order = F,
          colors = pal)



