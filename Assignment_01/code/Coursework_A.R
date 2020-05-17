
#[Appendix A-Source Code]
# CS4125 Seminar Research Methodology for Data Science
# Coursework assignment A - Part 2, Question 1 - Twitter sentiment analysis
# 2017
#
# This code requires the following file:
# sentiment3.R, negative-words.txt, and positive-words.txt.
#
#
# this is based on youtube https://youtu.be/adIvt_luO1o
# also see
https://silviaplanella.wordpress.com/2014/12/31/sentiment-analysis-twitter-and-r/
############################################################################################
setwd("C:\\Users\\pc1\\Desktop\\Seminar")
# apple , note use / instead of \, which used by windows
#install.packages("twitteR", dependencies = TRUE)
library(twitteR)
#install.packages("RCurl", dependencies = T)
library(RCurl)
#install.packages("bitops", dependencies = T)
library(bitops)
#install.packages("plyr", dependencies = T)
library(plyr)
#install.packages(’stringr’, dependencies = T)
library(stringr)
#install.packages("NLP", dependencies = T)
library(NLP)
library(openssl)
library(httpuv)
#install.packages("tm", dependencies = T)
library(tm)
#install.packages("wordcloud", dependencies=T)
#install.packages("RColorBrewer", dependencies=TRUE)
library(RColorBrewer)
library(wordcloud)
#install.packages("reshape", dependencies=T)
library(reshape)
################### functions
clearTweets <- function(tweets, excl) {
tweets.text <- sapply(tweets, function(t)t$getText()) #get text out of tweets
tweets.text = gsub(’[[:cntrl:]]’, ’’, tweets.text)
tweets.text = gsub(’\\d+’, ’’, tweets.text)
tweets.text <- str_replace_all(tweets.text,"[^[:graph:]]", " ") #remove graphic
corpus <- Corpus(VectorSource(tweets.text))
corpus_clean <- tm_map(corpus, removePunctuation)
corpus_clean <- tm_map(corpus_clean, content_transformer(tolower))
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords("english"))

corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)
corpus_clean <- tm_map(corpus_clean, removeWords, c(excl,"http","https","httpst"))
return(corpus_clean)
}
## capture all the output to a file.
sink("output.txt")
################# Collect from Twitter
# for creating a twitter app (apps.twitter.com) see youtube https://youtu.be/lT4Kosc_ers
#consumer_key <-’your key’
#consumer_scret <- ’your secret’
#access_token <- ’your access token’
#access_scret <- ’your access scret’
source("your_twitter.R") #this file will set my personal variables for my twitter app,
adjust the name of this file. use the provide template your_twitter.R
setup_twitter_oauth(consumer_key,consumer_scret, access_token,access_scret) #connect to
twitter app
##### This example uses the following 3 celebrities: Donald Trump, Hillary Clinton, and
Bernie Sanders
## You should replace this with your own celebrities, at least 3, but more preferred
## Note that it will take the computer some to collect the tweets
tweets_T <- searchTwitter("#trump", n=800, lang="en", resultType="recent") #1000 recent
tweets about Donald Trump, in English (I think that 1500 tweets is max)
tweets_C <- searchTwitter("#BarackObama", n=800, lang="en", resultType="recent") #1000
recent tweets about Hillary Clinton
tweets_B <- searchTwitter("#hillary", n=800, lang="en", resultType="recent") #1000 recent
tweets about Bernie Sanders
######################## WordCloud
### This not requires in the assignment, but still fun to do
# based on https://youtu.be/JoArGkOpeU0
corpus_T<-clearTweets(tweets_T,
c("trump","amp","realdonaldtrump","trumptrain","donald","trumps","alwaystrump"))
#remove also some campain slogans
wordcloud(corpus_T, max.words=50)
corpus_C<-clearTweets(tweets_C, c("Obama","BarackObama","obamalegacy","44thPresident"))
wordcloud(corpus_C, max.words=50)
corpus_B<-clearTweets(tweets_B, c("hillary","amp","clinton","hillarys"))
wordcloud(corpus_B, max.words=50)
##############################
######################## Sentiment analysis
tweets_T.text <- laply(tweets_T, function(t)t$getText()) #get text out of tweets
tweets_C.text <- laply(tweets_C, function(t)t$getText()) #get text out of tweets
tweets_B.text <- laply(tweets_B, function(t)t$getText()) #get text out of tweets
#taken from https://github.com/mjhea0/twitter-sentiment-analysis
pos <- scan(’positive-words.txt’, what = ’character’, comment.char=’;’) #read the
positive words
neg <- scan(’negative-words.txt’, what = ’character’, comment.char=’;’) #read the
negative words
source("sentiment3.R") #load algoritm
# see sentiment3.R form more information about sentiment analysis. It assigns a intereger
score
# by substracitng the number of occurrence of negative words from that of positive words
analysis_T <- score.sentiment(tweets_T.text, pos, neg)
analysis_C <- score.sentiment(tweets_C.text, pos, neg)
analysis_B <- score.sentiment(tweets_B.text, pos, neg)
sem<-data.frame(analysis_T$score, analysis_C$score, analysis_B$score)
semFrame <-melt(sem, measured=c(analysis_T.score,analysis_C.score, analysis_B.score ))
names(semFrame) <- c("Candidate", "score")
semFrame$Candidate <-factor(semFrame$Candidate, labels=c("Donald Trump", "Barack Obama",
"Hillary Clinton")) # change the labels for your celibrities
################## Below insert your own code to answer question 1. The data you need can
be found in semFrame
#Question1.2 Homogeneity of Variance
install.packages(’lawstat’)
library(car)
leveneTest(semFrame$score, semFrame$Candidate, center = median)
bartlett.test(semFrame$score, semFrame$Candidate, center=median)
#leveneTest(semFrame$score, semFrame$Candidate=="Donald Trump", center=median)
#leveneTest(semFrame$score, semFrame$Candidate=="Barack Obama", center=median)
#leveneTest(semFrame$score, semFrame$Candidate=="Hillary Clinton", center=median)
#bartlett.test(semFrame$score, semFrame$Candidate=="Donald Trump", center=median)
#bartlett.test(semFrame$score, semFrame$Candidate=="Barack Obama", center=median)
#bartlett.test(semFrame$score, semFrame$Candidate=="Hillary Clinton", center=median)
#Question1.3
newdataTrump <- subset(semFrame$score, semFrame$Candidate=="Donald Trump")
newdataObama <- subset(semFrame$score, semFrame$Candidate=="Barack Obama")
newdataClinton <- subset(semFrame$score, semFrame$Candidate=="Hillary Clinton")
# plot the results histogram
hist(newdataTrump)
hist(newdataObama)
hist(newdataClinton)
# plot the results density plot
d <- density(newdataTrump) # returns the density data
plot(d , main="Tweets sentiments for Trump")
d <- density(newdataObama) # returns the density data
plot(d,main="Tweets sentiments for Obama")
d <- density(newdataClinton) # returns the density data
plot(d,main="Tweets sentiments for Hillary")
#Question1.4
meanScoreCancidate= tapply(semFrame$score, semFrame$Candidate, mean)
barplot((meanScoreCancidate),
main="Mean Sentiment of Tweets for Celebrities",
xlab="Celebrities",
ylab="MeanScore",
border="black",
col="darkred",
density=10)
hist(newdataTrump,prob=T )
m<-mean(newdataTrump);std<-sqrt(var(newdataTrump))
hist(newdataTrump,prob=T )
curve(dnorm(x, mean=m, sd=std),lwd=2, add=TRUE)
hist(newdataObama,prob=T )
m<-mean(newdataObama);std<-sqrt(var(newdataObama))
hist(newdataObama,prob=T )
curve(dnorm(x, mean=m, sd=std),lwd=2, add=TRUE)
hist(newdataClinton,prob=T )
m<-mean(newdataClinton);std<-sqrt(var(newdataClinton))
hist(newdataClinton,prob=T )
curve(dnorm(x, mean=m, sd=std),lwd=2, add=TRUE)
boxplot(semFrame$score~semFrame$Candidate)
#boxplot(semFrame$score~semFrame$Candidate=="Donald Trump")
#boxplot(semFrame$score~semFrame$Candidate=="Barack Obama")
#boxplot(semFrame$score~semFrame$Candidate=="Hillary Clinton")
#Question1.5
#model without predictor
model0<- lm(score ~ 1, data = semFrame)
#model with Candidate predictor
model1 <- lm(score ~ Candidate, data = semFrame)
anova(model0,model1, test = "F")
summary(model1)
#Question1.6
pairwise.t.test(semFrame$score, semFrame$Candidate, paired = FALSE, p.adjust.method =
"bonferroni")
######### stop redireting output.
####################Question 2 Website Visits(between Groups- Two
Factors)########################################################################################################################Question2.2
library(Rcmdr)
library(foreign)
library(ggplot2)
#read from the webvisit0.csv
WebVisit<-read.csv("webvisit1.csv", header = TRUE)
#install.packages("sm")
library(sm)
hist(WebVisit$pages, main="Page Visit Frequency", xlab =’PagesVisited’)
hist(WebVisit$pages,prob=T )
m<-mean(WebVisit$pages);std<-sqrt(var(WebVisit$pages))
hist(WebVisit$pages,prob=T )
curve(dnorm(x, mean=m, sd=std),lwd=2, add=TRUE)
counter <- table(WebVisit$portal, WebVisit$pages)
barplot(counter, main="Portal Type-PagesVisited",
xlab="PagesVisited", col=c("blue","orange"),
legend = rownames(counter))
counter <- table(WebVisit$version, WebVisit$pages)
barplot(counter, main="Version Type-PagesVisited",
xlab="PagesVisited", col=c("purple","darkgreen"),
legend = rownames(counter))
#Question2.3
hist(WebVisit$pages, main="Histogram of Pages Visited",xlab = "Pages Visited", col="blue")
#Question2.4
#adding two factors version and portal
WebVisit$version <- factor(WebVisit$version)
WebVisit$portal <- factor(WebVisit$portal)
model0 = glm(WebVisit$pages~1, data = WebVisit, family = "poisson")
model1 = glm(WebVisit$pages~version, data = WebVisit, family = "poisson")
model2 = glm(WebVisit$pages~portal, data = WebVisit, family = "poisson")
model3 = glm(WebVisit$pages~version+portal, data = WebVisit, family = "poisson")
model4 = glm(WebVisit$pages~version+portal+version:portal, data = WebVisit, family =
"poisson")
anova(model0,model1, test="Chisq")
anova(model0,model2, test="Chisq")
anova(model0,model3, test="Chisq")
anova(model0,model4, test="Chisq")
summary(model4)
anova(model4,test="Chisq")
#Question2.5
########version on consumers and companies portal
factors <- interaction(WebVisit$version, WebVisit$portal)
levels(factors)
ver_consumers <-c(1,-1,0 , 0) #test version with consumers portal
ver_comp <-c(0, 0, 1, -1) #test version with companies portal
SimpleEff <- cbind(ver_consumers,ver_comp)
SimpleEff
contrasts(factors) <- SimpleEff
contrasts(factors)
simpleEffectModel <-lm(WebVisit$pages ~ factors , data = WebVisit, na.action = na.exclude)
summary.lm(simpleEffectModel)
########effect of portal in the old and mew version
factors <- interaction(WebVisit$version, WebVisit$portal)
levels(factors)
new_ver <-c(0,-1,0 , 1) #portal new version
old_ver <-c(-1, 0, 1, 0) #portal old version
SimpleEff <- cbind(new_ver,old_ver)
SimpleEff
contrasts(factors) <- SimpleEff
contrasts(factors)
simpleEffectModel <-lm(WebVisit$pages ~ factors , data = WebVisit, na.action = na.exclude)
summary.lm(simpleEffectModel)
#Part 2: Question 3: Linear regression
analysis#####################################################################################
#######################################
#load libraries
library(Rcmdr)
library(foreign)
library(ggplot2)
library(lattice)
library(QuantPsyc)
library(car)
#Measurements for 158 cruise ships
colnames = c("Ship_Name", "Cruise_Line", "Age",
"Tonnage", "Passengers", "Length",
"Cabins","Passengers_Density","Crew")
#Import the dataset
Cruise_Ship <- read.fwf(
file=url("http://www.stat.ufl.edu/~winner/data/cruise_ship.dat"),
widths=c(20, 20, 9, 8, 8, 8, 8, 9, 9 ),
col.names = colnames)
#2)Graphical analysis of the distribution of the dependent variable
#Histogram of number of Passengers (100s)
hist(Cruise_Ship$Passengers, main="Histogram of number of passengers (100s)", col =
"blue",xlab = "Number of passengers (100s)")
#Density plot of number of Passengers (100s)
density_passengers <-density(Cruise_Ship$Passengers)
plot(density_passengers , main = "Density plot of number of Passengers (100s)", col =
"blue", xlab = "Number of passengers (100s)")
#computation of the mean and the standar deviation of the number of Passengers (100s)
mean(Cruise_Ship$Passengers)
sd(Cruise_Ship$Passengers)
#3)Scatter plots between dependent variable and the predictor variables
#Scatter plot between Number of Passengers(100s)-Length of the ship(100s of feat)
xyplot(Cruise_Ship$Passengers ~ Cruise_Ship$Length, data = Cruise_Ship,
xlab = "Length of the ship (100s of feat)",
ylab = "Number of Passengers (100s)",
main = "Number of Passengers(100s)-Length of the ship(100s of feat)"
)
#Scatter plot between Number of Passengers(100s)-Number of Cabins(100s) of ship
xyplot(Cruise_Ship$Passengers ~ Cruise_Ship$Cabins, data = Cruise_Ship,
xlab = "Number of Cabins(100s) of ship",
ylab = "Number of Passengers (100s)",
main = "Number of Passengers(100s)-Number of Cabins(100s)of ship"
)
#Scatter plot between Number of Passengers(100s)-Tonnage (1000s of tons) of ship
xyplot(Cruise_Ship$Passengers ~ Cruise_Ship$Tonnage, data = Cruise_Ship,
xlab = "Tonnage (1000s of tons) of ship",
ylab = "Number of Passengers (100s)",
main = "Number of Passengers(100s)-Tonnage(1000s of tons) of ship"
)
#4)Multiple linear regression
#creation of 7 models
model0<-lm(Cruise_Ship$Passengers~1, data=Cruise_Ship)
model1<-lm(Cruise_Ship$Passengers~Cruise_Ship$Length, data=Cruise_Ship)
model2<-lm(Cruise_Ship$Passengers~Cruise_Ship$Tonnage, data=Cruise_Ship)
model3<-lm(Cruise_Ship$Passengers~Cruise_Ship$Cabins, data=Cruise_Ship)
model4<-lm(Cruise_Ship$Passengers~Cruise_Ship$Length+Cruise_Ship$Cabins, data=Cruise_Ship)
model5<-lm(Cruise_Ship$Passengers~Cruise_Ship$Cabins+Cruise_Ship$Tonnage,
data=Cruise_Ship)
model6<-lm(Cruise_Ship$Passengers~Cruise_Ship$Length+Cruise_Ship$Tonnage,
data=Cruise_Ship)
#creation of model7 that has all the three above predictors length, cabins and tonnage
fit<-lm( Cruise_Ship$Passengers ~ Cruise_Ship$Length + Cruise_Ship$Cabins +
Cruise_Ship$Tonnage, data = Cruise_Ship)
#comparison of the models using ANOVA function
anova(model0,model1,model2,model3,model4,model5,model6,fit)
#summary of the fit model
summary(fit)
#Determination of the confidence intervals (95%)
confint(fit)
#Computation of the the beta values (standardised regression coefficients):
lm.beta(fit)
#5)Examination of assumptions underlying linear regression
#checking the collinearity assumption
vif(fit)
#checking the normality assumption of residuals
shipres = residuals(fit)#
qqnorm(shipres, main="Normality assumption of Residuals")
qqline(shipres)
hist(shipres,main="Histogram of Residuals", xlab = "Residuals")
#checking the linearty assumption
qqmath( ~ resid(fit),
xlab = "Theoretical Quantiles",
ylab = "Residuals"
)
#checking the homogeneity of variance with levene test
leveneTest(Cruise_Ship$Passengers~ factor(Cruise_Ship$Length)*factor(Cruise_Ship$Cabins),
center = mean)
leveneTest(Cruise_Ship$Passengers, factor(Cruise_Ship$Cabins), center = mean)
leveneTest(Cruise_Ship$Passengers, factor(Cruise_Ship$Length), center = mean)
leveneTest(Cruise_Ship$Passengers, factor(Cruise_Ship$Tonnage), center = mean)
#6)Examination of the effect of single cases on the predicted values
#application of Cooks’ distance
plot(cooks.distance(fit),ylab="Cooks distances",xlab="Observation number")
#application of DFBeta
dfbetaPlots(fit)
###################################################Question 4 Logistic
Regression######################################
library(foreign)
library(car) #Package includes Levene’s test
library(tidyr) # for wide to long format transformation of the data
library(ggplot2)
library(QuantPsyc) #include lm.beta()
library(class)
library(pscl)
library(gmodels)
columns = c("PH","NisinConcentration","Temoerature","BrixConcetration",
"Growth"
)
#reading the dataset
CRAinAppleJuice <- read.fwf(
file=url("http://www.stat.ufl.edu/~winner/data/apple_juice.dat"),
widths=c(9, 9, 9, 8, 8),
col.names = columns)
model0 <- glm(CRAinAppleJuice$Growth ~ 1, data=CRAinAppleJuice, family=binomial())
model1 <- glm(CRAinAppleJuice$Growth ~ PH, data=CRAinAppleJuice, family=binomial())
#model2 <- glm(CRAinAppleJuice$Growth ~NisinConcentration , data=CRAinAppleJuice,
family=binomial())
model2<-glm(CRAinAppleJuice$Growth ~ PH + NisinConcentration, data=CRAinAppleJuice,
family=binomial())
anova(model0,model1,model2,test = "Chisq")
summary(model2)
##Pseudo R2
logisticPseudoR2s <- function(LogModel)
#taken from Andy Fields et al. book on R, p.334
{
dev <- LogModel$deviance
nullDev <- LogModel$null.deviance
modelN <- length(LogModel$fitted.values)
R.l <- 1 - dev / nullDev
R.cs <- 1 - exp(-(nullDev - dev) / modelN)
R.n <- R.cs / (1 - (exp(-(nullDev / modelN))))
cat("Pseudo R^2 for logistic regression\n")
cat("Hosmer and Lemshow R^2: ", round(R.l, 3), "\n")
cat("Cox and Snell R^2: ", round(R.cs, 3), "\n")
cat("Nagelkerke R^2: ", round(R.n, 3), "\n")
}
logisticPseudoR2s(model2)
##Odd and conf
exp(model2$coefficients)
exp(confint(model2))
##crosstable
CRAinAppleJuice$Growpred[fitted(model2) <=0.5] <- 0
CRAinAppleJuice$Growpred[fitted(model2) > 0.5] <- 1
CRAinAppleJuice$Growpred<-factor(CRAinAppleJuice$Growpred, levels = c(0:1), labels =
c("Absence","Presence"))
table(CRAinAppleJuice$Growth, CRAinAppleJuice$Growpred)
CrossTable(CRAinAppleJuice$Growpred, CRAinAppleJuice$Growth, prop.c=FALSE, prop.t=FALSE,
prop.chisq=FALSE, fisher=FALSE, chisq=FALSE, expected = FALSE)
##########################################################Part
3#####################################
###################################################################################################
library(MASS)
library(foreign)
library(car)
library(ggplot2)
library(nlme)
library(reshape)
library(graphics)
require(lattice)
library(lattice)
require(Matrix)
library(lme4)
#question 1#
set1 <- read.csv2("set1.csv", header = TRUE, sep = ",",
dec = ",", fill = TRUE, comment.char = "")
hist(set1$score,col="lightblue",main="Score distribution",xlab="score")
boxplot(set1$score ~ set1$session , xlab="Session", ylab="Score", main="Session related
to score",col="green",outcol="red")
#question 2
##We present thr intercdept only model which violates the independent assumption. It will
only allow us to comparre its results with multilevel analysis.
interceptOnly <- gls(score ~ 1, data = set1, method = "ML")
summary(interceptOnly)
randomInterceptOnly <- lme(score ~ 1, data = set1, random = ~1|Subject, method = "ML")
summary(randomInterceptOnly)
#adding session to our model
sessionMod<-lme(score ~(1+ session), data = set1, random = ~1|Subject, method = "ML")
summary(sessionMod)
##compute confidence intervals
intervals(sessionMod, 0.95)
sink()
[Appendix B-Code with Output]
####################Question 2 Website Visits(between Groups- Two
Factors)#######################################################################################################################> #Question2.2
> library(Rcmdr)
Loading required package: splines
Loading required package: RcmdrMisc
Loading required package: car
Loading required package: sandwich
Loading required package: effects
Loading required package: carData
Guyer, UN, Vocab
lattice theme set by effectsTheme()
See ?effectsTheme for details.
RcmdrMsg: [1] NOTE: R Commander Version 2.4-1: Mon Mar 19 16:12:59 2018
Rcmdr Version 2.4-1
> library(foreign)
> library(ggplot2)
Attaching package: ’ggplot2’
The following object is masked from ’package:NLP’:
annotate
> #read from the webvisit0.csv
> WebVisit<-read.csv("webvisit1.csv", header = TRUE)
> #install.packages("sm")
> library(sm)
Package ’sm’, version 2.2-5.4: type help(sm) for summary information
> hist(WebVisit$pages, main="Page Visit Frequency", xlab =’PagesVisited’)
> hist(WebVisit$pages,prob=T )
> m<-mean(WebVisit$pages);std<-sqrt(var(WebVisit$pages))
> hist(WebVisit$pages,prob=T )
> curve(dnorm(x, mean=m, sd=std),lwd=2, add=TRUE)
> counter <- table(WebVisit$portal, WebVisit$pages)
> barplot(counter, main="Portal Type-PagesVisited",
+ xlab="PagesVisited", col=c("blue","orange"),
+ legend = rownames(counter))
> counter <- table(WebVisit$version, WebVisit$pages)
> barplot(counter, main="Version Type-PagesVisited",
+ xlab="PagesVisited", col=c("purple","darkgreen"),
+ legend = rownames(counter))
> #Question2.3
> hist(WebVisit$pages, main="Histogram of Pages Visited",xlab = "Pages Visited",
col="blue")
> #Question2.4
> #adding two factors version and portal
> WebVisit$version <- factor(WebVisit$version)
> WebVisit$portal <- factor(WebVisit$portal)
> model0 = glm(WebVisit$pages~1, data = WebVisit, family = "poisson")
> model1 = glm(WebVisit$pages~version, data = WebVisit, family = "poisson")
> model2 = glm(WebVisit$pages~portal, data = WebVisit, family = "poisson")
> model3 = glm(WebVisit$pages~version+portal, data = WebVisit, family = "poisson")
> model4 = glm(WebVisit$pages~version+portal+version:portal, data = WebVisit, family =
"poisson")
> anova(model0,model1, test="Chisq")
Analysis of Deviance Table
Model 1: WebVisit$pages ~ 1
Model 2: WebVisit$pages ~ version
Resid. Df Resid. Dev Df Deviance Pr(>Chi)
1 998 1067.0
2 997 1032.8 1 34.249 0.00000000485 ***
---
Signif. codes: 0 ’***’ 0.001 ’**’ 0.01 ’*’ 0.05 ’.’ 0.1 ’ ’ 1
> anova(model0,model2, test="Chisq")
Analysis of Deviance Table
Model 1: WebVisit$pages ~ 1
Model 2: WebVisit$pages ~ portal
Resid. Df Resid. Dev Df Deviance Pr(>Chi)
1 998 1067.00
2 997 898.85 1 168.16 < 2.2e-16 ***
---
Signif. codes: 0 ’***’ 0.001 ’**’ 0.01 ’*’ 0.05 ’.’ 0.1 ’ ’ 1
> anova(model0,model3, test="Chisq")
Analysis of Deviance Table
Model 1: WebVisit$pages ~ 1
Model 2: WebVisit$pages ~ version + portal
Resid. Df Resid. Dev Df Deviance Pr(>Chi)
1 998 1067.00
2 996 861.98 2 205.02 < 2.2e-16 ***
---
Signif. codes: 0 ’***’ 0.001 ’**’ 0.01 ’*’ 0.05 ’.’ 0.1 ’ ’ 1
> anova(model0,model4, test="Chisq")
Analysis of Deviance Table
Model 1: WebVisit$pages ~ 1
Model 2: WebVisit$pages ~ version + portal + version:portal
Resid. Df Resid. Dev Df Deviance Pr(>Chi)
1 998 1067.00
2 995 833.97 3 233.03 < 2.2e-16 ***
---
Signif. codes: 0 ’***’ 0.001 ’**’ 0.01 ’*’ 0.05 ’.’ 0.1 ’ ’ 1
> summary(model4)
Call:
glm(formula = WebVisit$pages ~ version + portal + version:portal,
family = "poisson", data = WebVisit)
Deviance Residuals:
Min 1Q Median 3Q Max
-1.8100 -0.7783 -0.4582 0.4642 5.5300
Coefficients:
Estimate Std. Error z value Pr(>|z|)
(Intercept) 0.68916 0.04472 15.410 < 2e-16 ***
version[T.1] 0.03018 0.06315 0.478 0.633
portal[T.1] 0.70524 0.05485 12.859 < 2e-16 ***
version[T.1]:portal[T.1] -0.42399 0.08017 -5.289 0.000000123 ***
---
Signif. codes: 0 ’***’ 0.001 ’**’ 0.01 ’*’ 0.05 ’.’ 0.1 ’ ’ 1
(Dispersion parameter for poisson family taken to be 1)
Null deviance: 1067.00 on 998 degrees of freedom
Residual deviance: 833.97 on 995 degrees of freedom
AIC: 3553.5
Number of Fisher Scoring iterations: 5
> anova(model4,test="Chisq")
Analysis of Deviance Table
Model: poisson, link: log
Response: WebVisit$pages
Terms added sequentially (first to last)
Df Deviance Resid. Df Resid. Dev Pr(>Chi)
NULL 998 1067.00
version 1 34.249 997 1032.75 0.00000000485 ***
portal 1 170.773 996 861.98 < 2.2e-16 ***
version:portal 1 28.013 995 833.97 0.00000012050 ***
---
Signif. codes: 0 ’***’ 0.001 ’**’ 0.01 ’*’ 0.05 ’.’ 0.1 ’ ’ 1
> ########version on consumers and companies portal
> factors <- interaction(WebVisit$version, WebVisit$portal)
> levels(factors)
[1] "0.0" "1.0" "0.1" "1.1"
> ver_consumers <-c(1,-1,0 , 0) #test version with consumers portal
> ver_comp <-c(0, 0, 1, -1) #test version with companies portal
> SimpleEff <- cbind(ver_consumers,ver_comp)
> SimpleEff
ver_consumers ver_comp
[1,] 1 0
[2,] -1 0
[3,] 0 1
[4,] 0 -1
> contrasts(factors) <- SimpleEff
> contrasts(factors)
ver_consumers ver_comp
0.0 1 0 -0.5
1.0 -1 0 -0.5
0.1 0 1 0.5
1.1 0 -1 0.5
> simpleEffectModel <-lm(WebVisit$pages ~ factors , data = WebVisit, na.action =
na.exclude)
> summary.lm(simpleEffectModel)
Call:
lm(formula = WebVisit$pages ~ factors, data = WebVisit, na.action = na.exclude)
Residuals:
Min 1Q Median 3Q Max
-3.0325 -1.0325 -0.7198 0.9469 12.0080
Coefficients:
Estimate Std. Error t value Pr(>|t|)
(Intercept) 2.69936 0.05050 53.455 <2e-16 ***
factorsver_consumers -0.03051 0.07166 -0.426 0.67
factorsver_comp 0.65634 0.07117 9.222 <2e-16 ***
factors 1.35364 0.10100 13.403 <2e-16 ***
---
Signif. codes: 0 ’***’ 0.001 ’**’ 0.01 ’*’ 0.05 ’.’ 0.1 ’ ’ 1
Residual standard error: 1.596 on 995 degrees of freedom
Multiple R-squared: 0.2079, Adjusted R-squared: 0.2056
F-statistic: 87.08 on 3 and 995 DF, p-value: < 2.2e-16
> ########effect of portal in the old and mew version
> factors <- interaction(WebVisit$version, WebVisit$portal)
> levels(factors)
[1] "0.0" "1.0" "0.1" "1.1"
> new_ver <-c(0,-1,0 , 1) #portal new version
> old_ver <-c(-1, 0, 1, 0) #portal old version
> SimpleEff <- cbind(new_ver,old_ver)
> SimpleEff
new_ver old_ver
[1,] 0 -1
[2,] -1 0
[3,] 0 1
[4,] 1 0
> contrasts(factors) <- SimpleEff
> contrasts(factors)
new_ver old_ver
0.0 0 -1 -0.5
1.0 -1 0 0.5
0.1 0 1 -0.5
1.1 1 0 0.5
> simpleEffectModel <-lm(WebVisit$pages ~ factors , data = WebVisit, na.action =
na.exclude)
> summary.lm(simpleEffectModel)
Call:
lm(formula = WebVisit$pages ~ factors, data = WebVisit, na.action = na.exclude)
Residuals:
Min 1Q Median 3Q Max
-3.0325 -1.0325 -0.7198 0.9469 12.0080
Coefficients:
Estimate Std. Error t value Pr(>|t|)
(Intercept) 2.69936 0.05050 53.455 < 2e-16 ***
factorsnew_ver 0.33339 0.07124 4.680 3.27e-06 ***
factorsold_ver 1.02024 0.07159 14.252 < 2e-16 ***
factors -0.62582 0.10100 -6.196 8.44e-10 ***
---
Signif. codes: 0 ’***’ 0.001 ’**’ 0.01 ’*’ 0.05 ’.’ 0.1 ’ ’ 1
Residual standard error: 1.596 on 995 degrees of freedom
Multiple R-squared: 0.2079, Adjusted R-squared: 0.2056
F-statistic: 87.08 on 3 and 995 DF, p-value: < 2.2e-16
> #load libraries
> library(Rcmdr)
> library(foreign)
> library(ggplot2)
> library(lattice)
> library(QuantPsyc)
Loading required package: boot
Attaching package: ’boot’
The following object is masked from ’package:lattice’:
melanoma
The following object is masked from ’package:sm’:
dogs
The following object is masked from ’package:car’:
logit
Loading required package: MASS
Attaching package: ’MASS’
The following object is masked from ’package:sm’:
muscle
Attaching package: ’QuantPsyc’
The following object is masked from ’package:base’:
norm
> library(car)
> #Measurements for 158 cruise ships
> colnames = c("Ship_Name", "Cruise_Line", "Age",
+ "Tonnage", "Passengers", "Length",
+ "Cabins","Passengers_Density","Crew")
> #Import the dataset
> Cruise_Ship <- read.fwf(
+ file=url("http://www.stat.ufl.edu/~winner/data/cruise_ship.dat"),
+ widths=c(20, 20, 9, 8, 8, 8, 8, 9, 9 ),
+ col.names = colnames)
> #Histogram of number of Passengers (100s)
> hist(Cruise_Ship$Passengers, main="Histogram of number of passengers (100s)", col =
"blue",xlab = "Number of passengers (100s)")
> #Density plot of number of Passengers (100s)
> density_passengers <-density(Cruise_Ship$Passengers)
> plot(density_passengers , main = "Density plot of number of Passengers (100s)", col =
"blue", xlab = "Number of passengers (100s)")
> #computation of the mean and the standar deviation of the number of Passengers (100s)
> mean(Cruise_Ship$Passengers)
[1] 18.45741
> sd(Cruise_Ship$Passengers)
[1] 9.677095
> #Scatter plot between Number of Passengers(100s)-Length of the ship(100s of feat)
> xyplot(Cruise_Ship$Passengers ~ Cruise_Ship$Length, data = Cruise_Ship,
+ xlab = "Length of the ship (100s of feat)",
+ ylab = "Number of Passengers (100s)",
+ main = "Number of Passengers(100s)-Length of the ship(100s of feat)"
+ )
> #Scatter plot between Number of Passengers(100s)-Number of Cabins(100s) of ship
> xyplot(Cruise_Ship$Passengers ~ Cruise_Ship$Cabins, data = Cruise_Ship,
+ xlab = "Number of Cabins(100s) of ship",
+ ylab = "Number of Passengers (100s)",
+ main = "Number of Passengers(100s)-Number of Cabins(100s)of ship"
+ )
> #Scatter plot between Number of Passengers(100s)-Tonnage (1000s of tons) of ship
> xyplot(Cruise_Ship$Passengers ~ Cruise_Ship$Tonnage, data = Cruise_Ship,
+ xlab = "Tonnage (1000s of tons) of ship",
+ ylab = "Number of Passengers (100s)",
+ main = "Number of Passengers(100s)-Tonnage(1000s of tons) of ship"
+ )
> #creation of 7 models
> model0<-lm(Cruise_Ship$Passengers~1, data=Cruise_Ship)
> model1<-lm(Cruise_Ship$Passengers~Cruise_Ship$Length, data=Cruise_Ship)
> model2<-lm(Cruise_Ship$Passengers~Cruise_Ship$Tonnage, data=Cruise_Ship)
> model3<-lm(Cruise_Ship$Passengers~Cruise_Ship$Cabins, data=Cruise_Ship)
> model4<-lm(Cruise_Ship$Passengers~Cruise_Ship$Length+Cruise_Ship$Cabins,
data=Cruise_Ship)
> model5<-lm(Cruise_Ship$Passengers~Cruise_Ship$Cabins+Cruise_Ship$Tonnage,
data=Cruise_Ship)
> model6<-lm(Cruise_Ship$Passengers~Cruise_Ship$Length+Cruise_Ship$Tonnage,
data=Cruise_Ship)
> fit<-lm( Cruise_Ship$Passengers ~ Cruise_Ship$Length + Cruise_Ship$Cabins +
Cruise_Ship$Tonnage, data = Cruise_Ship)
> #comparison of the models using ANOVA function
> anova(model0,model1,model2,model3,model4,model5,model6,fit)
Analysis of Variance Table
Model 1: Cruise_Ship$Passengers ~ 1
Model 2: Cruise_Ship$Passengers ~ Cruise_Ship$Length
Model 3: Cruise_Ship$Passengers ~ Cruise_Ship$Tonnage
Model 4: Cruise_Ship$Passengers ~ Cruise_Ship$Cabins
Model 5: Cruise_Ship$Passengers ~ Cruise_Ship$Length + Cruise_Ship$Cabins
Model 6: Cruise_Ship$Passengers ~ Cruise_Ship$Cabins + Cruise_Ship$Tonnage
Model 7: Cruise_Ship$Passengers ~ Cruise_Ship$Length + Cruise_Ship$Tonnage
Model 8: Cruise_Ship$Passengers ~ Cruise_Ship$Length + Cruise_Ship$Cabins +
Cruise_Ship$Tonnage
Res.Df RSS Df Sum of Sq F Pr(>F)
1 157 14702.4
2 156 3225.2 1 11477.2 2780.3302 < 2e-16 ***
3 156 1571.1 0 1654.1
4 156 687.5 0 883.6
5 155 672.0 1 15.4 3.7399 0.05496 .
6 155 635.7 0 36.3
7 155 1557.3 0 -921.6
8 154 635.7 1 921.6 223.2466 < 2e-16 ***
---
Signif. codes: 0 ’***’ 0.001 ’**’ 0.01 ’*’ 0.05 ’.’ 0.1 ’ ’ 1
> #summary of the fit model
> summary(fit)
Call:
lm(formula = Cruise_Ship$Passengers ~ Cruise_Ship$Length + Cruise_Ship$Cabins +
Cruise_Ship$Tonnage, data = Cruise_Ship)
Residuals:
Min 1Q Median 3Q Max
-3.0910 -1.0304 -0.5211 0.0814 10.9521
Coefficients:
Estimate Std. Error t value Pr(>|t|)
(Intercept) -0.296000 1.214610 -0.244 0.80779
Cruise_Ship$Length 0.004222 0.235752 0.018 0.98574
Cruise_Ship$Cabins 1.727292 0.115604 14.941 < 2e-16 ***
Cruise_Ship$Tonnage 0.048637 0.016402 2.965 0.00351 **
---
Signif. codes: 0 ’***’ 0.001 ’**’ 0.01 ’*’ 0.05 ’.’ 0.1 ’ ’ 1
Residual standard error: 2.032 on 154 degrees of freedom
Multiple R-squared: 0.9568, Adjusted R-squared: 0.9559
F-statistic: 1136 on 3 and 154 DF, p-value: < 2.2e-16
> #Determination of the confidence intervals (95%)
> confint(fit)
2.5 % 97.5 %
(Intercept) -2.6954489 2.10344794
Cruise_Ship$Length -0.4615031 0.46994631
Cruise_Ship$Cabins 1.4989178 1.95566703
Cruise_Ship$Tonnage 0.0162354 0.08103908
> #Computation of the the beta values (standardised regression coefficients):
> lm.beta(fit)
Cruise_Ship$Length Cruise_Ship$Cabins Cruise_Ship$Tonnage
0.0007823944 0.7981160818 0.1871162826
> vif(fit)
Cruise_Ship$Length Cruise_Ship$Cabins Cruise_Ship$Tonnage
6.799215 10.162402 14.181589
> #checking the normality assumption of residuals
> shipres = residuals(fit)#
> qqnorm(shipres, main="Normality assumption of Residuals")
> qqline(shipres)
> hist(shipres,main="Histogram of Residuals", xlab = "Residuals")
> #checking the linearty assumption
> qqmath( ~ resid(fit),
+ xlab = "Theoretical Quantiles",
+ ylab = "Residuals"
+ )
> leveneTest(Cruise_Ship$Passengers~
factor(Cruise_Ship$Length)*factor(Cruise_Ship$Cabins), center = mean)
Levene’s Test for Homogeneity of Variance (center = mean)
Df F value Pr(>F)
group 113 5.0802 0.00000001382 ***
44
---
Signif. codes: 0 ’***’ 0.001 ’**’ 0.01 ’*’ 0.05 ’.’ 0.1 ’ ’ 1
> leveneTest(Cruise_Ship$Passengers, factor(Cruise_Ship$Cabins), center = mean)
Levene’s Test for Homogeneity of Variance (center = mean)
Df F value Pr(>F)
group 97 2.8778 0.00001039 ***
60
---
Signif. codes: 0 ’***’ 0.001 ’**’ 0.01 ’*’ 0.05 ’.’ 0.1 ’ ’ 1
> leveneTest(Cruise_Ship$Passengers, factor(Cruise_Ship$Length), center = mean)
Levene’s Test for Homogeneity of Variance (center = mean)
Df F value Pr(>F)
group 79 1.6893 0.01071 *
78
---
Signif. codes: 0 ’***’ 0.001 ’**’ 0.01 ’*’ 0.05 ’.’ 0.1 ’ ’ 1
> leveneTest(Cruise_Ship$Passengers, factor(Cruise_Ship$Tonnage), center = mean)
Levene’s Test for Homogeneity of Variance (center = mean)
Df F value Pr(>F)
group 93 5.9238 1.73e-12 ***
64
---
Signif. codes: 0 ’***’ 0.001 ’**’ 0.01 ’*’ 0.05 ’.’ 0.1 ’ ’ 1
> #application of Cooks’ distance
> plot(cooks.distance(fit),ylab="Cooks distances",xlab="Observation number")
> #application of DFBeta
> dfbetaPlots(fit)
> ###################################################Question 4 Logistic
Regression######################################
> library(foreign)
> library(car) #Package includes Levene’s test
> library(tidyr) # for wide to long format transformation of the data
Attaching package: ’tidyr’
The following objects are masked from ’package:reshape’:
expand, smiths
The following object is masked from ’package:RCurl’:
complete
> library(ggplot2)
> library(QuantPsyc) #include lm.beta()
> library(class)
Attaching package: ’class’
The following object is masked from ’package:reshape’:
condense
> library(pscl)
Classes and Methods for R developed in the
Political Science Computational Laboratory
Department of Political Science
Stanford University
Simon Jackman
hurdle and zeroinfl functions by Achim Zeileis
> library(gmodels)
> columns = c("PH","NisinConcentration","Temoerature","BrixConcetration",
+ "Growth"
+ )
> #reading the dataset
> CRAinAppleJuice <- read.fwf(
+ file=url("http://www.stat.ufl.edu/~winner/data/apple_juice.dat"),
+ widths=c(9, 9, 9, 8, 8),
+ col.names = columns)
> model0 <- glm(CRAinAppleJuice$Growth ~ 1, data=CRAinAppleJuice, family=binomial())
> model1 <- glm(CRAinAppleJuice$Growth ~ PH, data=CRAinAppleJuice, family=binomial())
> #model2 <- glm(CRAinAppleJuice$Growth ~NisinConcentration , data=CRAinAppleJuice,
family=binomial())
> model2<-glm(CRAinAppleJuice$Growth ~ PH + NisinConcentration, data=CRAinAppleJuice,
family=binomial())
> anova(model0,model1,model2,test = "Chisq")
Analysis of Deviance Table
Model 1: CRAinAppleJuice$Growth ~ 1
Model 2: CRAinAppleJuice$Growth ~ PH
Model 3: CRAinAppleJuice$Growth ~ PH + NisinConcentration
Resid. Df Resid. Dev Df Deviance Pr(>Chi)
1 73 95.945
2 72 87.248 1 8.6977 0.003186 **
3 71 64.049 1 23.1983 0.000001461 ***
---
Signif. codes: 0 ’***’ 0.001 ’**’ 0.01 ’*’ 0.05 ’.’ 0.1 ’ ’ 1
> summary(model2)
Call:
glm(formula = CRAinAppleJuice$Growth ~ PH + NisinConcentration,
family = binomial(), data = CRAinAppleJuice)
Deviance Residuals:
Min 1Q Median 3Q Max
-2.3566 -0.6756 -0.2462 0.5301 1.8431
Coefficients:
Estimate Std. Error z value Pr(>|z|)
(Intercept) -6.31911 2.07827 -3.041 0.002361 **
PH 1.64210 0.49171 3.340 0.000839 ***
NisinConcentration -0.05819 0.01498 -3.884 0.000103 ***
---
Signif. codes: 0 ’***’ 0.001 ’**’ 0.01 ’*’ 0.05 ’.’ 0.1 ’ ’ 1
(Dispersion parameter for binomial family taken to be 1)
Null deviance: 95.945 on 73 degrees of freedom
Residual deviance: 64.049 on 71 degrees of freedom
AIC: 70.049
Number of Fisher Scoring iterations: 5
> ##Pseudo R2
> logisticPseudoR2s <- function(LogModel)
+ #taken from Andy Fields et al. book on R, p.334
+ {
+ dev <- LogModel$deviance
+ nullDev <- LogModel$null.deviance
+ modelN <- length(LogModel$fitted.values)
+ R.l <- 1 - dev / nullDev
+ R.cs <- 1 - exp(-(nullDev - dev) / modelN)
+ R.n <- R.cs / (1 - (exp(-(nullDev / modelN))))
+ cat("Pseudo R^2 for logistic regression\n")
+ cat("Hosmer and Lemshow R^2: ", round(R.l, 3), "\n")
+ cat("Cox and Snell R^2: ", round(R.cs, 3), "\n")
+ cat("Nagelkerke R^2: ", round(R.n, 3), "\n")
+ }
> logisticPseudoR2s(model2)
Pseudo R^2 for logistic regression
Hosmer and Lemshow R^2: 0.332
Cox and Snell R^2: 0.35
Nagelkerke R^2: 0.482
> ##Odd and conf
> exp(model2$coefficients)
(Intercept) PH NisinConcentration
0.00180154 5.16602001 0.94346707
> exp(confint(model2))
Waiting for profiling to be done...
2.5 % 97.5 %
(Intercept) 0.00002005301 0.07865834
PH 2.14732861143 15.24082097
NisinConcentration 0.91259180182 0.96873926
> ##crosstable
> CRAinAppleJuice$Growpred[fitted(model2) <=0.5] <- 0
> CRAinAppleJuice$Growpred[fitted(model2) > 0.5] <- 1
> CRAinAppleJuice$Growpred<-factor(CRAinAppleJuice$Growpred, levels = c(0:1), labels =
c("Absence","Presence"))
> table(CRAinAppleJuice$Growth, CRAinAppleJuice$Growpred)
Absence Presence
0 42 6
1 8 18
> CrossTable(CRAinAppleJuice$Growpred, CRAinAppleJuice$Growth, prop.c=FALSE,
prop.t=FALSE, prop.chisq=FALSE, fisher=FALSE, chisq=FALSE, expected = FALSE)
Cell Contents
|-------------------------|
| N |
| N / Row Total |
|-------------------------|
Total Observations in Table: 74
| CRAinAppleJuice$Growth
CRAinAppleJuice$Growpred | 0 | 1 | Row Total |
-------------------------|-----------|-----------|-----------|
Absence | 42 | 8 | 50 |
| 0.840 | 0.160 | 0.676 |
-------------------------|-----------|-----------|-----------|
Presence | 6 | 18 | 24 |
| 0.250 | 0.750 | 0.324 |
-------------------------|-----------|-----------|-----------|
Column Total | 48 | 26 | 74 |
-------------------------|-----------|-----------|-----------|
> ##########################################################Part
3#####################################
>
###################################################################################################
> library(MASS)
> library(foreign)
> library(car)
> library(ggplot2)
> library(nlme)
> library(reshape)
> library(graphics)
> require(lattice)
> library(lattice)
> require(Matrix)
Loading required package: Matrix
Attaching package: ’Matrix’
The following object is masked from ’package:tidyr’:
expand
The following object is masked from ’package:reshape’:
expand
> library(lme4)
Attaching package: ’lme4’
The following object is masked from ’package:nlme’:
lmList
> #question 1#
> set1 <- read.csv2("set1.csv", header = TRUE, sep = ",",
+ dec = ",", fill = TRUE, comment.char = "")
> hist(set1$score,col="lightblue",main="Score distribution",xlab="score")
> boxplot(set1$score ~ set1$session , xlab="Session", ylab="Score", main="Session related
to score",col="green",outcol="red")
> #question 2
> ##We present thr intercdept only model which violates the independent assumption. It
will only allow us to comparre its results with multilevel analysis.
> interceptOnly <- gls(score ~ 1, data = set1, method = "ML")
> summary(interceptOnly)
Generalized least squares fit by maximum likelihood
Model: score ~ 1
Data: set1
AIC BIC logLik
177112.8 177128.1 -88554.38
Coefficients:
Value Std.Error t-value p-value
(Intercept) 102.2758 0.4619305 221.4095 0
Standardized residuals:
Min Q1 Med Q3 Max
-3.80616985 -0.56725058 0.04643939 0.57489464 4.46159777
Residual standard error: 58.66154
Degrees of freedom: 16128 total; 16127 residual
> randomInterceptOnly <- lme(score ~ 1, data = set1, random = ~1|Subject, method = "ML")
> summary(randomInterceptOnly)
Linear mixed-effects model fit by maximum likelihood
Data: set1
AIC BIC logLik
162676.1 162699.1 -81335.04
Random effects:
Formula: ~1 | Subject
(Intercept) Residual
StdDev: 46.38355 35.22182
Fixed effects: score ~ 1
Value Std.Error DF t-value p-value
(Intercept) 102.0279 2.091487 15627 48.78248 0
Standardized Within-Group Residuals:
Min Q1 Med Q3 Max
-4.135330461 -0.641203765 0.008247302 0.642200771 4.009769404
Number of Observations: 16128
Number of Groups: 501
> #adding session to our model
> sessionMod<-lme(score ~(1+ session), data = set1, random = ~1|Subject, method = "ML")
> summary(sessionMod)
Linear mixed-effects model fit by maximum likelihood
Data: set1
AIC BIC logLik
162453.4 162484.1 -81222.7
Random effects:
Formula: ~1 | Subject
(Intercept) Residual
StdDev: 46.443 34.96826
Fixed effects: score ~ (1 + session)
Value Std.Error DF t-value p-value
(Intercept) 108.66574 2.1398997 15626 50.78076 0
session -0.42533 0.0282743 15626 -15.04304 0
Correlation:
(Intr)
session -0.206
Standardized Within-Group Residuals:
Min Q1 Med Q3 Max
-4.114815156 -0.647994395 0.007899928 0.638213193 4.160353413
Number of Observations: 16128
Number of Groups: 501
> ##compute confidence intervals
> intervals(sessionMod, 0.95)
Approximate 95% confidence intervals
Fixed effects:
lower est. upper
(Intercept) 104.4715439 108.6657350 112.8599262
session -0.4807492 -0.4253317 -0.3699142
attr(,"label")
[1] "Fixed effects:"
Random Effects:
Level: Subject
lower est. upper
sd((Intercept)) 43.60687 46.443 49.4636
Within-group standard error:
lower est. upper
34.58273 34.96826 35.35809