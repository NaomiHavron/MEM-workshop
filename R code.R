##At the begining of every session you upload the packages you are going to use
library(languageR)
library(lme4)
library(psych) 
library (lmerTest)
library (doBy)
library (ggplot2)
library(dplyr)
library(tidyr)
library(boot)


##first set directory to the one containing the file
setwd("C:/Users/naomi/Dropbox/Naomi/Postdoc-NaomiHavron/MEM workshop")

#now read in the file to an object called "d"
d <- read.csv("lexicalpriming.csv", header=T, na.strings = "NULL", comment.char="") 

##what does the data look like?
summary(d)


##some variables are coded wrong, let's fix that!
d$wordness<-as.factor(d$wordness)

## faster to do all at once:
d[,c("prime","subject", "item")] <- lapply(d[,c("prime","subject", "item")] , factor)

##random intercepts

MEM1 <- lmer(RT ~ wordness + (1|subject)+ (1|item), data = d, control=lmerControl(optimizer="bobyqa"))
summary (MEM1)
ranef(MEM1)
plot(ranef(MEM1))
################### -> back to slides

##random slopes
MEM1<- lmer(RT ~ wordness + (1+wordness|subject)+ (1|item), data = d, control=lmerControl(optimizer="bobyqa"))
summary (MEM1)
ranef(MEM1)

##model comparison
MEM1<- lmer(RT ~ wordness + (1+wordness|subject)+ (1|item), data = d, control=lmerControl(optimizer="bobyqa"))
MEM2<- lmer(RT ~ wordness + (1|subject)+ (1|item), data = d, control=lmerControl(optimizer="bobyqa"))


anova(MEM2,MEM1)

### but we usually use it to test our fixed effects:
MEM1<- lmer(RT ~ wordness + (1+wordness|subject)+ (1|item), data = d, control=lmerControl(optimizer="bobyqa"))
MEM2<- lmer(RT ~  (1+wordness|subject)+ (1|item), data = d, control=lmerControl(optimizer="bobyqa"))

anova(MEM2,MEM1)

###### -> back to presentation!

##model does not converge
MEM1<- lmer(RT ~ wordness + (1+wordness|subject)+ (1+prime+luminance|item), data = d, control=lmerControl(optimizer="bobyqa"))
summary (MEM1)

MEM1<- lmer(RT ~ wordness + (1+wordness|subject)+ (1+prime|item), data = d, control=lmerControl(optimizer="bobyqa"))
summary (MEM1)

###### -> back to presentation!

##coding categorical data
#1. dummy
levels(d$wordness)
levels(d$wordness) <- c("nonword", "word")
contrasts(d$wordness) <- c(0, 1)

MEM1<- lmer(RT ~ wordness + (1+wordness|subject)+ (1|item), data = d, control=lmerControl(optimizer="bobyqa"))
summary (MEM1)

#2. sum
levels(d$wordness)
levels(d$wordness) <- c("nonword", "word")
contrasts(d$wordness) <- c(-1, 1)

                                                                                                      
MEM1<- lmer(RT ~ wordness + (1+wordness|subject)+ (1|item), data = d, control=lmerControl(optimizer="bobyqa"))
summary (MEM1)

###### -> back to presentation!

##interactions
MEM1<- lmer(RT ~ luminance*wordness + (1+wordness|subject)+ (1|item), data = d, control=lmerControl(optimizer="bobyqa"))
summary (MEM1)

#simple slopes:
MEM1<- lmer(RT ~ luminance*wordness-luminance + (1+wordness|subject)+ (1|item), data = d, control=lmerControl(optimizer="bobyqa"))
summary (MEM1)

#we could also plot this to see how it looks
ggplot(data = subset(d), aes(y=RT, x=luminance, colour = wordness))+
  stat_smooth(aes(colour=wordness ), method = "lm", formula = y ~ x, size=1.4)+
  theme_bw()+
  theme(text = element_text(size = 20))

## model comparisons with interactions
d$interaction<-contrasts(d$wordness)[d$wordness]*
  contrasts(d$prime)[d$prime]

MEM1<- lmer(RT ~ prime + wordness + interaction + (1+wordness|subject)+ (1|item), data = d, control=lmerControl(optimizer="bobyqa"))
summary (MEM1)

###### -> back to presentation!

##logistic regression
list1<- rbinom(4920, 1, 0.8)
list2<- rbinom(4920, 1, 0.2)

d$decision<-ifelse(d$wordness=="word",list1,list2)
summary(d)

MEM1<- glmer(decision ~ wordness+ (1+wordness|subject) + (1|item), data = d, family = binomial, control=glmerControl(optimizer="bobyqa"))
summary (MEM1)

MEM1<- glmer(decision ~ wordness+ (1|subject) + (1|item), data = d, family = binomial, control=glmerControl(optimizer="bobyqa"))
summary (MEM1)

###### BONUS
levels(d$prime)
levels(d$prime) <- c("identical", "different", "neutral", "control")
treat.codes <- matrix(c(0, 0, 1, 0,1, 0,1, 0, 0,0,0,0), 
                      nrow = 4, 
                      byrow = T,
                      dimnames = list(c("identical", "different", "neutral", "control"), c("identical", "different", "neutral")))
contrasts(d$prime) <- treat.codes
