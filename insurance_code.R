setwd("F:/DATASET FOR REGRESSION")

library(psych)
library(ggplot2)

# Reading the data
ins<-read.csv("insurance.csv")

# Finding the data type of variables
str(ins)

# Finding NA value
table(is.na(ins))

# For altering the names
ins$sex<-factor(ins$sex,levels=c("female","male"),labels=c("f","m"))

# Exploring the categorical variables
ggplot(data=ins) + geom_bar(aes(x=sex,y=..count..,fill=sex)) + geom_text(stat='count',aes(x=sex,label=..count..),hjust=-0.5) + coord_flip()
ggplot(data=ins) + geom_bar(aes(x=region,y=..count..,fill=region)) + geom_text(stat='count',aes(x=region,label=..count..),hjust=-0.5) + coord_flip()

# Creation of correlation matrix

cor(ins[c("age","bmi","children","charges")])

# Creation of scatterplot matrix

pairs(ins[c("age","bmi","children","charges")])

pairs.panels(ins[c("age","bmi","children","charges")])

# Training the model
model<-lm(charges~age+children+bmi+sex+smoker+region,data=ins)
model
summary(model)

# Improving model by adding non linear relationships

ins$age2<-ins$age^2

# Converting a numerical value into binary value

ins$bmi30<-ifelse(ins$bmi>=30,1,0)
ins<-ins[,-9]

# Adding interaction effect

model1<-lm(charges~age+children+bmi+sex+smoker+region+age2+bmi30*smoker,data=ins)
summary(model1)
