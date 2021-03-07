##################################################
# 
#University of Central Florida
#College of Business
#QMB 6912 Capstone Project in Business Analytics
#Problem Set #5
#
#Salma Zayed
##################################################
# 
#Flyreels data set
# 
##################################################
# 
#working directory
#print(getwd())
#wd<-"C:/Users/szaye/OneDrive/Documents/UCF/Spring 2021/QMB6912/ProblemSet6"
# Set data directory.
data_dir <- 'Data'

# Set directory for storing figures.
fig_dir <- 'Figures'

# Set directory for storing tables.
tab_dir <- 'Tables'

# Set directory for storing text.
text_dir <- 'Text'

# Load libraries.

# The xtable library creates tex scripts for tables.
#install.packages("xtable")
library(xtable)

#install.packages("lattice")
library(lattice)

#install.packages("gclus")
library(gclus)

# The texreg library creates tex scripts for tables from

#Selecting relevant variables:
#install.packages("relaimpo")
library("relaimpo")

# regression models.
#install.packages("texreg")
library(texreg)
#install.packages("ggpubr")
library(ggpubr)

library(tidyverse) 
#library(Hmisc)
library(ggplot2)
library(dplyr)
#library(data.table)
#library(funModeling) 

#compare models
#install.packages("MASS")
library(MASS)

#install.packages("plyr")
library(plyr)

#install.packages("gcookbook")
#library(gcookbook)

#Read CSV
data <- read.csv("FlyReels.csv")
print(data)

#Assuming that a reel is like a cylinder where Volume = ?? R^2L  
density <- ((data$Weight)/(pi * (data$Diameter/2)^2 * data$Width))

#Adding density as a column to the dataframe
attach(data)
data$density =density
detach(data)

#check headers to make sure density was added
colnames(data)

#check top rows to make sure everything looks good
head(data)

# Print the summary.
print(summary(data))


###############     Muliple Variable Regression Mixed    ##################################################3

modelMultiple <- lm(Price ~ Weight+Diameter+Width+Sealed+Country+Machined+density, data)
print(modelMultiple)
coef(modelMultiple)
summary(modelMultiple)

modelMultiple.Country <- lm(Price ~ Country, data)
summary(modelMultiple.Country)

modelMultiple.Country2 <- glm(Price ~ Country, data, family="gaussian")
summary(modelMultiple.Country2)


layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(modelMultiple1)


#Changing Yes and No characters into numeric 0s and 1
data$Sealed <- revalue(data$Sealed, c("Yes"=1))
data$Sealed <- revalue(data$Sealed, c("No"=0))
head(data$Sealed)
data$Sealed<-as.numeric(as.character(data$Sealed))
str(data$Sealed)

data$Machined <- revalue(data$Machined, c("Yes"=1))
data$Machined <- revalue(data$Machined, c("No"=0))
head(data$Machined)
data$Machined<-as.numeric(as.character(data$Machined))
str(data$Machined)


#Changing Country to numeric. Where USA=1 China=2 Korea=3
data$Country <- revalue(data$Country, c("USA"=1))
data$Country <- revalue(data$Country, c("China"=2))
data$Country <- revalue(data$Country, c("Korea"=3))
head(data$Country)
data$Country<-as.numeric(as.character(data$Country))
str(data)

###############     Muliple Variable Regression Numerics    ##################################################3
modelMultiple1 <- lm(Price ~ Weight+Diameter+Width+Sealed+Country+Machined+density, data)
print(modelMultiple1)
coef(modelMultiple1)
summary(modelMultiple1)

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(modelMultiple1)


#filter relevant columns
data_filtered <- data %>% select(3,4,5,6,7,8,9,10)
head(data_filtered)

#let's make the data have mean of zero and standard deviation of 1 standardize the data (normalize)
dataScaled <- as.data.frame(scale(data_filtered))
print(dataScaled[1:7,])

pairs(dataScaled)

#Running the Muliple Variable Regression with scaled data
#modelMultiple1_scaled <- lm(Price ~ Weight+Diameter+Width+Sealed+Country+Machined+density, dataScaled)
#print(modelMultiple1_scaled)
#coef(modelMultiple1_scaled)
#summary(modelMultiple1_scaled)

#layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
#plot(modelMultiple1_scaled)



##Choosing the most significant variables (Diameter, Sealed, Country, Machined)
#Multiple Variable Regression
modelMultiple2 <- lm(Price ~ Diameter+Sealed+Country+Machined, data)
print(modelMultiple2)
summary(modelMultiple2)

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(modelMultiple2)


#Multiple Variable Regression scaled
#modelMultiple2_scaled <- lm(Price ~ Diameter+Sealed+Country+Machined, dataScaled)
#print(modelMultiple2_scaled)
#summary(modelMultiple2_scaled)

#layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
#plot(modelMultiple2_scaled)

data$Country = as.factor(data$Country)
str(data)
#Multiple Variable Regression country broken down
modelMultiple3 <- lm(Price ~ Weight+Diameter+Width+Sealed+Country+Machined+density, data)
print(modelMultiple3)
coef(modelMultiple3)
summary(modelMultiple3)

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(modelMultiple3)



#model selection with AIC, simple to use
#modelMultiple1
step1 <- stepAIC(modelMultiple1, direction="both")
step1$anova # display results

#modelMultiple1_scaled
#step2 <- stepAIC(modelMultiple1_scaled, direction="both")
#step2$anova # display results

#modelMultiple2 - has the highest AIC
step3 <- stepAIC(modelMultiple2, direction="both")
step3$anova # display results

#modelMultiple2_scaled
#step4 <- stepAIC(modelMultiple2_scaled, direction="both")
#step4$anova # display results


#Looking more into variable importance:
#modelMultiple2
boot <- boot.relimp(modelMultiple2, b = 1000, rank = TRUE, diff = TRUE, rela = TRUE)
booteval.relimp(boot)
plot(booteval.relimp(boot,sort=TRUE))

#modelMultiple1
boot2 <- boot.relimp(modelMultiple1, b = 1000, rank = TRUE, diff = TRUE, rela = TRUE)
booteval.relimp(boot2)
plot(booteval.relimp(boot2,sort=TRUE))


###############     Muliple Variable Regression     ##################################################3
