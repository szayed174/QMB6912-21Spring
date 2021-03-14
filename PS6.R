##################################################
# 
#University of Central Florida
#College of Business
#QMB 6912 Capstone Project in Business Analytics
#Problem Set #6
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
library(relaimpo)

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
#install.packages("kdensity", dependencies = TRUE)
#library(kdensity)

#install.packages("plyr")
library(plyr)

#install.packages("gcookbook")
library(gcookbook)

#Box-Cox transformation package
#install.packages("faraway")
library(faraway)

#Read CSV
data <- read.csv("FlyReels.csv")
print(data)

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

#Changing Country to numeric. Where USA=1 China & Korea=2
Country_num <- revalue(data$Country, c("USA"=1,"China"=2,"Korea"=2))
#Adding Country_num  as a column to the dataframe
attach(data)
data$Country_num =Country_num
detach(data)
head(data,50)

data$Country_num<-as.numeric(as.character(data$Country_num))
str(data)

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


#filter relevant columns
data_num <- data %>%
  select(3,4,5,6,7,9,10,11)
head(data_num)

#Print Pairs:
pairs(data_num)
# function gpairs in "gpairs"
#install.packages("gpairs")
library("gpairs")
gpairs(data_num)

###############     Multiple Variable Regression Mixed    ##################################################3

modelMultiple <- lm(Price ~ Brand+Weight+Diameter+Width+Sealed+Country+Machined+density, data)
print(modelMultiple)
summary(modelMultiple)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(modelMultiple)


modelMultiple.log <- lm(log(Price) ~ Brand+Weight+Diameter+Width+Sealed+Country+Machined+density, data)
print(modelMultiple.log)
summary(modelMultiple.log)

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(modelMultiple.log)

modelMultiple3 <- lm((((Price^0.3131)-1)/0.3131) ~ Brand+Weight+Diameter+Width+Sealed+Country+Machined+density, data)
summary(modelMultiple3)

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(modelMultiple3)


modelMultiple4 <- lm(Price ~.-Price-Country_num, data)
summary(modelMultiple4)

##################    AIC    #######################

#model selection with AIC, simple to use
#modelMultiple
step1 <- stepAIC(modelMultiple, direction="both")
step1$anova # display results

AIC1 <- AIC(modelMultiple)
print(AIC1)
#modelMultiple.log
step2 <- stepAIC(modelMultiple.log, direction="both")
step2$anova # display results

AIC2 <- AIC(modelMultiple.log)
print(AIC2)

#modelMultiple3
step3 <- stepAIC(modelMultiple3, direction="both")
step3$anova # display results

AIC3 <- AIC(modelMultiple3)
print(AIC3)

#############       Likelihood        ########

calcLogLikelihood <- function(model_param, dependent_var_param, lambda_param) {
  eps = 1/50
  y <- dependent_var_param
  xqr <- model_param$qr
  n <- length(y)
  y <- y/exp(mean(log(y))) #y/geometric_mean(y)
  #logy <- log(y)
  
  xl <- loglik <- as.vector(lambda)
  m <- length(xl)
  if (abs(lambda_param) > eps) {
    yt <- (y^lambda_param - 1)/lambda_param
  }  else {
    yt <- log(y)
  }
  
  logLikelihood <- -n/2 * log(sum(qr.resid(xqr, yt)^2))
  
  
  return(logLikelihood)
}


test <- calcLogLikelihood(modelMultiple, data$Price, 2)
test <- calcLogLikelihood(modelMultiple, data$Price, 0)

test <- calcLogLikelihood(modelMultiple.log, data$Price, 2)
test <- calcLogLikelihood(modelMultiple.log, data$Price, 0)

test <- calcLogLikelihood(modelMultiple3, data$Price, 2)
test <- calcLogLikelihood(modelMultiple3, data$Price, 0)


logLik(modelMultiple)
logLik(modelMultiple.log)
logLik(modelMultiple3)

#For modelMultiple1
lambda = seq(-2, 2, 1/20)
i=1;
logLikelihood_y <- as.vector(lambda)
for (x_lambda in lambda){
  logLikelihood_y[i] <- calcLogLikelihood(modelMultiple, data$Price, x_lambda)
  i = i + 1
}
plot(lambda, logLikelihood_y)

#For modelMultiple LOG
lambda2 = seq(-5, 5, 1/20)
i=1;
logLikelihood_y2 <- as.vector(lambda2)
for (x_lambda in lambda2){
  logLikelihood_y2[i] <- calcLogLikelihood(modelMultiple.log, log(data$Price), x_lambda)
  i = i + 1
}
plot(lambda2, logLikelihood_y2)

###############    Box-Cox Transformation     ##################################################

#box-cox for model 1
bc_m1=boxcox(modelMultiple,lambda=seq(-2,2, by=0.1))
print(bc_m1)
  #extract best lambda - close to the above lambda -3,3 if we round up
  best.lam_m1=bc_m1$x[which(bc_m1$y==max(bc_m1$y))]
  print(best.lam_m1)

#box-cox for model2
bc_m2=boxcox(modelMultiple.log,lambda=seq(-5,5,by = 0.1))
print(bc_m2)
  #extract best lambda - close to the above lambda -3,3 if we round up
  best.lam_m2=bc_m2$x[which(bc_m2$y==max(bc_m2$y))]
  print(best.lam_m2)

#box-cox for model3
bc_m3=boxcox(modelMultiple3,lambda=seq(-3,3,by = 0.1))
print(bc_m3)
  #extract best lambda - close to the above lambda -3,3 if we round up
  best.lam_m3=bc_m3$x[which(bc_m3$y==max(bc_m3$y))]
  print(best.lam_m3)  
  
#box-cox for model4
bc_m4=boxcox(modelMultiple4,lambda=seq(-3,3,by = 0.1))
print(bc_m4)
  #extract best lambda - close to the above lambda -3,3 if we round up
  best.lam_m4=bc_m4$x[which(bc_m4$y==max(bc_m4$y))]
  print(best.lam_m4)

  

