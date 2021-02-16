##########      ENTM 642 Week 2 Assignment    #########
##########  Distributions & Transformations  #########
##########          Hannah Klein           ##########

#setwd(F:/128 Sandisk backup/2021 Classes/ENTM642)

# library(MASS)							# One of the most well-used packages 
# library(ROCR)							# ROC curves and other classification evaluation tools
# library(PredictABEL) 						# Handy for Hosmer-Lemeshow plot for model goodness of fit 
# library(PresenceAbsence)  					# All kinds of goodies for pres--abs data


# 1.) Read in the SaperdaTridentata_elytra.csv data. Plot the data with sex as a predictor of elytral
# length (size).

my.data <- read.csv("SaperdaTridentata_elytra.csv", header=T)
my.data
attach(my.data)
names(my.data)


# not sure how to predict with out making the data binary 

data.log <- glm(Sex ~ Size,data=my.data, family=binomial("logit"))	
my.pred <- predict(x, type="response")	


#2.) Convert the male/female factor (sex) to be male=0 and female=1. Replot the data.
my.data$isFemale = as.integer(Sex == "f")

my.log <- glm(isFemale ~ Size, data=my.data, family=binomial("logit"))	
my.log
summary(my.log)									# Lots of information here

my.pred1 <- predict(my.log, type="response")				# One of several ways to generate the predicted values
plot(Size, isFemale)							# Make original plot and ...
points(Size, isFemale, pch=10, col="blue", cex=0.5)		# Add the predicted as smaller red points

# 3.) Conduct a logistic regression on the data. Replot the data (from question 2) and make the
# male and female points different colors and symbols. Add a curve of the predicted relationship
# between elytral length and sex. Add a legend.


# 4.) What is the test statistic, degrees of freedom, and p-value associated with the coefficient for
# elytral length?


#   5.) What is the significance of the overall model and how did you calculate this?


#   6.) What does the coefficient tell you about the change in probability of a beetle being male or
# female for a given length of elytra?
