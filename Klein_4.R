##########      ENTM 642 Week 2 Assignment    #########
##########  Distributions & Transformations  #########
##########          Hannah Klein           ##########

#setwd(F:/128 Sandisk backup/2021 Classes/ENTM642)

# library(MASS)							# One of the most well-used packages 
# library(ROCR)							# ROC curves and other classification evaluation tools
# library(PredictABEL) 						# Handy for Hosmer-Lemeshow plot for model goodness of fit 
# library(PresenceAbsence)  					# All kinds of goodies for pres--abs data
#library(ggplot2) 

# 1.) Read in the SaperdaTridentata_elytra.csv data. Plot the data with sex as a predictor of elytral
# length (size).
#change from character to factor as.factor 

my.data <- read.csv("SaperdaTridentata_elytra.csv", header=T)
my.data
attach(my.data)
names(my.data)


sex <-my.data$Sex
size<-my.data$Size
df<- data.frame(size, sex)
plot <-ggplot(df, aes( x=sex,y=size,color=sex)) +
  geom_point() +
  labs(title="Length and sex",x="Length", y = "Sex")
plot + scale_color_brewer(palette ="Set2")+theme_classic()

# not sure how to predict with out making the data binary 

data.log <- glm(Sex ~ Size,data=my.data, family=binomial("logit"))	
my.pred <- predict(x, type="response")	

#2.) Convert the male/female factor (sex) to be male=0 and female=1. Replot the data.
# my.data$isFemale = as.integer(Sex == "f")
isFemale = as.integer(Sex == "f")
my.data2 = cbind(my.data,isFemale)
my.data2

plot(Size, isFemale, ylab = "Sex: Female (1), Male (0)",
     pch=10, col="blue", cex=0.8)


# 3.) Conduct a logistic regression on the data. Replot the data (from question 2) and make the
# male and female points different colors and symbols. Add a curve of the predicted relationship
# between elytral length and sex. Add a legend.

my.log <- glm(isFemale ~ Size, data=my.data, family=binomial("logit"))	
my.log

my.pred1 <- predict(my.log, type="response")

plot(Size, isFemale, col=as.numeric(isFemale)+33, pch=as.numeric(isFemale)+5,
     ylab = "Sex: Female (1), Male (0)",
     xlab = "Lenght",
     main = "Saperda Tridentata Length by Sex")
points(Size, my.pred1, pch=16, col="blue", cex=0.8)
legend(20, .3, legend=c("Female", "Male"), col=c("red", "black"), pch =(6:5)) # cant get legend to work 

# 4.) What is the test statistic, degrees of freedom, and p-value associated with the coefficient for
# elytral length?  (we want the one on the x variable)

summary(my.log)	
my.log

#test statistic =  4.318 from the z statistic
#degrees of freedom = 80 (Null), 79 (Residual)
#p-value associated with coefficient = 1.58e-05


#   5.) What is the significance of the overall model and how did you calculate this?

with(my.log, (null.deviance - deviance))								# Improvement in deviance over null model (just Bo)
with(my.log, df.null - df.residual)									# Degrees of freedom in the model
with(my.log, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

# 4.049984e-15 calculated above
# Using deviance we calculate the g statistics by comparing the model with and without the prediction to give us 
# a likelihood number of this pattern occuring randomly. 

#   6.) What does the coefficient tell you about the change in probability of a beetle being male or
# female for a given length of elytra?

# It is the change in the logit ratio for one change in x. We can see that as we move towards one (female) 0.9436
# the evidence points to females being more likely to have long antennas.  

