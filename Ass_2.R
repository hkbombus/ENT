##########      ENTM 642 Week 2 Assignment    #########
##########  Distributions & Transformations  #########
##########          Hannah Klein           ##########

setwd("~/Desktop/PURDUE/Spring21/ENT/RCode")

#F:/128 Sandisk backup/2021 Classes/ENTM642

"
Lab_2_assignment_data.csv contains count data on a longicorn beetle, Typocerus v. velutinus,
at research sites in the stateforests of Indiana as part of the Hardwood Ecosystem
Experiment (HEE). This super-cool beetle is the focus of the dissertation of a previous
TA for the course. You will test the null hypothesis of non-random distribution
across the landscape.
"

#install.packages(c("MASS","ggplot2"),repos = "http://cran.r-project.org")
library(MASS)						
library(ggplot2) 

#import data
my.data <- read.csv(file="Assignment_02_data.csv", sep=",", header=TRUE)  # ??? there are headers should we say TRUE
my.data
names(my.data)						## Look at the variable names

attach(my.data)
hist(typocerus)						## Look at data


"
1.) Plan how you will test the hypothesis. Do you need to transform the data? Why or why not?
"
print(
"
To test the hypothesis I will see if the the distribution of longicorn beetles is signficantly different from 
a random distribution of numbers using Poisson, to test this I will use the Chi-square goodness of fit.
")

"
2.) Plot a histogram of the data and the appropriate probability density function 
that you will test against, with both in the same plot. Label axes appropriately, 
make the null distribution and actual data in the graphs different colors, 
and provide a legend of these. [4]
"
rand_dist <- rpois(43, lambda = mean(typocerus) ) # create a random distribution, with poisson. Sample number?
poissdist <- rpois(500, lambda = mean(typocerus))

#on screen
par(mfrow=c(1,2))		
hist(typocerus,col = "red", density = 20,breaks = 6, main = "Typocerus Site Density", xlab = "Typocerus")
plot(density(rand_dist), col = "blue", main = "Poisson Density Functioin") 

#separate window 
dev.new(title = "Distributions", 
        noRStudioGD = TRUE
)
par(mfrow=c(1,2))						## Create frame for multiple plots 
hist(typocerus,col = "red", density = 20,breaks = 6, main = "Site Density of Typocerus", xlab = "Typocerus")
plot(density(rand_dist), col = "blue", main = "Poisson Density Functioin") 


#separate file
jpeg("Typvspoiss.jpg")			## Save the following to a jpg in the wdir
par(mfrow=c(1,2))						
hist(typocerus,col = "red", density = 20,breaks = 6, main = "Typocerus Site Density", xlab = "Typocerus")
plot(density(rand_dist), col = "blue", main = "Poisson Density Functioin") 
dev.off()		

#tried to use ggplots but I could not get this to run: 
#as.data.frame(my.data)
#ggplot(my.data, aes(y=typocerus))+
  #geom_histogram(x)


"
3.) What kind of analysis should you use to determine whether the beetles are 
distributed randomly? There are many possibilities (e.g., Fortin & Dale 2005), 
but use a method you saw in class. [2]"

print("
I need to test the goodness of fit, for this I will use the Chi-square test.This test is best when comparing results of observed and expected distrubutions -in this case the observed site distribution of Typocerus
and random distribution via. Poisson. If the Chi-square test returns a non signficant value we can assume that observed data
and the random distribution are not the same, and that the Typocerus are not randomly distributed.
")

"
4.) Perform the analysis and report the results (test statistic, degrees of freedom, 
p value). You can do this either with a canned routine if you find an appropriate 
command, or manually (but use R either way). [5]
"
plot(lm(site ~ typocerus, data = my.data))

test <- chisq.test(typocerus, rand_dist)
print (test)

mytab <- (table(typocerus, poissdist))
test2 <- chisq.test(mytab)

"
5.) What do you conclude regarding the hypothesis of non-random spatial distribution?
Be careful with how you phrase this.
"

print(
"
The test results are not signficant indicating there is not a goodness of fit and that Typocerus is not randomly distributed.
")
