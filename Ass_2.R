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

To test the hypothesis I will see if the the distribution of longicorn beetles is signficantly different from 
a random distribution of numbers using Poisson. I will not transfrom the data ??? 

"

"
2.) Plot a histogram of the data and the appropriate probability density function 
that you will test against, with both in the same plot. Label axes appropriately, 
make the null distribution and actual data in the graphs different colors, 
and provide a legend of these. [4]
"""
hist(typocerus)
rand_dist <- rpois(43, lambda = 245/43 ) # create a random distribution, with poisson. Sample number?
hist (rand_dist, col = "red", density = 20,breaks = 6, main = "Random Density")
plot(density(rand_dist)) # line graph of rand dist 
plot(density(typocerus))
"
3.) What kind of analysis should you use to determine whether the beetles are 
distributed randomly? There are many possibilities (e.g., Fortin & Dale 2005), 
but use a method you saw in class. [2]

I will use the Chi-square goodness of fit test. This test is best when comparing results of observed
and expected distrubutions, in this case the observed site distribution of Typocerus
and random distribution via. Poisson. 

"


"
4.) Perform the analysis and report the results (test statistic, degrees of freedom, 
p value). You can do this either with a canned routine if you find an appropriate 
command, or manually (but use R either way). [5]
"
test <- chisq.test(typocerus, rand_dist)
print ("The test results are: X-squared = 81.935, df = 81, p-value = 0.45")


"
5.) What do you conclude regarding the hypothesis of non-random spatial distribution?
Be careful with how you phrase this.
"
