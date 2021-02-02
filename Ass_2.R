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
plot(site,typocerus)						## Plot counts vs sites


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
hist(rpois(43, lambda = ), breaks = 6)

mean <- rnorm(100,40)
hist(mean, col = "red", density = 20)
my.tst_norm <- shapiro.test(my.means)

points(mean, type="l", col="blue")

"
3.) What kind of analysis should you use to determine whether the beetles are 
distributed randomly? There are many possibilities (e.g., Fortin & Dale 2005), 
but use a method you saw in class. [2]

"
my.data
my.test <- shapiro.test(typocerus) ## the results are significant p-value = 1.014e-12, which means it is NOT NORMAL


test.modeltyp <- lm(site ~ typocerus)			## Run a linear model
my.bc <- boxcox(test.modeltyp, lambda=seq(-2, 4, by=0.1) )	## Use the lm model object and range of lambdas in b-c (needs MASS package)
my.bc <- boxcox(test.modeltyp, lambda=seq(1.4, 1.6, by=0.01) ) ## 1.425

new.typ <- (typocerus^1.425)/1.425	
hist(new.typ)
shapiro.test(new.typ)  # still significant did not work 
hist(typocerus)	


"
4.) Perform the analysis and report the results (test statistic, degrees of freedom, 
p value). You can do this either with a canned routine if you find an appropriate 
command, or manually (but use R either way). [5]
"

"
5.) What do you conclude regarding the hypothesis of non-random spatial distribution?
Be careful with how you phrase this.
"
