##Solution for the problem 2 of hands-on Takens##

library(rEDM)
library(scatterplot3d)

source("https://raw.githubusercontent.com/mathbio/edmTutorials/master/utilities/make_block.R") ##make_block function##

lag<-3
tau<-1
##the variables to be passed to the function##

problem_time_series<-read.csv("https://raw.githubusercontent.com/mathbio/edmTutorials/master/takens/problem-time-series.csv")
problem_time_series<-as.data.frame(problem_time_series)
##to transform the csv into a data frame##

prob_block<-make_block(problem_time_series, max_lag = lag, tau = tau)
##with the default tau, tau = 1, the reconstruction will be compressed ##

scatterplot3d(prob_block$X, prob_block$X_1, prob_block$X_2, type =  "l")
##scatterplot of the three series, original and two more lags##
##try to change the tau variable in the make_block fucntion, this will help to unravel the attractor##

##an optimal tau for this problem wil be between 5 and 20##
prob_block<-make_block(problem_time_series, max_lag = lag, tau = 5)
scatterplot3d(prob_block$X, prob_block$X_5, prob_block$X_10, type =  "l", main =  "The Lorenz Attractor")

##This is the Lorenz attractor##