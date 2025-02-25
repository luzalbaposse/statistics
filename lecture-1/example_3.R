#################################################
#                                               #
# Methods: Statistics                           #
# Lecture 1                                     #
# Example: Measures of Dispersion               #
#                                               #
#################################################


# variance
#--------------------------------------
# defining a new variance function
variance<-function(x) sum((x-mean(x))^2)/(length(x)-1)
# example: returns (in %) of different stocks in an portfolio
returns<-c(2.8,7.0,1.6,0.4,1.9,2.6,3.8,3.8)
variance(returns)

# step-by-step calculations:
returns<-data.frame(returns)

# step 1: additional vector with the calculated mean
arithmetic.mean<-function(x) sum(x)/length(x)
returns$average<-arithmetic.mean(returns$returns)

# step 2: additional vector with differences of  values and the mean 
returns$difference<-returns$returns-returns$average

# step 3: additional vector with squared differences
returns$diff.squared<-returns$difference^2
returns

# step 4: variance calculation
sum(returns$diff.squared)/(length(returns$diff.squared)-1)
