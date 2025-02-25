#################################################
#                                               #
# Methods: Statistics                           #
# Lecture 1                                     #
# Example: Measures of Central Tendency         #
#                                               #
#################################################



# arithmetric mean
#--------------------------------------
# defining a new mean function
arithmetic.mean<-function(x) sum(x)/length(x)

# example: return (in %) of different stocks in an portfolio 
returns<-c(2.8,7.0,1.6,0.4,1.9,2.6,3.8,3.8)
arithmetic.mean(returns)
mean(returns) # using the specific function of R


# median
#--------------------------------------
# defining a new median function
med <- function(x){
            odd.even <- length(x)%%2
            if(odd.even==0){(sort(x)[length(x)/2]+sort(x)[1+length(x)/2])/2}
            else {sort(x)[ceiling(length(x)/2)]}
}
# (1) if - statement is true (even number): the mathematical expression
#     following the if statement is executed (median for even number of values)
# (2) if-statement is wrong (odd number, odd.even==1), then the 
#     mathematical expression executed after the else statement (median for odd number)
# (3) modulo function to test if even/odd number of values in vector:
#     -> odd number has modulo 2 with value 1
#     -> even number has modulo 2 with value 0
9%%2
8%%2

# example
med(returns) # vector with odd number of observations
median(returns) # using the specific function of R
# median as average of the mean values 2.6 and 2.8 
sort(returns)


# mode
#--------------------------------------
# defining a new mode function
mode <- function(x){
                ux<-unique(x)
                ux[which.max(tabulate(match(x,ux)))]
                    }
# example
mode(returns)