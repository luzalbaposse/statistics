#################################################
#                                               #
# Methods: Statistics                           #
# Lecture 1                                     #
# Example: Newspapers                           #
#                                               #
#################################################


# analysing the number of newspapers sold at a newsstand for 200 consecutive days.
# original list sorted by size: 
urliste<-c(rep(0,21),rep(1,46),rep(2,54),rep(3,40),rep(4,24),rep(5,10),rep(6,5))


# frequency distribution
#--------------------------------------
table(urliste)
plot(table(urliste),main="frequency distribution",xlab="sold newspapers",ylab="number of days (absolute)")
# the plot shows absolute frequencies

# show relative frequencies on the y-axis
tab<-table(urliste)
tab<-tab/sum(tab)

plot(tab,main="frequency distribution",xlab="sold newspapers",ylab="number of days (relative)")
tab

sum(tab) # check: should add up to one


# empirical distribution function
#--------------------------------------
cumsum(table(urliste)) # cumulated absolute frequency
cumsum(tab) # cumulated relative frequency

# both plots combined in one graph
op <- par(mfrow = c(1, 2), mgp = c(1.5, 0.8, 0), mar =  .1+c(3,3,2,1))
plot(tab,main="frequency distribution",xlab="sold newspapers",ylab="h(x)")
F1 <- ecdf(urliste)
summary(F1)
plot(F1, main="empirical distribution function", xlab="sold newspapers",ylab="H(x)")
par(op)


# ADD-ON
#--------------------------------------
# plot alternatives for the empirical distribution function
op <- par(mfrow = c(3, 1), mgp = c(1.5, 0.8, 0), mar =  .1+c(3,3,2,1))
F1 <- ecdf(urliste)
summary(F1)
plot(F1)
plot(F1, verticals = TRUE, do.points = FALSE)
knots(F1) 
summary.stepfun(F1) 
plot(F1, verticals = TRUE, col.points = "blue",col.hor = "red", col.vert = "bisque")
par(op)