#################################################
#                                               #
# Methods: Statistics                           #
# Lecture 1                                     #
# Example: Boxplot                              #
#                                               #
#################################################


# consider the number of new apartments in Switzerland (per 1000 inhabitants)
# source: http://www.bfs.admin.ch/bfs/portal/en/index/regionen/kantone/daten.html
canton<-c("ZH","BE","LU","UR","SZ","OW","NW","GL","ZG","FR","SO","BS","BL","SH",
          "AR","AI","SG","GR","AG","TG","TI","VD","VS","NE","GE","JU")
new.housing<-c(6.7,4.4,7.6,6.4,9.7,6.4,6.4,3.6,6.3,7.8,5.6,1.0,3.9,6.6,6.5,
               3.5,7.0,11.7,6.8,8.2,6.6,6.4,8.2,3.3,2.7,6.2)
housing.starts<-data.frame(canton,new.housing)


# mean
#--------------------------------------
mean(housing.starts$new.housing)

# median
#--------------------------------------
median(housing.starts$new.housing)

# mode
#--------------------------------------
mean(housing.starts$new.housing)
# defining a new mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
Mode(housing.starts$new.housing)

# sample variance
#--------------------------------------
var(housing.starts$new.housing) # using the specific function of R
variance<-function(x) sum((x-mean(x))^2)/(length(x)-1)
variance(housing.starts$new.housing)

# mean absolute deviation
#--------------------------------------
mad(housing.starts$new.housing) # using the specific function of R
Mad<-function(x) sum(abs(x-mean(x)))/(length(x))
Mad(housing.starts$new.housing) 

# range
#--------------------------------------
range(housing.starts$new.housing)
diff(range(housing.starts$new.housing))

# quantiles
#--------------------------------------
quantile(housing.starts$new.housing)  
IQR(housing.starts$new.housing)# quantile 3- quantile 1 (interquartile range)

# boxplot
#--------------------------------------
boxplot(housing.starts$new.housing, col = "lightgray",horizontal = FALSE,
        main = "New Apartments in Switzerland",ylab = "per 1000 residents")

# variance
#--------------------------------------
# step-by-step calculations
housing.starts<-data.frame(canton,new.housing)

# step 1
arithmetic.mean<-function(x) sum(x)/length(x) 
housing.starts$average<-arithmetic.mean(housing.starts$new.housing)

# step 2
housing.starts$difference<-housing.starts$new.housing-housing.starts$average

# step 3
housing.starts$diff.squared<-housing.starts$difference^2

# step 4
sum(housing.starts$diff.squared)/(length(housing.starts$diff.squared)-1)