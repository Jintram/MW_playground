

################################################################################
# How to calculate mean by category

mydata <- data.frame(x=c(1,2,3,4),y=c(5,5,8,100),category=c(1,1,2,2))
aggregate(mydata, by=list(mydata$category), mean)
aggregate(mydata$x, by=list(mydata$category), mean) # only for x


################################################################################
# and per bin
# TODO: this is copied from other code and doesn't work as example yet

bin_assignment <- cut(x=my_df$Pseudotime, 5)
mean_Expression <- tapply(my_df$Expression, bin_assignment, mean)



