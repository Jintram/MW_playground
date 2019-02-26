
# Some random stuff to learn R

library(tidyverse)
library(nycflights13)
library(dplyr)

source("/Users/m.wehrens/Documents/Scripts_R/simplefunctionsMW.R")

my_flights <- flights

# add additional parameters
#transmute(my_flights, gain  

# combine group_by with summarize
# Note that summarize will automatically take into consideration the grouping
# if present.
by_day = group_by(my_flights,year,month,day)
blabla=summarize(by_day, delay=mean(dep_delay, na.rm=TRUE))

# See correlation between distance and duration of delay -------------
# this is example of p. 60 R for data science book
by_dest <- group_by(flights, dest)
delay <- summarize(by_dest, 
                   count=n(),
                   dist = mean(distance, na.rm=TRUE),
                   delay = mean(arr_delay, na.rm=TRUE)
)
# remove destinations with small nr observations, and honolulu which is far away
delay<-filter(delay,count>20,dest!="HNL")

# Make a little plot
ggplot(data=delay) +
  geom_point(mapping=aes(x=dist,y=delay))


# above can be done way faster using pipe --------------
# This creates a pipeline fo transforming one thing into the next
# Constant renaming is not necessary, and the first argument needs
# to be left out of the function that is used, as this is assumed to 
# process the output of the previous operation
delay <- flights %>% group_by(dest) %>%
                 summarize(count=n(),
                   dist = mean(distance, na.rm=TRUE),
                   delay = mean(arr_delay, na.rm=TRUE)
) %>%
filter(count>20,dest!="HNL")

# Pipe concept
x <- pi
y <- cos(x)
z <- sin(y)
# equals
z <- pi %>% cos() %>% sin()


# Make a little plot
ggplot(data=delay) +
  geom_point(mapping=aes(x=dist,y=delay))





# plotting a bar graph with an inverted sorted axis
# Key is that x axis will be sorted, so you have to give your own
# sorting, using levels of the factor type.
testdata=data_frame(c=factor(c(1,2,3),levels=c(3,2,1)),y=c(10,4,2))
ggplot(data=testdata, mapping=aes(x=c, y=y))+
  geom_bar(stat="identity")





# using the apply function to matrix rows
myfun<-function(x,y){
  x+y
}
to_add = c(1,2,3)
test_matrix=matrix(c(1,1,1,
                     2,2,2,
                     3,3,3),ncol=3)
test_result<-apply(test_matrix,2,myfun,y=c(1,2,3))







