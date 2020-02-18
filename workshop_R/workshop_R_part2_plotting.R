


################################################################################
# Reading in the data

# Use the read.csv function
my_temperature_data  =
  read.csv(file = '/Users/m.wehrens/Documents/Presentations/POWERPOINT/_R_workshop/weather_data_timeanddata-com.txt',
      sep = "\t")

# To see whether it has worked properly..
View(my_temperature_data)

################################################################################
# An alternative strategy, directly reading excel

library(openxlsx)
library(xlsx)
my_temperature_data2  =
  openxlsx::read.xlsx(xlsxFile = '/Users/m.wehrens/Documents/Presentations/POWERPOINT/_R_workshop/weather_data_timeanddata-com.xlsx')
View(my_temperature_data2)
my_temperature_data3  =
  xlsx::read.xlsx(file = '/Users/m.wehrens/Documents/Presentations/POWERPOINT/_R_workshop/weather_data_timeanddata-com.xlsx',
    sheetName = 1)
View(my_temperature_data3)
  # Note that both these methods also require some additional wrangling

################################################################################
# A simple plot

# if you don't have it, install it with:
install.packages('ggplot2')

# Let's plot the data
library(ggplot2)
ggplot(data = my_temperature_data)+
  geom_point(mapping = aes(x=Time,y=temperature))

################################################################################
# Let's go over some parameters and cast them in the right form

# template for the exercise
# how to fil a new vector
new_vector = double() # creates a vector with 0 entries
# now do something 10 times
for (idx in 1:10) {
  # an element can be added to the end of the list by
  # addressing the n+1-th element in the list
  new_vector[idx] = idx
}

# One can also convert parameter types
some_string = '15'
typeof(some_string) # character
some_double = as.double(some_string)
typeof(some_double) # double

# Recalculate the temperatures
strsplit_out =
  strsplit(as.character(my_temperature_data$temperature),
    split=' ')
# loop over output
my_temperatures = double()
for (idx in 1:length(strsplit_out)) {
  my_temperatures[idx] = as.double(strsplit_out[[idx]][1])
}
my_temperature_data$Temperature2 = my_temperatures

# Recalculate the times
strsplit_out = strsplit(as.character(my_temperature_data$Time),
  split=':')
#
my_times = double()
for (idx in 1:length(strsplit_out)) {
  my_times[idx] = as.double(strsplit_out[[idx]][1])+
                  as.double(strsplit_out[[idx]][2])/60
}
my_temperature_data$Time2 = my_times

################################################################################
# Basic plot

ggplot(data = my_temperature_data)+
  geom_point(mapping = aes(x=Time2,y=Temperature2))

################################################################################
# Prettier plot

ggplot(data = my_temperature_data)+
  geom_point(mapping = aes(x=Time2,y=Temperature2))+
  geom_line(mapping = aes(x=Time2,y=Temperature2))+
  ylab('Temperature (C)')+
  xlab('Time (hours)')+
  ggtitle('Yesterday\'s temperature')+
  theme(text = element_text(size=15),
        axis.text.x = element_text(angle=90, hjust=1))

################################################################################
# Some more plots

# Wrangle the humidity
strsplit_out =
  strsplit(as.character(my_temperature_data$Humidity),
    split='%')
# loop over output
my_humidity = double()
for (idx in 1:length(strsplit_out)) {
  my_humidity[idx] = as.double(strsplit_out[[idx]][1])
}
my_temperature_data$Humidity2 = my_humidity

ggplot(data = my_temperature_data)+
  geom_point(mapping = aes(x=Time2,y=Temperature2,color=Humidity2),size=4)+
  geom_line(mapping = aes(x=Time2,y=Temperature2))+
  ylab('Temperature (C)')+
  xlab('Time (hours)')+
  ggtitle('Yesterday\'s temperature')+
  ylim(c(0,10))+
  theme_bw() + # I like theme_classic() or theme_bw()
  theme(text = element_text(size=15),
        axis.text.x = element_text(angle=90, hjust=1))

ggplot(data = my_temperature_data)+
  geom_bar(mapping = aes(x=Time2,y=Temperature2,fill=Humidity2),size=4,stat='identity')+
  ylab('Temperature (C)')+
  xlab('Time (hours)')+
  ggtitle('Yesterday\'s temperature')+
  ylim(c(0,10))+
  theme_bw() + # I like theme_classic() or theme_bw()
  theme(text = element_text(size=15),
        axis.text.x = element_text(angle=90, hjust=1))

ggplot(data = my_temperature_data)+
  geom_point(mapping = aes(x=Time2,y=Temperature2),size=4)+
  geom_smooth(mapping = aes(x=Time2,y=Temperature2),size=2)+
    # note: this function requires extra parameters, so it will throw a warning
    # and assume you want some default settings
    # you should also provide: geom_smooth(..., method = 'loess', formula 'y ~ x')
  ylab('Temperature (C)')+
  xlab('Time (hours)')+
  ggtitle('Yesterday\'s temperature')+
  ylim(c(0,10))+
  theme_bw() + # I like theme_classic() or theme_bw()
  theme(text = element_text(size=15),
        axis.text.x = element_text(angle=90, hjust=1))

################################################################################

# create a new parameter that distinguishes night and day times
# this line creates an empty factor parameter
my_temperature_data$dayornight = NA
  # use "View(my_temperature_data)" to see what I've done here
  # I create a new column, but since I only supply 1 value
  # R will just repeat that value and make all entries "NA"
# now make rows that correspond to daytime have the value 'day'
indices_corresponding_to_daytime = my_temperature_data$Time2<20 & my_temperature_data$Time2>8
my_temperature_data[indices_corresponding_to_daytime,]$dayornight = 'day'
# now make rows that correspond to nighttime have the value 'night'
# do this in a shorter way
my_temperature_data[my_temperature_data$Time2>20|
    my_temperature_data$Time2<8,]$dayornight = 'night'
  # note that I defined night and day a bit arbitrarily

# plot the distributions
# (note that we are missing the time information now!)
ggplot(data = my_temperature_data)+
  geom_violin(mapping = aes(x=dayornight,y=Temperature2))+
  geom_jitter(mapping = aes(x=dayornight,y=Temperature2,color=Humidity2))+
  geom_boxplot(mapping = aes(x=dayornight,y=Temperature2), alpha=.3)+
  ylab('Temperature (C)')+
  xlab('When')+
  ggtitle('Yesterday\'s temperature')+
  ylim(c(0,10))+
  theme_bw() + # I like theme_classic() or theme_bw()
  theme(text = element_text(size=15),
        axis.text.x = element_text(angle=90, hjust=1))

# Make a simple histogram
ggplot(data = my_temperature_data)+
  geom_histogram(mapping = aes(x=Temperature2),bins=30)+
  theme(text = element_text(size=15))

################################################################################
# Example of pie chart with ggplot
test_data=data.frame(group=c(1,1,1,1,12,23,1,12,12))
ggplot(test_data, aes(x=factor(1), fill=factor(group)))+
  geom_bar(width = 1)+
  coord_polar("y")

