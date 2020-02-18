

# Some final exercises

# What type of modifications would you make to the following code?
i=c(1,1); n=10
for (j in 3:n) {
 i[j]=i[j-2]+i[j-1]
}

# first we generate some fake data
my_data = matrix(c(1,2,3,4,5,
                   6,7,8,9,10,
                   11,12,13,14,15,
                   16,17,18,19,20),nrow=4,byrow = T)
rownames(my_data) = c('sham1','sham2','tab1','tab2')
colnames(my_data) = c('heart_size','tibia_size','weight','length','ejection_fraction')

# Now copy the code above, and add another fake measurement 
# first we generate some fake data
my_data = matrix(c(1,2,3,4,5,1,
                   6,7,8,9,10,1,
                   11,12,13,14,15,1,
                   16,17,18,19,20,1),nrow=4,byrow = T)
rownames(my_data) = c('sham1','sham2','tab1','tab2')
colnames(my_data) = c('heart_size','tibia_size','weight','length','ejection_fraction','nr_tails')


# the grepl function allows you to search in a vector of strings for partial
# matches; use this function to 
# (1) identify in which columns sham data is present
# (2a) convert the matrix to a data frame
# (2b) now add a column to the dataframe indicating whether
#      the data came from a tab or sham animal
# (3) using above information, determine the mean and
#     standard deviation of tibia size in sham animals
# (4a) use ggplot with the "geom_boxplot" function to 
#     visualize data on the ejection fraction for different 
#     conditions
# (4b) also add the function "geom_jitter" to your plot
# Note: perhaps google for e.g. "R example geom_boxplot" to understand
# how plotting works if you don't know by heart.
sham_ones = grepl('sham',rownames(my_data))
tab_ones = grepl('tab',rownames(my_data))

my_df = as.data.frame(my_data)
View(my_df)

my_df$annotation = 0
my_df$annotation[sham_ones] = 'sham'
my_df$annotation[tab_ones] = 'tab'

mean(my_df$tibia_size[sham_ones])
sd(my_df$tibia_size[sham_ones])

ggplot(my_df,aes(x=annotation,y=heart_size))+
    geom_boxplot()+
    geom_jitter()

ggplot(data=my_df,
        mapping=aes(x=annotation,y=heart_size))+
    geom_boxplot()+
    geom_jitter()


# use a loop to calculate the mean for all of the 
# parameters in the matrix we created earlier
# hint: the dim(my_data)[2] will give the number of 
# columns of the matrix "my_data"
the_means=c()
for (idx in 1:dim(my_data)[2]) {
    the_means[idx] = mean(my_data[,idx])
}

# Now use the apply function to do the same
the_means2 = apply(my_data,2,mean)

# Write a function that as input takes data from one mouse
# (e.g. my_df['tab1',]), and then calculates the BMI
# for that mice
# Perhaps start with this code:
#mouse_data = my_df['tab1',]
#give_BMI = function(mouse_data) { ..
give_BMI = function(mouse_data) {
    mouse_data$weight/mouse_data$length^2
}
give_BMI(my_df['tab2',])

# bonus: now create a loop to calculate
# the BMI for all mice

# Import the file netflix_titles_nov_2019.csv available at:
# https://surfdrive.surf.nl/files/index.php/s/So9rnz6YFEoaM55
file_path2 = '/Users/m.wehrens/Data/_2019_12_R_workshop_data/netflix_titles_nov_2019.csv'
netflix_data = read.csv(file = file_path2, sep = ',')
View(netflix_data)

# Now plot the number of tv-shows and movies released per year
# You'll need the aggregate function for this
# See e.g. here:
# https://stackoverflow.com/questions/21982987/mean-per-group-in-a-data-frame
# For an example
# Here is code applied to the data given:
nr_releases_per_yr = 
    aggregate(x = netflix_data[,'type'], by = list(netflix_data$release_year),
        FUN = length)
# roughly spoken, what happens is that aggregate divides
# the rows given in x in subsets, where each subset is determined
# by looking which entries in the list given by the parameter 'by'
# are the same. It then applies the function given by FUN
# to each of the subsets. I here used length to just determine
# the size of each of the groups
# 
# Now you can make the plot
#ggplot(...
ggplot(nr_releases_per_yr, aes(x=Group.1, y=x))+
    geom_line()

# bonus: now do this for shows and movies separately
my_selection = netflix_data$type=='TV Show'
nr_shows_per_yr = 
    aggregate(x = netflix_data[my_selection,'type'], 
        by = list(netflix_data$release_year[my_selection]), FUN = length)
my_selection = netflix_data$type=='Movie'
nr_movies_per_yr = 
    aggregate(x = netflix_data[my_selection,'type'], 
        by = list(netflix_data$release_year[my_selection]), FUN = length)

ggplot(mapping=aes(x=Group.1, y=x))+
    geom_line(data = nr_releases_per_yr)+
    geom_line(data = nr_shows_per_yr, color='blue')+
    geom_line(data = nr_movies_per_yr, color='red')+
    xlab('Year')+ylab('# Releases')
    # note for the expert: a neater way to do this would be 
    # using the "melt" function





