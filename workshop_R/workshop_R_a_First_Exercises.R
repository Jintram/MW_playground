

################################################################################
#
# R workshop
# Part 1: Basics of data manipulation and programming
# ===
# This script contains the exapmles that I have used during my 1st workshop
# on R programming.
# The workshop is targeted to people who have looked at R, but are stil
# novice.
# I will also use some examples / material that I have adapted from the
# online workshop material at Software Carpentry:
# https://swcarpentry.github.io/r-novice-inflammation/01-starting-with-data/index.html
#
# 2019-11-25, m.wehrens@hubrecht.eu
#
################################################################################
# My first line of coding

print('hello world')

################################################################################
# Different types of variables

# a number
# ===
a = 1
a <- 1 # ±equivalent

# a string
# ===
my_name <- 'Martijn'

# vector
# ===
my_numbers = c(1,2,3,4,6,8)

office_members = c('Anko','Jingchao','Martijn','Pim','Jantine',
    'Eirini','Anne','Lieneke')
hot_office_members = c('Brian', 'Jenny', 'Maya', 'Su Ji', 'Arwa', 'Kees')

# factor
# ===
fruit_data_labels = factor(x=c('orange','orange','apple'),levels=c('orange','apple','banana'))
as.factor(my_numbers)
    # use also, as.integer, as.matrix, as.vector, ...

# list
# ===
example_list = list('a',1,2,3,'b')

some_thing = example_list[1] # select part of the list retaining identity
typeof(some_thing)

some_thing = example_list[[1]] # select an item, result is type item
typeof(some_thing)

example_list2 = list('a',1,2,3,'b',c(1,2,3),list(1,2,3,'a','b'))

# what is correct?
# ===
# (a)
some_thing = example_list2[6]
some_thing[c(1,2)]
# (b)
some_thing = example_list2[[6]]
some_thing[c(1,2)]

# (b) is correct

# we can give items in list names (will be important later)
# ===
list_with_names = list(a=1, b='b', awesome_item = 'yay')

# we can also access items by their names, in two ways
# ===
list_with_names$awesome_item

list_with_names[['awesome_item']]
# note you can also do this with a string
variable_with_name_in_list = 'awesome_item'
list_with_names[[variable_with_name_in_list]] # convenient in certain cases


# note we can construct combined statements
our_group_offices = list(office_members=office_members, hot_office_members=hot_office_members,
    group_leader='eva')

# matrix
# ===
my_example_matrix = matrix(c(1,2,3,4,5,6),nrow = 2)
my_example_matrix
View(my_example_matrix)

my_example_matrix2 = matrix(c(1,2,3,4,5,6),nrow = 2, byrow = T)
my_example_matrix2

my_example_matrix[,1]
my_example_matrix[c(1,2),]
my_example_matrix[c(T,F),]



# data frame (similar to matrix, but structure both more rigid and flexible)
# ===
office.data = data.frame(
    office_names = c('my_office', 'hot office', 'cold office'),
    average_temperature = c(20, 22, 18),
    square_meters = c(NA, NA, NA)
    )
# add some more complicated thing
office.data$office_members = list(office_members, hot_office_members, NA)
    # ^ can't be done immediately because data.frame interpratation of lists
office.data['nr_sockets'] = c(NA,NA,NA)

View(office.data)

# looking at type of data
# ===
typeof(my_example_matrix)
is.matrix(my_example_matrix)
is.data.frame(my_example_matrix)

################################################################################
# Some typical things you do in programming
# ===
# This won't cover everything, but I'd like to give you a feel right now
# and understand some basic things going on

# You can define parameters and
# perform mathematical operations
# ===
x1 <- 1
x2 <- 3

y <- x1+x2
y # 4

x1+x2^2 # 10

c(x1,x2)+2 # 3 5


# concept of a funcion
xplusy = function(x,y) {

  z = x+y

  return(z)

}

# print something ten times
#===

# The function seq
one_to_ten <- seq(1,10)
one_to_ten <- 1:10

# making a loop
#for (idx in seq(1,10)) {
#for (idx in c(1,333,10)) {
for (idx in seq(2,10)) {

 print( paste(  toString(idx),  ' hello' ) )

}


# if statements
a<-1
b<-3
if (a==1) {

  b<-b+20
  print('hoi')
  b

}

# some other logical statements
1 == 1
1 != 2
c(0,1,0,1) | c(0,0,1,1)
c(0,1,0,1) & c(0,0,1,1)


# logical operators
my_vector = c(1,1,1,2,3,4,1,1,2)
1 == my_vector






LakeHuron_df = data.frame(
    heights = LakeHuron,
    years   = )

################################################################################
# examples to do

# =====
# assignment: select data from a data frame that's from condition 1
#
# list of conditions
annotation_conditions = c(1,1,1,2,2,2,3,3,3,1,1)
# generate some random 'data'
my_data_matrix = matrix(runif(11*22), nrow = 11)
    # Note: each i-th row corresponds to a measurement
    # from condition annotation_conditions[i]

my_data_matrix[annotation_conditions==1,]


# =====
# assignment: function that returns fibonacci nth
# element (starting from 1 and 1)
fibonacci_nth = function(N) {

    number_previous1 = 1
    number_previous2 = 1

    if (N<=2) {
        stop('Can\'t do that.')
    }

    for (idx in 1:(N-2)) {

        number_new = number_previous1 + number_previous2
        number_previous2 = number_previous1
        number_previous1 = number_new

    }

    return(number_new)

}

unlist(lapply(3:10,fibonacci_nth))

# =====
# 2nd implementation with a bit more comments



# As a reference, the first few fibonacci numbers
# 1 1 2 3 5 8

# a function to calculate the N-th fibonacci number
fibo = function(N) {

    # define row 1 and 2
    fibonacci_n_minus1 = 1
    fibonacci_n_minus2 = 1

    # repeat calculation of next number
    for (idx in 3:N) {

        # new current number is sum previous two
        fibonacci_current = fibonacci_n_minus1 + fibonacci_n_minus2

        # update what are the last two numbers
        fibonacci_n_minus2 = fibonacci_n_minus1
        fibonacci_n_minus1 = fibonacci_current

        # (note that we now proceed to the next loop with updated parameter values)

    }

    # after all loops return the current number
    return(fibonacci_current)

}

# try out the function
fibo(5)
fibo(10)

# =====
# and now adapt that function such that it returns
# the first n elements of the series
# tip: you can add element to the end of an array
# by simply putting it in the (n+1)th element,
# n being the current length of the array

fibonacci_series = function(N=10) {

    fibonacci_series = c(1,1)

    if (N>2) {

        for (idx in 2:(N-2)) {

            fibonacci_series[idx+1] = fibonacci_series[idx-1]+fibonacci_series[idx]

        }

    }

    return(fibonacci_series)

}

fibonacci_series(20)

# =====
# assignment:  function that returns the maximum of a vector, without using "max"
# hint: use ">"
# hint: use length() to determine the length of a vector
give_maximum = function(input_vector) {

    my_maximum = -Inf

    for (idx in 1:length(input_vector)) {

        if (input_vector[idx]>my_maximum) {
            my_maximum=input_vector[idx]
        }

    }

    if (my_maximum==-Inf) { my_maximum = NA }

    return(my_maximum)

}

give_maximum(c(-10,11,12,1,1,-100,67,10))

# =====
# assignment: function that returns the maximum of each
# row of a matrix, using previous function
# tip: use the "dim()" function to find out size of
# input matrix
# tip: "output_vector = double()" creates an empty
# output vector

give_maximum_each_row = function(input_matrix) {

    output_vector = double()

    for (idx in 1:dim(input_matrix)[1]) {

        output_vector[idx] = give_maximum(input_matrix[idx,])

    }

    return(output_vector)

}

give_maximum_each_row(matrix(c(1,2,3,4,5,6),nrow=2))

# for such a purpose the apply function is very convenient
example_matrix = matrix(c(1,2,3,4,5,6),nrow=2)
apply(example_matrix,1,give_maximum)
apply(example_matrix,2,give_maximum)

# =====
# assignment: function that calculates the square root,
# approximated by an integer value, without using sqrt
# tip: the function break() to stop loop execution
# tip: the function ceil() rounds a number up
# tip: i'm not looking for a smart solution; just try
# OPTIONAL: add a decimal point

give_sqrt = function(input_nr) {

    for (nr_to_try in 1:ceiling(input_nr)) {

        if ((nr_to_try)*(nr_to_try) > input_nr) {
            output_sqrt = nr_to_try-1
            break
        }

    }

    return(output_sqrt)

}

give_sqrt(99)

# =====
# Assignment: Prime numbers
prime_yes_no = c(T)

for (nr in 2:100) {
    
    prime_yes_no[nr] = T
    
    for (denominator in 2:(nr-1)) {
        
        print(paste0('testing ', nr, '/', denominator))
        if ((nr %% denominator) == 0) {
            prime_yes_no[nr] = F
        }
        
    }
    
}
which(prime_yes_no)

my_number = 56
prime_yes_no=F
for (denominator in 2:(my_number-1)) {
        
        if ((my_number %% denominator) == 0) {
            prime_yes_no = F
        }
        
}
prime_yes_no

# =====
# OPTIONAL (personal favorite)
# Assignment: assuming production and breakdown rates of a protein, the first being
# subject to random fluctuations, create a function that generates a time
# series that gives the concentration of a protein over time
# It's convenient to make a loop that goes over the desired time steps (e.g. 100)
# and calculates the new concentration based on the old concentration
# and the production/degradation rates.
# The breakdown rate is a percentage of the protein being removed each
# time step; the production rate is an absolute number produced per time unit.
# The latter is influenced by noise.
# Tip: to model noise, use "(runif(1)-0.5)" to generate random
# numbers between 0.5 and 1.5

protein_concentation_series = function() {

    production_rate       = 10
    degradation_rate      = .5

    concentrations_over_time = c(100)

    for (time_idx in 2:100) {

        noise_percentage = (runif(1)-0.5)

        concentrations_over_time[time_idx] =
            concentrations_over_time[time_idx-1] +
            production_rate*(1+noise_percentage) -
            degradation_rate * concentrations_over_time[time_idx-1]


    }

    return(concentrations_over_time)

}

protein_concentation_series()

ggplot(data.frame(time=1:100,concentrations_over_time=protein_concentation_series()))+
    geom_line(aes(x=time,y=concentrations_over_time))+
    geom_point(aes(x=time,y=concentrations_over_time),size=.3,color='black')+
    theme(text = element_text(size=20))

SHOW WITH LAPPLY HERE



################################################################################

# data examples
# ===
# See all built-in ones with:
data()
# e.g.
View(EuStockMarkets)
View(presidents)
# load
data(EuStockMarkets)
data(LakeHuron)

# look at cars that begin with the letter M
cars_letter_m = grepl('^M',rownames(mtcars))
cars_letter_m
mtcars[cars_letter_m,]

# plot
library(ggplot2)
ggplot(data=data.frame(x=1:(dim(EuStockMarkets)[1]), DAX=EuStockMarkets[,1]))+
    geom_line(mapping=aes(x=x,y=DAX))
# EXERCISE, apply to LakeHuron dataset
ggplot(data=data.frame(year=1875:1972, height=LakeHuron))+
    geom_line(mapping=aes(x=year,y=height),size=1)+
    # add some additional things to make it prettier
    ggtitle('Level of Lake Huron 1875-1972')+
    xlab('Year')+ylab('Height [feet]')+
    theme(text = element_text(size=20))


#

PLOT FIBONACCI SERIES

#

PLOT PROTEIN FLUCTUATION SERIES



# Illustration of loop
for (idx in 1:10) {
 print('Hello world')   
}



# Illustration of if-statement
life=T
if (life) {
    print('Yay, I am alive!')
} 

if (life) {
    print('Yay, I am alive!')
} else {
    print('Oh no, I am dead.')
}






