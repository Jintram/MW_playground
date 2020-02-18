
################################################################################
# Some exercises regarding loops
# ====

# Canonical loop
# ===
# A loop is a piece of code that is executed
# multiple times; during that execution, a 
# parameter that you choose takes on different
# values that you set.
# Below, the parameter "idx" is set to 
# values 1 to 10, and for each of those
# values, the code inside the loop is executed.
# *

# Definition of the loop
for (idx in 1:10) { 
    
    # Code inside the loop that gets 
    # executed each loop
    print("Hello world")
    
}


# Exercise:
# The following loop prints the value of the index; 
# modify the loop such that it prints the square
# of that value
for (idx in 1:10) { 
    print( idx )
}

# solution 1
for (idx in 1:10) {
    print( toString(idx^2) )
}
# solution 2
for (idx in (1:10)^2) {
    print( toString(idx) )
}
# solution 3 (not applicable in some other programming languages)
for (idx in 1:10) {
    idx=idx^2
    print( toString(idx) )
}


# Exercise:
# The following loop goes over the entries in 
# "my_vector"; set my_vector such that those
# values are 'apple', 'banana', 'banana',
# 'pineapple', 'orange' and 'orange'.
# This should result in the loop printing 
# those values.
# Note: make sure these are recognized
# by R as characters
my_vector = c(XXX)
for (value in my_vector) { 
    print( value )
}

# solution
my_vector = c('apple', 'banana', 'banana', 'pineapple', 'orange', 'orange')
for (value in my_vector) { 
    print( value )
}


# Exercise
# Modify the following loop such that the
# values over which the loop goes 
# (12,3,64,23,12) are summed in
# the parameter a.
# That is to say, when the loop execution
# is done, a should have value 114.
a = 0
for (value in c(12,3,64,23,12)) { 
    a = XXXX
}
a

# solution
a = 0
for (value in c(12,3,64,23,12)) { 
    a = a + value
}
a

# solution (making it more complex but more explicit)
a = 0
vector_1 = c(12,3,64,23,12)
for (idx in 1:length(vector_1)) { 
    print(toString(value))
    a = a + vector_1[idx]
}
a

# Exercise
# Execute the following code
my_vector = c() # init vector
my_vector[1] = 1 # add element by putting it at end
my_vector[2] = 2
# This will initialize a vector, and add two values
# using the fact that R can add values to an arbitrary
# position in a vector (values that are not set 
# will be filled with NA values).
# Now change the following loop such that this process
# is automaically continued such that the vector
# my_vector will contain values from 1-1000.
for (value in XXX) { 
    my_vector[YYY] = ZZZ
}

# solution
my_vector = c() # init vector
my_vector[1] = 1 # add element by putting it at end
my_vector[2] = 2
for (value in 3:1000) { 
    my_vector[value] = value
}





