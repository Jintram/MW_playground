


################################################################################
# This document holds some explanation and exercises on the use of logicals,
# logicals tests, and if-statements.
#
# Part of Workshop series, m.wehrens@hubrecht.eu, 12-2019
#
################################################################################

################################################################################
# Logical tests

# A logical is a parameter that can only have two values, True or False.

# By using the "==" statement, we can evaluate whether something is true.
# The output is returned as a logical:
1==1 # Returns True
1==2 # Returns False

# Shorthand notations for True and False are "T" and "F", respectively

# When comparing two vectors, the test is automatically applied to all elements 
# in the two vectors in an element-wise fashion
c(1,1,1,1)==c(1,2,2,1) # Returns c(True,False,False,True), or abbreviated c(T,F,F,T)
c('a','b')==c('a','d') # Returns (T, F)



################################################################################






