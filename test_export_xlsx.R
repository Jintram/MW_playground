
# create some test data
X = data.frame(a=c(1,2,3), b=c(4,5,6))


# testing the two export libraries

# library 1
# First run:
# install.packages('xlsx')
library(xlsx) 
xlsx::write.xlsx(x = X, file = '/Users/m.wehrens/Desktop/test.xlsx')

# library 2
# First run:
# install.packages('openxlsx')
library(openxlsx) 
openxlsx::write.xlsx(x =  X, file = '/Users/m.wehrens/Desktop/test-open.xlsx')