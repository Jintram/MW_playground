
# Some personal notes on the melt function

# There's actually two melt functions, 
# melt.data.frame
# melt.array
# the latter also being used for matrices etc.
# These have different properties
# 
# On top of this, there are two libraries that supply the melt
# function, which behave similar, but not entirly similar.
#
# There's reshape and there's reshape2
# reshape::melt.array() outputs X1, X2 as id.var names
# reshape2::melt.array() outputs Var1 and Var2 as id.var names

# Examples of things being different:

example_df = data.frame(x=c(1,2,3), y=c(234,212,123), z=c(9,9,9), q=c(666,666,666))
example_matrix = as.matrix(example_df)

library(reshape)
library(reshape2)

reshape::melt(example_df)
reshape2::melt(example_matrix)

reshape::melt(example_matrix, id.vars='x', measure.vars = c('y','z'))
reshape::melt(example_matrix, id.vars=c('x','q'), measure.vars = c('y','z'), varnames = c('hoi','doei'))

reshape2::melt(example_df, id.vars='x', measure.vars = c('y','z'))
reshape2::melt(example_df, id.vars='x', measure.vars = c('y','z'), variable.name = 'hoi')

reshape2::melt(example_matrix, id.vars='x', measure.vars = c('y','z'))
reshape2::melt(example_matrix, id.vars=c('x','q'), measure.vars = c('y','z'), varnames = c('hoi','doei'))

