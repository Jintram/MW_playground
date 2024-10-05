

# Some random code snippets copied from internet, see notes
# to further understand FA



# install.packages("psych")
library(psych)
data(iris)


iris_data <- iris[, 1:4]
iris_cor <- cor(iris_data)

library(pheatmap)
pheatmap(iris_cor)

my_fa <- fa(r = iris_cor, nfactors = 2)
print(my_fa)

# or the alternative with varimax rotation:
my_fa <- factor.pa(iris, nfactors=3, rotation="varimax")
print(my_fa)
# or the alternative with proimax rotation
my_fa <- factor.pa(iris, nfactors=3, rotation="promax")
print(my_fa)

##########
# MW

fa_out_test = 
    fa(r = iris_cor, nfactors=4)
fa_out_test2 = 
    fa(r = iris_data, nfactors=4)

df_summary_vacc = 
    as.data.frame(t(fa_out_test$Vaccounted))
df_summary_vacc$N = 1:nrow(df_summary_vacc)

ggplot(df_summary_vacc, aes(x=N, y=`Proportion Explained`))+
    geom_point()+theme_bw()+xlab('Factor')


iris_data[1:3,]
fa_out_test$loadings

fa_out_test$values
fa_out_test$residual
fa_out_test$weights

fa_out_test$
    
scree(iris_cor, pc=F)




# First tried to use the fa function from the psych library, but 
# I didn't quite understand the output (why are there not
# as many values as the rows in the data, e.g. in residuals?)


# Now trying factanal
factanal_out = factanal(iris_data, factors = 1)
    # output is similar, factanal only accepts 1 factor as option?!
    


# Note that PCA analysis output will also give x values, which
# are the data in the new space. Factor analysis doesn't seem
# to be doing this ..
pca_out =  prcomp(iris_data)
iris_data[1:4,]
View(pca_out)
pca_out$x[1:4,]
dim(pca_out$x)

