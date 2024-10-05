



# Simple slice example
# note that rows here could be interpreted as genes
# columns are slices, where sigma1 is typical expression of 
# a certain cell type
# s1 is the "actual" slice that needs to be fitted
slice_data_simple_example = data.frame(
    sigma1= c(1, 0, 0, 1, 0, 2),
    sigma2= c(0, 1, 0, 1, 1, 0),
    s1= c(1, 2, 0, 3, 2, 2)) # sigma1+2*sigma2
lm_out = lm(s1 ~ sigma1+sigma2, data = slice_data_simple_example)
lm_out$coefficients
    # indeed, the proper information is extracted


# bad-fitting example
slice_data_simple_example2 = data.frame(
    sigma1= c(1, 0, 0, 1, 0, 2),
    sigma2= c(0, 1, 0, 1, 1, 0),
    s1= c(1,2,1,2,1,2)) # sigma1+2*sigma2
lm_out2 = lm(s1 ~ sigma1+sigma2, data = slice_data_simple_example2)
    # now, we expect this to be worse-fitting than the previous one, let's 
    # check this in lm_out
summary(lm_out)
summary(lm_out2)

lm_out$residuals
lm_out2$residuals
# can indeed be seen from residuals that first example is 
# much better fit than 2nd one.

