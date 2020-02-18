
# See also:
# https://stackoverflow.com/questions/20251119/increase-the-size-of-variable-size-points-in-ggplot2-scatter-plot

library(ggplot2)

my_df = data.frame(
        x=runif(10),
        y=runif(10),
        z=runif(10))

ggplot(my_df
        )+
    geom_point(aes(x=x,y=y,size=z))+
     scale_size_continuous(range = c(.25, 4))

ggplot(my_df
        )+
    geom_point(aes(x=x,y=y,size=z))+
     scale_size_continuous(range = c(2, 20))