lapply(tomoseq_data,typeof)

lala=data.frame(matrix(as.integer(c(1,2,3,4)), nrow = 2))
typeof(lala[2,])

lala=data.frame(matrix(as.double(c(1,2,3,4)), nrow = 2))
expr=lala[2,]
typeof(expr)

lala=data.frame(a=c(1,2), b=c(3,4))
expr=lala[2,]
typeof(expr)
typeof(lala[2,])
typeof(lala[,2])

lala=matrix(c(1,2,3,4),nrow=2)
typeof(lala[2,])
typeof(lala[,2])

ggplot(data.frame(x=lala[2,]))+
    geom_histogram(aes(x=x))




