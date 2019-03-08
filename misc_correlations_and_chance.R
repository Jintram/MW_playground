


# Some illustrations of the principles behind correlations
# Specically, many points for which x=y, do not necessarily
# imply there is correlation.


# Generate 10k random 0 or 1 for x and y
# 50% overlap, but no correlation
x<-round(runif(10000,0,1))
y<-round(runif(10000,0,1))
# Correlate them
cor(x,y)
# R = 0.004877287

# Many are the same, but no correlation
x<-as.numeric(runif(10,0,1)>0.95)
y<-as.numeric(runif(10,0,1)>0.95)
cor(x,y)

# Idem
x=c(rep(1,10000),rep(0,10000))
y=c(1,rep(0,20000-1))
cor(x,y)  

# Note that when there are many observations with low occurence
# of one of two states, there might be a coincidental strong 
# correlation
x=c(1,rep(0,200-1))
y=c(1,rep(0,200-1)) # expected to be seen when repeated 200++ times
cor(x,y)  


