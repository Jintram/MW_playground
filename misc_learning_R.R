
# p. 46 of advanced book
# conversions

# search and replace equivalent
fruit_list <- c('a','a','a','a','b','b','a')
lookup_names <- c(a='apple',b='berry')
lookup_names[fruit_list]

# for dataframe
grades<-c(1,2,3,1,2)
defs<-data.frame(grade=c(1,2,3), word=c('bad','medium','good'), passornot=c(F,T,T))

# option 1
defs[match(grades,defs$grade),]

# option 2
rownames(defs)<-defs$grade
defs[grades,]


# difference between = and <- is in scope

bla<-function(a1) { print(toString(a1)); b1=3; c1<-2; d1<<-3}
bla(a1=3) # a will only be available within function scope
bla(a1<-3) # makes a accesible after function call
# super-operator <<- makes d1 available outside function scope



