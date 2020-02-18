

# This will print a message
print('hello world')


# Different types of variables

# a number
a = 1

# a string
my_name <- 'Martijn' # string

# vector
my_numbers = c(1,2,3,4,6,8)

office_members = c('Anko','Jingchao','Martijn','Pim','Jantine',
    'Eirini','Anne','Lieneke')
hot_office_members = c('Brian', 'Jenny', 'Maya', 'Su Ji', 'Arwa', 'Kees')

# examples with lists
example_list = list('a',1,2,3,'b')

some_thing = example_list[1] # select part of the list retaining identity
typeof(some_thing)

some_thing = example_list[[1]] # select an item, result is type item
typeof(some_thing)

example_list2 = list('a',1,2,3,'b',c(1,2,3),list(1,2,3,'a','b'))
some_thing = example_list2[6]
some_thing[c(1,2)] # oops
some_thing = example_list2[[6]]
some_thing[c(1,2)] # correct

# we can give items in list names (will be important later)
list_with_names = list(a=1, b='b', awesome_item = 'yay')
# we can also access items by their names, in two ways
list_with_names$awesome_item
list_with_names[['awesome_item']] 
variable_with_name_in_list = 'awesome_item'
list_with_names[[variable_with_name_in_list]] # convenient in certain cases

our_group_offices = list(office_members=office_members, hot_office_members=hot_office_members,
    group_leader='eva')
    
# matrix

# data frame




x <- 7
y <- 3

# print something ten times
one_to_ten <- seq(1,10)

for (idx in seq(1,10)) {
  
 print( paste(  toString(idx),  ' hello' ) )
  
}


xplusy<-function(x,y) {
  
  z<-x+y
  
  return(z)
  
}

a<-1
b<-3
if (a==1) {
 
  b<-b+20
   
}







