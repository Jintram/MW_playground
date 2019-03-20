
# Some super simnple functions custom written
# (Might be superfluous but I'm new to R :)
# -MW

# doesn't quite yet work as expected
simpleplotxy <- function(data,xx,yy){
  
  ggplot(data=data) +
    geom_point(mapping=aes(x=xx,y=yy))
  
}
  