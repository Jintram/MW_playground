# Some basics of fitting and correlations --------------------
# See also http://mathworld.wolfram.com/Covariance.html
# and http://mathworld.wolfram.com/LeastSquaresFitting.html

x = seq(1,10)
y = 10+3*x

x_forfitline = seq(1,10)

# covariance is expected product of deviation from individual averages
cov_xy = cov(x,y)
# expected deviation for separate is the variance
var_x = var(x)
var_y = var(y)
# covariance says something relationship, variance doesnt, while sized are equal
# can be used to obtain normalized parameter that reflects correlation:
cor_xy = cor(x,y) # can be calculated by build-in function
cor_xy==(cov_xy/sqrt(var_x*var_y)) # but can also be calculated manually (this line confirms that)
# but it is also related to the least square fitting ()
lsf_b = cov_xy/var_x
lsf_a = mean(y)-lsf_b*mean(x)

df_forplot <- data.frame(x=x,y=y,x_fitted=x_forfitline,y_fitted=(x_forfitline*lsf_b+lsf_a) )

ggplot(data=df_forplot)+
  geom_point(aes(x=x,y=y))+
  geom_line( aes(x=x_fitted, y=y_fitted))








