
library(ggplot2)

MAXVAL=100
x = seq(0,MAXVAL,MAXVAL/100000)
# y = 5*x^2
K_half = 0.1; n_H=4
y = x^n_H / (x^n_H + K_half^n_H)

ggplot(data.frame(x=x, y=y), aes(x=x, y=y))+
    geom_line()+
    xlim(c(0,MAXVAL))+
    ylim(c(0,1))+
    theme(axis.text.x = element_text(size=35),
          axis.text.y = element_text(size=35))

# log scale of the plot
ggplot(data.frame(x=x, y=y), aes(x=x, y=y))+
    geom_line()+
    xlim(c(0,MAXVAL))+
    ylim(c(0,1))+
    scale_x_log10()+
    theme(axis.text.x = element_text(size=35),
          axis.text.y = element_text(size=35))

# now manually rescale
log_x = log10(x) # x = (10^log_x)
y_log_x = (10^log_x)^n_H / ((10^log_x)^n_H + K_half^n_H)
#y_w_logx = 5*(10^log10(x))^2
#y_w_logx = 5*(10^log_x)^2
ggplot(data.frame(x=log_x, y=y_w_logx), aes(x=x, y=y))+
    geom_line()+
    theme(axis.text.x = element_text(size=35),
          axis.text.y = element_text(size=35))




