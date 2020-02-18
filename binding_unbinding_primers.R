
################################################################################

# Approximate plotting of temperate-rate relation of binding/unbinding
# of primers to target.


################################################################################

library(ggplot2)
source('/Users/m.wehrens/Documents/git_repos/SCS_RaceID3/Functions/MW_standard_plots.R')

#fun_kon  = function(x) {exp(1/x)}
#fun_koff = function(x) {1/exp(1/x)}

################################################################################

k0_on=7e4
k0_off=7e4*1e24 # Interpretation lacking
HA_on=-12.5
HA_off=25
R=0.00198375 #8.3
fun_kon  = function(Temp) {k0_on*exp(-HA_on /(R*Temp))}
fun_koff = function(Temp) {k0_off*exp(-HA_off/(R*Temp))}

ggplot(data = data.frame(x = 0))+ 
    stat_function(fun = fun_kon, aes(x=x, color='k_on')) +
    stat_function(fun = fun_koff, aes(x=x,color='k_off')) +
        xlim(300,350)+xlab('Temperature')+ylab('Rate')+
        scale_y_continuous(trans='log10')+theme_bw()+
        give_better_textsize_plot(13)

fun_koff(350)/fun_koff(300)

