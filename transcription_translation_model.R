
library(zoo)
library(ggplot2)

maxtime = 10000
nr_events = 100
smooth_window_size=101

timeline = 1:maxtime
timeseries_mrna = rep(0,maxtime)
burst_times = round(runif(nr_events)*maxtime)
timeseries_mrna[burst_times] = round(runif(length(burst_times))*4+1)
timeseries_mrna = rollmean(timeseries_mrna,smooth_window_size)
newtimeline = timeline[(1+floor(smooth_window_size/2)):(length(timeline)-floor(smooth_window_size/2))]
    
ggplot(data.frame(time=newtimeline, mrna=100*timeseries_mrna)) + 
    geom_line(aes(x=time, y=mrna))+theme_bw()+ylab('mRNA')

prod_rate = 100
degr_rate = .001
timeseries_protein = c(0, rep(NA, length(timeseries_mrna)-1))
for (idx in 2:length(timeseries_mrna)) {
 
       timeseries_protein[idx] =  timeseries_protein[idx-1] + timeseries_mrna[idx]*prod_rate - timeseries_protein[idx-1]*degr_rate
    
}

ggplot(data.frame(time=newtimeline, mrna=100*timeseries_mrna)) + 
    geom_line(aes(x=time, y=timeseries_protein))+theme_bw()+ylab('protein')


