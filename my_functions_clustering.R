library("cluster")


get_optimal_cluster_size<-function(my_gap_stat_df)
# About gap statistic
# Grun2015 uses the gap statistic for this
# See also https://www.rdocumentation.org/packages/cluster/versions/2.0.7-1/topics/clusGap
# And /Users/m.wehrens/Documents/Naslag/data_science/clustering/K-means Cluster Analysis · UC Business Analytics R Programming Guide.pdf
  #
  # Note that this function is a bit redundant because clusGap can automatically determine max cluster size
{

# Organize important gap values
gaps <- my_gap_stat_df$gap
SE   <- my_gap_stat_df$SE.sim
  
# This is same as "firstmax"
# According to Grun2015 first local maximum provides optimal clustering number
differences <- gaps[seq(2,length(gaps))]-gaps[seq(1,length(gaps)-1)]
local_going_down <- which(differences<0)
point_idx_localmax <- local_going_down[1] 
  
# This is same as "2x_Tibs2001SEmax"
# Now the standard deviation method would also be nice
lower_treshold <- gaps[2:length(gaps)]-2*SE[2:length(SE)]
#upper_trashold <- gaps[2:length(gaps)]+4*SE[2:length(SE)]
above_next_treshold <- which(gaps[1:(length(gaps)-1)]>lower_treshold)# & gaps[1:(length(gaps)-1)]<lower_treshold)
# Might nog work though
point_idx_std <- above_next_treshold[1]

optimal_size <- c(point_idx_localmax, point_idx_std)
names(optimal_size) <- c("firstmax","Tibs2001SEmax")
return(optimal_size)
}



plot_gap_stat<-function(my_gap_stat_df,optimal_size)
{
  
  df_optimal_size = data_frame(x=optimal_size, y=my_gap_stat_df$gap[optimal_size], method=as.factor(c(1,2)))
  
# Now show statistic
TEXTSIZE=15
p1<-ggplot(data=my_gap_stat_df,aes(x=x,y=gap))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=gap-SE.sim, ymax=gap+SE.sim), width=.2)+
  ggtitle('Gap statistic')+
  xlab('Number of clusters')+ylab('Gap score')+
  geom_point(data=df_optimal_size, aes(x=x,y=y,shape=method), size=10)+
  scale_colour_manual(name="Key", 
                      values=c("black", "red"),
                      breaks=c("0","1"))+
  scale_shape_manual(values=c(0,1),labels=names(optimal_size))+
  theme(legend.position="right",
        text = element_text(size=TEXTSIZE),
        axis.text = element_text(size=TEXTSIZE),
        plot.title = element_text(size=TEXTSIZE))

  return(p1)
}