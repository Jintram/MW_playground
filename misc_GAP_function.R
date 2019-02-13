


# Let's generate a random distribution of points

df_random_points = data_frame(x=runif(60),y=runif(60))

TEXTSIZE=15
ggplot(data=df_random_points,aes(x=x,y=y))+
  geom_point(aes(),size=3)+
  ggtitle('Randomn distribution of 60 points')+
  xlab('X')+ylab('Y')+
  theme(legend.position="none",
        text = element_text(size=TEXTSIZE),
        axis.text = element_text(size=TEXTSIZE),
        plot.title = element_text(size=TEXTSIZE))

X = <- runif(1, 5.0, 7.5)

# Now create some clusters
# create three clusters
clusters_x=c(runif(20)/10+1/4,runif(20)/10+3/5,runif(20)/10+1/5)
clusters_y=c(runif(20)/10+1/5,runif(20)/10+2/4,runif(20)/10+3/4)
df_clustered_points = data_frame(x=clusters_x,y=clusters_y)

# Identify clusters using algorithm
myclusters <- kmeans(df_clustered_points,centers=3)
cluster_assignments <- factor(myclusters$cluster)

# Plot 'm too
TEXTSIZE=15
ggplot(data=df_clustered_points,aes(x=x,y=y))+
  geom_point(aes(color=cluster_assignments),size=3)+
  ggtitle('Clearly clustered distribution of 60 points')+
  xlab('X')+ylab('Y')+
  xlim(c(0,1))+ylim(c(0,1))+
  theme(legend.position="none",
        text = element_text(size=TEXTSIZE),
        axis.text = element_text(size=TEXTSIZE),
        plot.title = element_text(size=TEXTSIZE))

# Now apply the GAP stat to both ----------------------------------------------------------

MYKMAX=30

# Let's do it for the fake clustered data
my_gap_stat <- clusGap(x=df_clustered_points, FUNcluster = kmeans, K.max = MYKMAX)#, B=5)
my_gap_stat_df <- data.frame(my_gap_stat$Tab, x=seq(1,MYKMAX))
# Let's also do it for the random data
# (Note that random data is already an integral component to this algorithm. It is 
# used for normalization, so we expect all values to be constant.)
my_gap_stat_rnd <- clusGap(x=df_random_points, FUNcluster = kmeans, K.max = MYKMAX)#, B=5)
my_gap_stat_df_rnd <- data.frame(my_gap_stat_rnd$Tab, x=seq(1,MYKMAX))

# According to Grun2015 first local maximum provides optimal clustering number
gaps <- my_gap_stat_df$gap
differences <- gaps[seq(2,length(gaps))]-gaps[seq(1,length(gaps)-1)]
local_going_down <- which(differences<0)
first_local_max = integer(MYKMAX)
first_local_max[local_going_down[1]]=1
my_gap_stat_df=mutate(my_gap_stat_df,first_local_max = as.factor(first_local_max))

# Now note that gap statistic uses bootstrapping and also provides standard deviation
# (Thanks to https://www.datanovia.com/en/lessons/determining-the-optimal-number-of-clusters-3-must-know-methods/#gap-statistic-method)
# And that we can use this to determine convergence (not sure how this works with standard error, because the SE 
# value doesn't seem to change when I change the number of samples B).
# Let's try anyways..
gaps <- my_gap_stat_df$gap
SE   <- my_gap_stat_df$SE.sim
level_to_reach <- gaps[2:length(gaps)]-2*SE[2:length(SE)]
treshold_exceeded <- which(gaps[1:(length(gaps)-1)]>level_to_reach)
point_idx <- treshold_exceeded[1]
# Also for random thingy
gaps_rnd <- my_gap_stat_df_rnd$gap
SE_rnd   <- my_gap_stat_df_rnd$SE.sim
level_to_reach_rnd <- gaps_rnd[2:length(gaps_rnd)]-2*SE_rnd[2:length(SE_rnd)]
treshold_exceeded_rnd <- which(gaps_rnd[1:(length(gaps_rnd)-1)]>level_to_reach_rnd)
point_idx_rnd <- treshold_exceeded_rnd[1]
# add to dataframe to plot
df_clust_sz <- data_frame(x=c(point_idx,point_idx_rnd),y=c(gaps[point_idx],gaps_rnd[point_idx_rnd]))


# Now show statistic
TEXTSIZE=15
ggplot(data=my_gap_stat_df,aes(x=x,y=gap))+
  geom_point(aes(color=first_local_max),size=3)+
  geom_line()+
  geom_errorbar(aes(ymin=gap-SE.sim, ymax=gap+SE.sim), width=.2)+
  geom_point(data=my_gap_stat_df_rnd, aes(x=x, y=gap),size=3,colour='seagreen')+
  geom_line( data=my_gap_stat_df_rnd, aes(x=x, y=gap), colour='seagreen')+
  geom_errorbar(data=my_gap_stat_df_rnd, aes(ymin=gap-SE.sim, ymax=gap+SE.sim), width=.2, colour='seagreen')+
  geom_point(data=df_clust_sz, aes(x=x,y=y), size=10, shape=1)+
  scale_colour_manual(name="Key", 
                      values=c("black", "red"),
                      breaks=c("0","1"))+
  ggtitle('Gap statistic on clustering of fake data
          comparing last monotonic rising point vs. first within 2 stdev')+
  xlab('Number of clusters')+ylab('Gap score')+
  theme(legend.position="none",
        text = element_text(size=TEXTSIZE),
        axis.text = element_text(size=TEXTSIZE),
        plot.title = element_text(size=TEXTSIZE))





