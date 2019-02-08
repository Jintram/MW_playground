# code graveyard
ggplot(data=filter(my_test_data_sum,value>0), mapping=aes(x=value)) +
  geom_histogram(binwidth=1, breaks=seq(0,150,1))+
  #geom_freqpoly(binwidth=10, breaks=seq(0,150,10))
  coord_trans(y="log2")

# ==============================================================================

# Now we can convert into long format to do this for all 
my_test_data_melted=melt(my_test_data)
#my_test_data_sum=data.frame(colSums(my_test_data))
my_test_data_processed<-filter(my_test_data_melted,value>0,value<150)
freq<-hist(x=my_test_data_processed$value,
           breaks=seq(0.5,max(my_test_data_processed$value)+1,1),plot=FALSE)
freq_frame<-data.frame(centers = freq$mids, counts = freq$counts)
freq_frame<-mutate(freq_frame, countsp1=counts+1)
# Note that logarithmic scale doesn't work with bar plots as they start at 0
ggplot(data=freq_frame, mapping=aes(x=centers, y=countsp1)) +
  geom_bar(stat='identity')
# also check how many observations resulted in 0 counts
nr_zero_obs=sum(as.integer(my_test_data_melted$value))
# So instead we rather use a line
TEXTSIZE=15
ggplot(data=freq_frame, mapping=aes(x=centers, y=countsp1)) +
  geom_line()+
  geom_point()+
  coord_trans(y="log2")+
  xlab("Transcript count")+
  ylab("Number of times observed")+
  ggtitle(paste("Count statistics (nr zero count = ", 
                toString(nr_zero_obs),
                " or ",
                toString(round(nr_zero_obs/nrow(my_test_data_melted)*100,2)),
                "%)"  )) +
  theme(text = element_text(size=TEXTSIZE),
        axis.text = element_text(size=TEXTSIZE),
        plot.title = element_text(size=TEXTSIZE)) +
  scale_x_continuous(breaks = seq(0,150, by = 10)) +
  scale_y_continuous(breaks = 10^seq(0,5, by = 1),labels = comma)