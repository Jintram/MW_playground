


# ---------------------------------------

# A line x=y (just for show)
xyline_df<-data_frame(x=c(0,10),y=c(0,10))

# Generate some "data"
fakepoints_df <- data_frame(x=runif(1000,0,10),y=runif(1000,0,10))

# Plot the data
p1<-ggplot()+
  geom_point(data=fakepoints_df, aes(x=x, y=y))+
  geom_line(data=xyline_df, aes(x=x, y=y))

# Calculate distances
distances<-abs(fakepoints_df$x-fakepoints_df$y)/sqrt(2)

# Now update the data with the distances
fakepoints_df <- mutate(fakepoints_df, my_selection=as.factor(distances>1))

# I plot the distribution of distances here, just to illustrate
# For real data, you could use this to determine some cutoff
p2<-ggplot()+
  geom_histogram(data=fakepoints_df,aes(x=distances))

# Now show our selection
p3<-ggplot()+
  geom_point(data=fakepoints_df, aes(x=x, y=y, color=my_selection))+
  geom_line(data=xyline_df, aes(x=x, y=y))



