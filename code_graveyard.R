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






########################################################################################################
########################################################################################################








# Now, R also has t-SNE and PCA algorithms that we can use from library ---------------------------------------------
# Let's use that for projection
library("Rtsne")

# Apply the clustering method
myclusters = kmeans(x=res_df,centers=point_idx[1])
cluster_assignments = factor(myclusters$cluster)

# tSNE
my_tsne_corr = Rtsne(X=as.matrix(res_df))
# Put the values of each sample in terms of the projected vectors in dataframe
my_tsne_corr_df = as.data.frame(my_tsne$Y)

# PCA
mypca_corr = prcomp(x=res_df)
# Put the values of each sample in terms of the principal components in dataframe
mypca_corr_df = as.data.frame(mypca$x)

# Plot again
p1<-ggplot(data=my_tsne_corr_df)+
  geom_point(aes(x=V1,y=V2), color=cluster_assignments)+
  scale_color_manual(values=col_vector)+
  ggtitle('t-SNE corr matrix \nK-means clustering of corr')
p2<-ggplot(data=mypca_corr_df)+
  geom_point(aes(x=PC1,y=PC2), color=cluster_assignments)+
  scale_color_manual(values=col_vector)+
  #coord_trans(y="log2", x="log2")
  ggtitle('PCA corr matrix \nK-means clustering of corr')
grid.arrange(p1,p2,nrow=1)

# now plot these points again, also showing the clusters
ggplot(data=my_tsne_df)+
  geom_point(aes(x=V1,y=V2, color=cluster_assignments))+
  scale_color_manual(values=col_vector)+
  ggtitle('Correlation matrix converted to points (t-SNE projection)')

# Note that this is of course using the correlation functions as input, which might explains the 
# pattern in the data



# Run clustering  ------------------------------------------ ------------------------------------------

# Now apply the clustering method
myclusters = kmeans(x=res_df,centers=point_idx[1])
cluster_assignments = factor(myclusters$cluster)

# Plot again
p1<-ggplot(data=my_tsne_df)+
  geom_point(aes(x=V1,y=V2), color=cluster_assignments)+
  scale_color_manual(values=col_vector)+
  ggtitle('t-SNE cells \nK-means clustering of corr')
p2<-ggplot(data=mypca_df)+
  geom_point(aes(x=PC1,y=PC2), color=cluster_assignments)+
  scale_color_manual(values=col_vector)+
  #coord_trans(y="log2", x="log2")
  ggtitle('PCA cells \nK-means clustering of corr')
grid.arrange(p1,p2,nrow=1)

# Now that we have determined the optimal cluster size run kmeans again -----------------------------------



# Now also plot the cells themselves using tSNE and PCA ==================================================

library("Rtsne")

# tSNE
my_tsne = Rtsne(X=t(as.matrix(my_test_data_selection)))
# Put the values of each sample in terms of the projected vectors in dataframe
my_tsne_df = as.data.frame(my_tsne$Y)

# PCA
mypca = prcomp(x=t(my_test_data_selection))
# Put the values of each sample in terms of the principal components in dataframe
mypca_df = as.data.frame(mypca$x)

# now just plot those points
+
  
  
  # now plot these points again, also showing the clusters
  library("gridExtra")  

p1<-ggplot(data=my_tsne_df)+
  geom_point(aes(x=V1,y=V2), color=cluster_assignments)+
  scale_color_manual(values=col_vector)+
  ggtitle('t-SNE cells \nK-means clustering of corr')
p2<-ggplot(data=mypca_df)+
  geom_point(aes(x=PC1,y=PC2), color=cluster_assignments)+
  scale_color_manual(values=col_vector)+
  #coord_trans(y="log2", x="log2")
  ggtitle('PCA cells \nK-means clustering of corr')
grid.arrange(p1,p2,nrow=1)






*******
  
  
  
  
  
  
  TEXTSIZE=15
ggplot(data=selected_data_df_low, mapping=aes(x=n123, y=differential_expression_inv))+#,fill=dataset_id)) +
  geom_bar(stat="identity", mapping=aes(fill=differential_expression_inv))+
  scale_x_discrete(breaks=n123)#+
  coord_flip()+
#,
                   labels=all_gene_names_short[selected_data_df_low$original_nr])+
  xlab("Genes")+
  ylab("Times lower in mutant")+
  scale_fill_gradient(low='skyblue', high="midnightblue")+
  ggtitle('Differential gene expression conditions')+
  theme(legend.position="none",
        text = element_text(size=TEXTSIZE),
        axis.text = element_text(size=TEXTSIZE),
        plot.title = element_text(size=TEXTSIZE))
#scale_x_continuous(labels = comma)+
#scale_y_continuous(labels = comma)  









