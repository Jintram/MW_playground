

# Playing around with Van Rooij datasets (Anne/Bas)
# See also "./plots/data_exploration.R" for more plots

# set working directory to directory of scripts
setwd('/Users/m.wehrens/Documents/git_repos/MW_playing/')

# Use install.packages("tidyverse") if library is not installed
library('tidyverse')
library('reshape2')
fileName = 'JE1_TranscriptCounts.tsv'
my_test_data <- read.table(fileName, header=T, row.names=1, sep="\t")

#boxplot(split(world1$literacy,world1$cont),main=Literacy by Continent')

# Just plotting the values of 1 cell
my_data_ordered=filter(my_test_data,X1>0)
my_data_ordered=arrange(my_data_ordered,desc(X1))
ggplot(data=my_data_ordered) +
  geom_point(mapping=aes(x=seq(length(X1)),y=X1)) +
  coord_trans(y="log2")

# getting an idea of the values in the data
ggplot(data=my_test_data) +
  geom_point(mapping=aes(x=seq(length(X1)),y=X1))

# we can also make a pdf for one column
ggplot(data=filter(my_test_data,X1>0), mapping=aes(x=X1)) +
  #geom_histogram(binwidth=10, breaks=seq(0,150,10))
  geom_freqpoly(binwidth=10, breaks=seq(0,150,10))

# Let's also do some additional statistics ====================================================

# are there rows that are completely filled with zeroes??
# Let's just look at the first row
row1 <- my_test_data %>% slice(1)
# This row has precisely 1 value that is higher than 0
sum(as.integer((row1>0)))

# See other script for more extensive analysis

my_test_data_selection2 = my_test_data %>% filter_all(all_vars(. > 0))

# Let's try to re-do the correlation plot ====================================================

# Convenient to use this stat -----------------------------------------------------------------
# Let's look at the statistics of non-zero values ("positives")
# (I called this counts_hits previously.)

# Let's see whether this can be done faster
my_test_data_binary<-my_test_data>0
positives_per_cell <- colSums(my_test_data_binary)
positives_per_gene <- rowSums(my_test_data_binary)

# Create selection criterion for cells (should have 100 positives)
indices_to_select = which(as.integer(positives_per_cell>100) %in% 1)

# Now select the cells
my_test_data_selection=my_test_data[,indices_to_select]

# Let's test getting these correlations on some fake data --------------------------------------

# generate some fake data
my_fake_data <- data.frame(cell1=c(10,2,3,4),cell2=c(9,3,2,8),cell3=c(1,2,3,2)) # cells 1 and 2 fairly similar

# get the correlation matrix
res <- cor(my_fake_data,my_fake_data)
round(res, 2)

# melt it (creates dataframe with all X,Y pairs and their Y value
melt_res <- melt(res)

# plot this using ggplot, most simple way
TEXTSIZE=12
ggplot(data = melt_res, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradient2(limits=c(-1, 1),low='darkred',mid = "white", high = "steelblue",name=element_blank())+
  xlab(element_blank())+ylab(element_blank())+
  ggtitle('Correlation')+
  theme(#legend.position="none",
        text = element_text(size=TEXTSIZE),
        axis.text = element_text(size=TEXTSIZE),
        plot.title = element_text(size=TEXTSIZE),
        legend.text = element_text(size=TEXTSIZE),
        axis.text.x = element_text(angle = 90, hjust = 1))

# Now for real data ----------------------------------------------------------------------------

# get the correlation matrix
res <- cor(my_test_data_selection,my_test_data_selection)
round(res, 2)

# melt it (creates dataframe with all X,Y pairs and their Y value
melt_res <- melt(res)

# plot this using ggplot, most raw method
TEXTSIZE=6
ggplot(data = melt_res, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradient2(limits=c(-1, 1),low='darkred',mid = "white", high = "steelblue",name=element_blank())+
  xlab(element_blank())+ylab(element_blank())+
  ggtitle('Correlation')+
  theme(#legend.position="none",
    text = element_text(size=TEXTSIZE),
    axis.text = element_text(size=TEXTSIZE),
    plot.title = element_text(size=TEXTSIZE),
    legend.text = element_text(size=TEXTSIZE),
    axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("./plots/data_exploration_correlationmap.pdf", width=30, height=30)
ggsave("./plots/data_exploration_correlationmap.png", width=30, height=30)

# How can we order the cells now such that we better see their pattern?
# This can obviously be done with the clustering analysis, 
# we could also do a t-SNE project onto 1 dimension, because then similar 
# cells are at similar distances. But this is a method that is independent from
# the correlation plot. So we'd rather use something related to the correlation
# matrix.
# --> I think also R-built in algorithms use some clustering algorithm, so
# we'd now also need to use a clustering algorithm.

# basic code version
{"
# get the correlation matrix
res <- cor(my_test_data_selection,my_test_data_selection)
round(res, 2)

# melt it (creates dataframe with all X,Y pairs and their Y value
melt_res <- melt(res)

# plot this using ggplot, most simple way
ggplot(data = melt_res, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()
"}

# Some additional correlation matrix tools ---------------------------------------------------------
# (Built-in)

# Below code can be used for plotting also, but since our dataset is large, it's not most convenient..
"
library(corrplot)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
"

# This plots a nice heatmap using heatmap plot function
# Also orders data by hierarchy
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = res, col = col, symm = TRUE)
legend("left")

# Let's try some clustering -------------------------------------------------------------------------

# So if I understand correctly the clustering is performed on the correlation matrix, to find the similarity
# between the similarity profiles (inception :))

# First dimensionality-reduced plotting of corr matrix ----------------------------------------------
# So let's just try to plot this -- but this is also a 256 dimensional space
# So now we'd need some dimensionality reduction methods also
# Convenient website: https://www.datacamp.com/community/tutorials/pca-analysis-r

res_df = as.data.frame(res)

mypca = prcomp(x=res_df)
# Put the values of each sample in terms of the principal components in dataframe
mypca_df = as.data.frame(mypca$x)

# now just plot those points
ggplot(data=mypca_df)+
  geom_point(aes(x=PC1,y=PC2))

# We need a bigger color palette ---------------------------------------------------------------------------------

library("RColorBrewer")
n <- 60
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
pie(rep(1,n), col=sample(col_vector, n))

# Then try to cluster the lines of the corr matrix according to K-means -----------------------------------

myclusters = kmeans(res_df,centers=15)
cluster_assignments = factor(myclusters$cluster)

library("RColorBrewer")
# now plot PCA again, but color coded for clusters
ggplot(data=mypca_df)+
  geom_point(aes(x=PC1,y=PC2, color=cluster_assignments))+
  scale_color_manual(values=col_vector)+
  ggtitle('Correlation matrix converted to points (PCA projection)')
  #scale_color_brewer(palette="Dark2")

# Now this hierarchy can be applied to the corr matrix to sort it
mysorted_data=sort.int(cluster_assignments,index.return=TRUE)
mysorted_idx = mysorted_data$ix

sorted_res = res[mysorted_idx,]
sorted_res = sorted_res[,mysorted_idx]

# now plot this
# needs melt first (creates dataframe with all X,Y pairs and their Y value
melt_res <- melt(sorted_res)
# plot it
TEXTSIZE=6
ggplot(data = melt_res, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradient2(limits=c(-1, 1),low='darkred',mid = "white", high = "steelblue",name=element_blank())+
  xlab(element_blank())+ylab(element_blank())+
  ggtitle('Correlation')+
  theme(#legend.position="none",
    text = element_text(size=TEXTSIZE),
    axis.text = element_text(size=TEXTSIZE),
    plot.title = element_text(size=TEXTSIZE),
    legend.text = element_text(size=TEXTSIZE),
    axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("./plots/data_exploration_correlationmap.pdf", width=30, height=30)
ggsave("./plots/data_exploration_correlationmap.png", width=30, height=30)

# Now, R also has t-SNE algorithm that we can use from library ---------------------------------------------
# Let's use that for projection
library("Rtsne")

my_tsne = Rtsne(X=res_df)
# Put the values of each sample in terms of the principal components in dataframe
my_tsne_df = as.data.frame(my_tsne$Y)

# now plot these points again, also showing the clusters
ggplot(data=my_tsne_df)+
  geom_point(aes(x=V1,y=V2, color=cluster_assignments))+
  scale_color_manual(values=col_vector)+
  ggtitle('Correlation matrix converted to points (t-SNE projection)')

# Note that this is of course using the correlation functions as input, which might explains the 
# pattern in the data

# Now it would be nice if we could determine the optimal cluster size ------------------------------------------
# Grun2015 uses the gap statistic for this
# See also https://www.rdocumentation.org/packages/cluster/versions/2.0.7-1/topics/clusGap
# And /Users/m.wehrens/Documents/Naslag/data_science/clustering/K-means Cluster Analysis · UC Business Analytics R Programming Guide.pdf

MYKMAX=30
my_gap_stat <- clusGap(x=res_df, FUNcluster = kmeans, K.max = MYKMAX, B =100)

my_gap_stat_df <- data.frame(my_gap_stat$Tab, x=seq(1,MYKMAX))

# According to Grun2015 first local maximum provides optimal clustering number
gaps <- my_gap_stat_df$gap
differences <- gaps[seq(2,length(gaps))]-gaps[seq(1,length(gaps)-1)]
local_going_down <- which(differences<0)
first_local_max = integer(MYKMAX)
first_local_max[local_going_down[1]]=1
my_gap_stat_df=mutate(my_gap_stat_df,first_local_max = as.factor(first_local_max))

# Now the standard deviation method would also be nice
# But seems a bit useless with these low stdvevs
# Let's try anyways.. 
gaps <- my_gap_stat_df$gap
SE   <- my_gap_stat_df$SE.sim
level_to_reach <- gaps[2:length(gaps)]-2*SE[2:length(SE)]
treshold_exceeded <- which(gaps[1:(length(gaps)-1)]>level_to_reach)
point_idx <- treshold_exceeded[1]
df_clust_sz <- data_frame(x=point_idx,y=gaps[point_idx])

# Now show statistic
TEXTSIZE=15
ggplot(data=my_gap_stat_df,aes(x=x,y=gap))+
  geom_point(aes(color=first_local_max),size=3)+
  geom_line()+
  geom_errorbar(aes(ymin=gap-SE.sim, ymax=gap+SE.sim), width=.2)+
  ggtitle('Gap statistic on clustering of correlation matrix')+
  xlab('Number of clusters')+ylab('Gap score')+
  geom_point(data=df_clust_sz, aes(x=x,y=y), size=10, shape=1)+
  scale_colour_manual(name="Key", 
                      values=c("black", "red"),
                      breaks=c("0","1"))+
  theme(legend.position="none",
        text = element_text(size=TEXTSIZE),
        axis.text = element_text(size=TEXTSIZE),
        plot.title = element_text(size=TEXTSIZE))

# Now also plot the cells themselves using tSNE and PCA ==================================================

library("Rtsne")

my_tsne = Rtsne(X=res_df)
# Put the values of each sample in terms of the principal components in dataframe
my_tsne_df = as.data.frame(my_tsne$Y)

# now plot these points again, also showing the clusters
ggplot(data=my_tsne_df)+
  geom_point(aes(x=V1,y=V2, color=cluster_assignments))+
  scale_color_manual(values=col_vector)+
  ggtitle('Correlation matrix converted to points (t-SNE projection)')






