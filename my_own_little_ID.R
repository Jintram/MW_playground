

# Playing around with Van Rooij datasets (Anne/Bas/Joep) ================================
# See also "./plots/data_exploration.R" for more plots

# set working directory to directory of scripts
setwd('/Users/m.wehrens/Documents/git_repos/MW_playing/')

# Libraries
# Use install.packages("xxxx") if library is not installed
library('tidyverse')
library('reshape2')
library("RColorBrewer")
library("Rtsne")
library(grid)
library("gridExtra") # this throws an error which appears to be ignorable 
library(ggplot2)

# Personal libraries
source("./my_functions_clustering.R")

# Settings
fileName = 'JE1_TranscriptCounts.tsv'

# Load and plot some data -------------------------------------------------------------------

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

# We can first use dimensionality reduction methods both on the cells and on the correlation matrix;
# In both cases this is purely for visualizing the data

# Dimensionality reduction  ---------------------------------------------------------------------------------

# So now we'd need some dimensionality reduction methods also
# Convenient website: https://www.datacamp.com/community/tutorials/pca-analysis-r

# Convenient to convert correlation matrix to dataframe for later
res_df = as.data.frame(res)

# tSNE on (selection of) cells
my_tsne_cells = Rtsne(X=t(as.matrix(my_test_data_selection)))
# Put the values of each sample in terms of the projected vectors in dataframe
my_tsne_cells_df = as.data.frame(my_tsne_cells$Y)

# PCA on (selection of) cells
my_pca_cells = prcomp(x=t(my_test_data_selection))
# Put the values of each sample in terms of the principal components in dataframe
my_pca_cells_df = as.data.frame(my_pca_cells$x)

# tSNE on correlation matrix
my_tsne_corr = Rtsne(X=res)
# Put the values of each sample in terms of the projected vectors in dataframe
my_tsne_corr_df = as.data.frame(my_tsne_corr$Y)

# PCA on correlation matrix
my_pca_corr = prcomp(x=res_df)
# Put the values of each sample in terms of the principal components in dataframe
my_pca_corr_df = as.data.frame(my_pca_corr$x)

# 
cell_123_array <- seq(1,length(my_test_data_selection))

# Plot these points
p1_dimred<-ggplot(data=my_tsne_cells_df)+
  geom_point(aes(x=V1,y=V2))+#, color=cell_123_array)+
  ggtitle('t-SNE cells')
p2_dimred<-ggplot(data=my_pca_cells_df)+
  geom_point(aes(x=PC1,y=PC2))+#, color=cell_123_array)+
  ggtitle('PCA cells')
p3_dimred<-ggplot(data=my_tsne_corr_df)+
  geom_point(aes(x=V1,y=V2))+#, color=cell_123_array)+
  ggtitle('t-SNE corr matrix')
p4_dimred<-ggplot(data=my_pca_corr_df)+
  geom_point(aes(x=PC1,y=PC2))+#, color=cell_123_array)+
  ggtitle('PCA corr matrix')
grid.arrange(p1_dimred,p2_dimred,p3_dimred,p4_dimred,nrow=2)


# Then try to cluster the lines of the corr matrix according to K-means -----------------------------------
# This code uses a user-selected K value

# We need a bigger color palette to display the clusters ---------------------------------------------------

n <- 60
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
pie(rep(1,n), col=sample(col_vector, n))

# Now determine the clusters, first with arbitrary amount of clusters ---------------------------------------------------

# First 
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
  ggtitle('Correlation (not some clusters are similar)')+
  theme(#legend.position="none",
    text = element_text(size=TEXTSIZE),
    axis.text = element_text(size=TEXTSIZE),
    plot.title = element_text(size=TEXTSIZE),
    legend.text = element_text(size=TEXTSIZE),
    axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("./plots/data_exploration_correlationmap.pdf", width=30, height=30)
ggsave("./plots/data_exploration_correlationmap.png", width=30, height=30)

# Now it would be nice if we could determine the optimal cluster size ------------------------------------------

# I some clustering stuff in a separate file
MYKMAX=30
MY_B = 30

# It would be nice to perform the clustering directly on 
# the actual data, but this works extremely slow (i.e. it makes R crash)
# Likely that's why they use the correlation matrix; since
# it's less computationally expensive..

## calculate gap stats for clustering on cells ----------------------------------------------------------
#my_gap_stat <- clusGap(x=my_test_data_selection, FUNcluster = kmeans, K.max = MYKMAX, B =MY_B)
#my_gap_stat_df <- data.frame(my_gap_stat$Tab, x=seq(1,MYKMAX))
## Use custom functions to find optimal size and plot gap stat
#optimal_size <- get_optimal_cluster_size(my_gap_stat_df)
#p1_gap<-plot_gap_stat(my_gap_stat_df,optimal_size)

# Note that this uses
# source("./my_functions_clustering.R")

# calculate gap stats for clustering on pca representation of cells ----------------------------------------------------------
my_gap_stat_pca <- clusGap(x=my_pca_cells_df, FUNcluster = kmeans, K.max = MYKMAX, B = MY_B)
optimal_size_pca <- c(maxSE(my_gap_stat_pca$Tab[,"gap"], my_gap_stat_pca$Tab[,"SE.sim"], method = "firstmax"),
                      maxSE(my_gap_stat_pca$Tab[,"gap"], my_gap_stat_pca$Tab[,"SE.sim"], method = "Tibs2001SEmax"))
names(optimal_size_pca)<-c("firstmax","Tibs2001SEmax")
# optimal_size_pca <- get_optimal_cluster_size(my_gap_stat_pca_df) # custom function is redundant..
my_gap_stat_pca_df <- data.frame(my_gap_stat_pca$Tab, x=seq(1,MYKMAX))
# Use custom functions to plot gap stat
p1_gap_pca<-plot_gap_stat(my_gap_stat_pca_df,optimal_size_pca)

# calculate gap stats for clustering on correlation matrix ----------------------------------------------------------
my_gap_stat_corr <- clusGap(x=res_df, FUNcluster = kmeans, K.max = MYKMAX, B = MY_B)
optimal_size_corr <- c(maxSE(my_gap_stat_corr$Tab[,"gap"], my_gap_stat_corr$Tab[,"SE.sim"], method = "firstmax"),
                      maxSE(my_gap_stat_corr$Tab[,"gap"], my_gap_stat_corr$Tab[,"SE.sim"], method = "Tibs2001SEmax"))
names(optimal_size_corr)<-c("firstmax","Tibs2001SEmax")
my_gap_stat_corr_df <- data.frame(my_gap_stat_corr$Tab, x=seq(1,MYKMAX))
# Use custom functions to plot gap stat
p2_gap_corr<-plot_gap_stat(my_gap_stat_corr_df,optimal_size_corr)

grid.arrange(p1_gap_pca,p2_gap_corr,nrow=2)

# What we can do however, is use the number of clusters determined by gap-stats, and use kmeans -------------------

# perform the clustering
# on raw data
optimal_size_raw_chosen = optimal_size_corr[1]
myclusters_raw = kmeans(x=t(my_test_data_selection),centers=optimal_size_corr[1])
cluster_assignments_raw = factor(myclusters_raw$cluster)
# on pca data
myclusters_pca = kmeans(my_pca_cells_df,centers=optimal_size_pca[1])
cluster_assignments_pca = factor(myclusters_pca$cluster)
# on correlation data
myclusters_corr = kmeans(res_df,centers=optimal_size_corr[1])
cluster_assignments_corr = factor(myclusters_corr$cluster)

# Now plot some stuff -----------------------------------------------------------------------------------

TEXTSIZE=15
grid.arrange(
  #my_title_row("t-SNE projection"),
  plot_scatter_w_highlighted_clusters(
    my_tsne_cells_df,"V1","V2",cluster_assignments_raw,"tSNE-1","tSNE-2",paste("Raw k=",toString(optimal_size_raw_chosen),""),col_vector),
  plot_scatter_w_highlighted_clusters(
    my_tsne_cells_df,"V1","V2",cluster_assignments_pca,"tSNE-1","tSNE-2",paste("PCA k=",toString(optimal_size_pca[1]),""),col_vector),
  plot_scatter_w_highlighted_clusters(
    my_tsne_cells_df,"V1","V2",cluster_assignments_corr,"tSNE-1","tSNE-2",paste("Corr k=",toString(optimal_size_corr[1]),""),col_vector),
  #my_title_row("PCA projection"),
  plot_scatter_w_highlighted_clusters(
    my_pca_cells_df,"PC1","PC2",cluster_assignments_raw,"PCA-1","PCA-2",paste("Raw k=",toString(optimal_size_raw_chosen),""),col_vector),
  plot_scatter_w_highlighted_clusters(
    my_pca_cells_df,"PC1","PC2",cluster_assignments_pca,"PCA-1","PCA-2",paste("PCA k=",toString(optimal_size_pca[1]),""),col_vector),
  plot_scatter_w_highlighted_clusters(
    my_pca_cells_df,"PC1","PC2",cluster_assignments_corr,"PCA-1","PCA-2",paste("Corr k=",toString(optimal_size_corr[1]),""),col_vector),
  top=textGrob("Plotting cells in gene expression space",gp=gpar(fontsize=TEXTSIZE,font=3)), #font=3 sets italic
  nrow=2)
  

TEXTSIZE=15
grid.arrange(
  #my_title_row("t-SNE projection"),
  plot_scatter_w_highlighted_clusters(
    my_tsne_corr_df,"V1","V2",cluster_assignments_raw,"tSNE-1","tSNE-2",paste("Raw k=",toString(optimal_size_raw_chosen),""),col_vector),
  plot_scatter_w_highlighted_clusters(
    my_tsne_corr_df,"V1","V2",cluster_assignments_pca,"tSNE-1","tSNE-2",paste("PCA k=",toString(optimal_size_pca[1]),""),col_vector),
  plot_scatter_w_highlighted_clusters(
    my_tsne_corr_df,"V1","V2",cluster_assignments_corr,"tSNE-1","tSNE-2",paste("Corr k=",toString(optimal_size_corr[1]),""),col_vector),
  #my_title_row("PCA projection"),
  plot_scatter_w_highlighted_clusters(
    my_pca_corr_df,"PC1","PC2",cluster_assignments_raw,"PCA-1","PCA-2",paste("Raw k=",toString(optimal_size_raw_chosen),""),col_vector),
  plot_scatter_w_highlighted_clusters(
    my_pca_corr_df,"PC1","PC2",cluster_assignments_pca,"PCA-1","PCA-2",paste("PCA k=",toString(optimal_size_pca[1]),""),col_vector),
  plot_scatter_w_highlighted_clusters(
    my_pca_corr_df,"PC1","PC2",cluster_assignments_corr,"PCA-1","PCA-2",paste("Corr k=",toString(optimal_size_corr[1]),""),col_vector),
  top=textGrob("Plotting cells in cell-cell correlation space",gp=gpar(fontsize=TEXTSIZE,font=3)), #font=3 sets italic
  nrow=2)

















