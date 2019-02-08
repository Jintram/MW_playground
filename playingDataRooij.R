

# Playing around with Van Rooij datasets (Anne/Bas)
# See also "./plots/data_exploration.R" for more plots

# Use install.packages("tidyverse") if library is not installed
library('tidyverse')
library('reshape2')
fileName = '/Users/m.wehrens/Data/HCM SCS/JE1_TranscriptCounts.tsv'
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

# Create a dataset which has at least 10 observations for each gene
my_test_data_selection = slice(my_test_data, which(as.integer(positives_per_gene>10) %in% 1)  )

# Let's look at an arbitrary correlation between two genes ------------------------------------

ID1<-1
ID2<-20
neitherzero=(as.numeric(slice(my_test_data_selection,ID1)>0))&(as.numeric(slice(my_test_data_selection,ID2)>0))
two_genes_scatter<-data_frame(x=as.numeric(slice(my_test_data_selection,ID1)),
                              y=as.numeric(slice(my_test_data_selection,ID2)),
                              neitherzero=neitherzero)
ggplot(data=two_genes_scatter, aes(x=x,y=y))+
  geom_point(aes(colour=neitherzero))+
  theme(legend.position="none",
        text = element_text(size=TEXTSIZE),
        axis.text = element_text(size=TEXTSIZE),
        plot.title = element_text(size=TEXTSIZE))+
  ggtitle(paste("Scatter between two genes, ", toString(nrow(two_genes_scatter)), " points that partially collapse"))+
  xlab(paste("gene 1    (",row.names(my_test_data)[ID1],")")) + ylab(paste("gene 2    (",row.names(my_test_data)[ID2],")"))


# Now get the correlation ----------------------------------------------------------------------




# get the correlation matrix
res <- cor(my_test_data_selection)
round(res, 2)

# melt it (creates dataframe with all X,Y pairs and their Y value
melt_res <- melt(res)

# plot this using ggplot, most simple way
ggplot(data = melt_res, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

# Slightly more elaborate plotting
ggplot(data = melt_res, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(aes(fill = rescale), colour="white")+ 
  scale_fill_gradient(low = "white",high = "steelblue")


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


p <- ggplot(res) + 
    geom_tile(aes(fill = rescale),colour = "white") + 
    scale_fill_gradient(low = "white",high = "steelblue"))





