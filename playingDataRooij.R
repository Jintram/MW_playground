

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

# Create a dataset which has at least 100 observations for each cell
my_test_data_selection = slice(my_test_data, which(as.integer(positives_per_cell>100) %in% 1)  )

# Let's look at an arbitrary correlation between two cells ------------------------------------

ID1<-1
ID2<-2

cell1_values <-my_test_data_selection[,ID1]
cell2_values <-my_test_data_selection[,ID2]

neitherzero=(as.numeric(slice(my_test_data_selection,ID1)>0))&(as.numeric(slice(my_test_data_selection,ID2)>0))

cor_cells = cor(cell1_values,cell2_values)
cov_cells = cov(cell1_values,cell2_values)
var_cell1 = var(cell1_values)
var_cell2 = var(cell2_values)
lsf_b = cov_cells/var_cell1
lsf_a = mean(cell2_values)-lsf_b*mean(cell1_values)
x_fitted = c(1,max(cell1_values))
y_fitted = lsf_b*x_fitted+lsf_a
  
scatter_2_cells = data.frame(x=cell1_values,y=cell2_values,x_fitted=x_fitted,y_fitted=y_fitted)

# normal plot (including zero values) ---
TEXTSIZE=15
ggplot(data=scatter_2_cells, aes(x=x,y=y))+
  geom_point()+
  geom_line(aes(x=x_fitted,y=y_fitted))+
  theme(legend.position="none",
        text = element_text(size=TEXTSIZE),
        axis.text = element_text(size=TEXTSIZE),
        plot.title = element_text(size=TEXTSIZE))+
  ggtitle(paste("Gene expression cell ", toString(ID1)," vs ", toString(ID2)))+
  xlab(paste("Cell ",toString(ID1))) + ylab(paste("gene ",toString(ID1)))

# log plot ---
TEXTSIZE=15
ggplot(data=filter(scatter_2_cells,x>0,y>0), aes(x=x,y=y))+
  geom_point()+
  geom_line(aes(x=x_fitted,y=y_fitted))+
  coord_trans(x="log10",y="log10")+
  theme(legend.position="none",
        text = element_text(size=TEXTSIZE),
        axis.text = element_text(size=TEXTSIZE),
        plot.title = element_text(size=TEXTSIZE))+
  ggtitle(paste("Gene expression cell ", toString(ID1)," vs ", toString(ID2), "(R = ",toString(round(cor_cells,2)),")"))+
  xlab(paste("Cell ",toString(ID1))) + ylab(paste("gene ",toString(ID1)))


# (Work in progress) ==================================================================
# I need to look at the code below 
# I first correlated the expression of genes over different cells;
# This does not tell much about cell identity, but might be interesting 
# to create functional groups of genes..

neitherzero=(as.numeric(slice(my_test_data_selection,ID1)>0))&(as.numeric(slice(my_test_data_selection,ID2)>0))
two_genes_scatter<-data_frame(x=as.numeric(slice(my_test_data_selection,ID1)),
                              y=as.numeric(slice(my_test_data_selection,ID2)),
                              neitherzero=neitherzero)

cor_genes = cor(two_genes_scatter$x,two_genes_scatter$y)
cov_genes = cov(two_genes_scatter$x,two_genes_scatter$y)
var_gene1 = var(two_genes_scatter$x)
var_gene2 = var(two_genes_scatter$y)
lsf_b = cov_genes/var_gene1
lsf_a = mean(two_genes_scatter$y)-lsf_b*mean(two_genes_scatter$x)

x_values_fitline = seq(0,max(two_genes_scatter$x))
y_values_fitline = lsf_b*x_values_fitline
fitline_df = data.frame(x=x_values_fitline,y=y_values_fitline)

ggplot(data=two_genes_scatter, aes(x=x,y=y))+
  geom_point(aes(colour=neitherzero))+
  geom_line(data=fitline_df)+
  theme(legend.position="none",
        text = element_text(size=TEXTSIZE),
        axis.text = element_text(size=TEXTSIZE),
        plot.title = element_text(size=TEXTSIZE))+
  ggtitle(paste("Scatter between two genes, ", toString(nrow(two_genes_scatter)), " points that partially collapse"))+
  xlab(paste("gene 1    (",row.names(my_test_data)[ID1],")")) + ylab(paste("gene 2    (",row.names(my_test_data)[ID2],")"))





# Now get the correlation ----------------------------------------------------------------------




# get the correlation matrix
res <- cor(my_test_data_selection,my_test_data_selection)
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





