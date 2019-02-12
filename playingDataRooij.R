

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

# Let's look at an arbitrary correlation between two cells ------------------------------------

ID1<-1
ID2<-2

cell1_values <- my_test_data_selection[,ID1]
cell2_values <- my_test_data_selection[,ID2]

overlap_of_postives = sum(as.numeric(cell1_values>0 & cell1_values>0))
nr_genes_sel_data = nrow(my_test_data_selection) # number of genes in selection

neitherzero_idxs=which((cell1_values>0 & cell1_values>0))
# (as.numeric(slice(my_test_data_selection,ID1)>0))&(as.numeric(slice(my_test_data_selection,ID2)>0))

cor_cells = cor(cell1_values,cell2_values)
cov_cells = cov(cell1_values,cell2_values)
var_cell1 = var(cell1_values)
var_cell2 = var(cell2_values)
lsf_b = cov_cells/var_cell1
lsf_a = mean(cell2_values)-lsf_b*mean(cell1_values)
x_fitted = c(1,max(cell1_values))
y_fitted = lsf_b*x_fitted+lsf_a
  
# Create dataframes for plotting
scatter_2_cells = data.frame(x=cell1_values,y=cell2_values)
fitline_df      = data.frame(x_fitted=x_fitted,y_fitted=y_fitted)

# Now also fit using standard function
my_lsf2 = lm(cell2_values ~ cell1_values)
lsf_a2 = my_lsf$coefficients[2]
lsf_b2 = my_lsf$coefficients[1]
# Now see what happens if we base fit on overlap only
my_lsf3 = lm(cell2_values[neitherzero_idxs] ~ cell1_values[neitherzero_idxs])
lsf_a3 = my_lsf$coefficients[2]
lsf_b3 = my_lsf$coefficients[1]
# Create params to plot again
x_fitted3 = c(1,max(cell1_values))
y_fitted3 = lsf_b3*x_fitted3+lsf_a3
fitline_df3      = data.frame(x_fitted=x_fitted3,y_fitted=y_fitted3)

neitherzero_idxs

# normal plot (including zero values) ---
TEXTSIZE=15
ggplot(data=scatter_2_cells, aes(x=x,y=y))+
  geom_point()+
  geom_line(data=fitline_df, aes(x=x_fitted,y=y_fitted))+
  geom_line(data=fitline_df3, aes(x=x_fitted,y=y_fitted),color='red')+
  theme(legend.position="none",
        text = element_text(size=TEXTSIZE),
        axis.text = element_text(size=TEXTSIZE),
        plot.title = element_text(size=TEXTSIZE))+
  ggtitle(paste("Gene expression cell ", toString(ID1)," vs ", toString(ID2), " (selection)
                 Points w/ 2 positive values = ", toString(overlap_of_postives), " of " , toString(nr_genes_sel_data)))+
  xlab(paste("Cell ",toString(ID1))) + ylab(paste("Cell ",toString(ID1)))

# log plot ; no zero values---
TEXTSIZE=15
ggplot(data=filter(scatter_2_cells,x>0,y>0), aes(x=x,y=y))+
  geom_point()+
  geom_line(data=fitline_df, aes(x=x_fitted,y=y_fitted))+
  geom_line(data=fitline_df3, aes(x=x_fitted,y=y_fitted),color='red')+
  coord_trans(x="log10",y="log10")+
  theme(legend.position="none",
        text = element_text(size=TEXTSIZE),
        axis.text = element_text(size=TEXTSIZE),
        plot.title = element_text(size=TEXTSIZE))+
  ggtitle(paste("Gene expression cell ", toString(ID1)," vs ", toString(ID2), " (selection; zero values genes ommitted)
                Points w/ 2 positive values = ", toString(overlap_of_postives), " of " , toString(nr_genes_sel_data)))+
  xlab(paste("Cell ",toString(ID1))) + ylab(paste("Cell ",toString(ID1)))

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





