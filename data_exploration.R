
# Load test data set and libraries =======================================================

# set working directory to directory of scripts
setwd('/Users/m.wehrens/Documents/git_repos/MW_playing/')

# Use install.packages("tidyverse") if library is not installed
library('tidyverse')
library('reshape2') # necessary for melt
library('scales') # necessary for axis labels
fileName = '/Users/m.wehrens/Data/HCM SCS/JE1_TranscriptCounts.tsv'
my_test_data <- read.table(fileName, header=T, row.names=1, sep="\t")

# Distribution of transcript counts for counts between 0-10 =============================
# This shows integer nature of counts

# Create histogram -------------------------------------------------------------------------
# Now we can convert into long format to do this for all 
my_test_data_melted=melt(my_test_data)
# Filter desired values
my_test_data_processed<-filter(my_test_data_melted,value>0,value<10)
# Create histogram manually
freq<-hist(x=my_test_data_processed$value,
           breaks=seq(0.5,max(my_test_data_processed$value)+1,.1),plot=FALSE)
# Pot in plotable data format
freq_frame<-data.frame(centers = freq$mids, counts = freq$counts)
# Add 1 to be able to use log scale
freq_frame<-mutate(freq_frame, countsp1=counts+1)

# Plot the data --------------------------------------------------------------------------
TEXTSIZE=15
ggplot(data=freq_frame, mapping=aes(x=centers, y=countsp1)) +
  geom_line()+
  geom_point()+
  coord_trans(y="log2")+
  xlab("Transcript count")+
  ylab("Number of times observed")+
  ggtitle("Distribution of transcript counts between 0-10") +
  theme(text = element_text(size=TEXTSIZE),
        axis.text = element_text(size=TEXTSIZE),
        plot.title = element_text(size=TEXTSIZE)) +
  scale_x_continuous(breaks = seq(0,10, by = 1)) +
  scale_y_continuous(breaks = 10^seq(0,5, by = 1),labels = comma)
ggsave("./plots/data_exploration_low_count.pdf", width=6, height=4)
ggsave("./plots/data_exploration_low_count.png", width=6, height=4)

# Distribution of transcript counts for counts between 0-150 =============================
# To make sure data is of good quality
# (E.g. not to many counts are zero or around very low values)

# Create histogram -------------------------------------------------------------------------

CUTOFF = 250

# Now we can convert into long format to do this for all 
my_test_data_melted=melt(my_test_data)
# Filter desired values
my_test_data_processed<-filter(my_test_data_melted,value>0,value<CUTOFF)
# Create histogram manually
freq<-hist(x=my_test_data_processed$value,
           breaks=seq(0.5,max(my_test_data_processed$value)+1,1),plot=FALSE)
# Pot in plotable data format
freq_frame<-data.frame(centers = freq$mids, counts = freq$counts)
# Add 1 to be able to use log scale
freq_frame<-mutate(freq_frame, countsp1=counts+1)

# Some additional stats
# How many values are zero?
nr_zero_obs = sum(as.integer(my_test_data_melted$value==0))
nr_above_cutoff = sum(as.integer(my_test_data_melted$value>CUTOFF))
total_observations = nrow(my_test_data_melted)

# Plot the data --------------------------------------------------------------------------
TEXTSIZE=15
ggplot(data=freq_frame, mapping=aes(x=centers, y=countsp1)) +
  geom_line()+
  geom_point()+
  coord_trans(y="log2")+
  xlab("Transcript count")+
  ylab("Number of times observed")+
  ggtitle(paste("Count statistics\n",
                "Zero count = ", toString(round(nr_zero_obs/total_observations*100,2)), "%;",
                "Above cutoff = ", toString(round(nr_above_cutoff/total_observations*100,2)), "%;")) +
  theme(text = element_text(size=TEXTSIZE),
        axis.text = element_text(size=TEXTSIZE),
        plot.title = element_text(size=TEXTSIZE)) +
  scale_x_continuous(breaks = seq(0,CUTOFF, by = 25)) +
  scale_y_continuous(breaks = 10^seq(0,5, by = 1),labels = comma)
ggsave("./plots/data_exploration_counts_pdf.pdf", width=6, height=4)
ggsave("./plots/data_exploration_counts_pdf.png", width=6, height=4)


# Look per gene, how many positive hits we see in all the cells =======================================

# Let's look at the statistics of non-zero values ("positives")
# (I called this counts_hits previously.)

# Let's see whether this can be done faster
my_test_data_binary<-my_test_data>0
positives_per_cell <- colSums(my_test_data_binary)
positives_per_gene <- rowSums(my_test_data_binary)

# Short way to plot distribution
# hist(positives_per_gene[positives_per_gene>0],breaks=seq(0,max(positives_per_gene),1))

# Some statistics
nr_genes_w_no_signal = sum(as.integer(positives_per_gene==0))
nr_cells_w_no_signal = sum(as.integer(positives_per_cell==0))
nr_genes = nrow(my_test_data)
nr_cells = ncol(my_test_data)

# Create histogram and dataframe (for plotting) manually for per-gene statistics
freq_pergene<-hist(positives_per_gene[positives_per_gene>0],breaks=seq(0.5,max(positives_per_gene)+1,1),plot=FALSE)
freq_pergene_frame<-data.frame(centers = freq_pergene$mids, counts = freq_pergene$counts,countsp1=freq_pergene$counts+1)
freq_percell<-hist(positives_per_cell[positives_per_cell>0],breaks=seq(0.5,max(positives_per_cell)+100,100),plot=FALSE)
freq_percell_frame<-data.frame(centers = freq_percell$mids, counts = freq_percell$counts,countsp1=freq_percell$counts+1)

# Create some convenient lines
line_at_one<-data_frame(x=seq(0,max(freq_frame$centers)),y=seq(1,1))
line_at_cellnr<-data_frame(x=seq(nr_cells,nr_cells),y=seq(1,max(freq_pergene_frame$counts)))
line_at_genenr<-data_frame(x=seq(nr_genes,nr_genes),y=seq(1,max(freq_percell_frame$counts)))

# Plot for per genes -----------------------------------------------------------------------------
ggplot(data=filter(freq_pergene_frame,counts>0), aes(x=centers, y=counts))+
  #geom_bar(stat="identity") +
  #geom_line()+ #Note we shouldn't use a line since we filtered the data
  geom_point()+
  #geom_line(data=line_at_one,aes(x=x,y=y),color='red')+
  geom_line(data=line_at_cellnr,aes(x=x,y=y),color='red')+
  coord_trans(x="log10",y="log10")+
  xlab(paste("Number of positive cells (of ",toString(nr_cells)," total cells)"))+
  ylab("Number of genes")+
  ggtitle(paste("Distribution of positive count, genes w/ no positives = ",toString(nr_genes_w_no_signal))) +
  theme(text = element_text(size=TEXTSIZE),
        axis.text = element_text(size=TEXTSIZE),
        plot.title = element_text(size=TEXTSIZE))+
  scale_x_continuous(breaks = 10^seq(0,5, by = .5),labels = comma) +
  scale_y_continuous(breaks = 10^seq(0,3, by = 1),labels = comma) +
  annotate("text",x=10^(log10(nr_cells)/2),y=max(freq$counts),label="Note scale is logarithmic, so points with value 0 ommitted;
           Red line indicates total nr of cells.")
ggsave("./plots/data_exploration_positivecount_pergene.pdf", width=6, height=4)
ggsave("./plots/data_exploration_positivecount_pergene.png", width=6, height=4)
# Consistency check, are all genes in my statistics?
(sum(freq_pergene$counts)==nr_genes)

# Plot for per cells -----------------------------------------------------------------------------
#ggplot(data=filter(freq_percell_frame,counts>0), aes(x=centers, y=counts))+
ggplot(data=freq_percell_frame, aes(x=centers, y=counts))+
  #geom_bar(stat="identity") +
  geom_line()+ #Note we shouldn't use a line since we filtered the data
  geom_point()+
  #geom_line(data=line_at_one,aes(x=x,y=y),color='red')+
  #geom_line(data=line_at_genenr,aes(x=x,y=y),color='red')+
  #coord_trans(x="log10",y="log10")+
  xlab(paste("Number of positive genes (of ",toString(nr_genes)," total genes)"))+
  ylab("Number of cells")+
  ggtitle(paste("Distribution of positive count, cells w/ no positives = ",toString(nr_cells_w_no_signal))) +
  theme(text = element_text(size=TEXTSIZE),
        axis.text = element_text(size=TEXTSIZE),
        plot.title = element_text(size=TEXTSIZE))+
  #scale_x_continuous(breaks = 10^seq(0,5, by = .5),labels = comma) +
  #scale_y_continuous(breaks = 10^seq(0,2, by = 1),labels = comma) +
  #annotate("text",x=10^(log10(nr_cells)/2),y=max(freq$counts),label="Note scale is logarithmic, so points with value 0 ommitted;
  #         Red line indicates total nr of cells.")
ggsave("./plots/data_exploration_positivecount_percell.pdf", width=6, height=4)
ggsave("./plots/data_exploration_positivecount_percell.png", width=6, height=4)
# Consistency check, are all cells in my statistics?
(sum(freq_percell$counts)==nr_cells)

# Looking at correlations in non-systemic way ----------------------------------------------------------------

# Let's look at an arbitrary correlation between two cells ------------------------------------

ID1<-1
ID2<-2

cell1_values <- my_test_data_selection[,ID1]
cell2_values <- my_test_data_selection[,ID2]

overlap_of_postives = sum(as.numeric(cell1_values>0 & cell1_values>0))
nr_genes_sel_data = nrow(my_test_data_selection) # number of genes in selection

neitherzero_idxs=which((cell1_values>0 & cell1_values>0))
# (as.numeric(slice(my_test_data_selection,ID1)>0))&(as.numeric(slice(my_test_data_selection,ID2)>0))

cor_cells <- cor(cell1_values,cell2_values)
cov_cells <- cov(cell1_values,cell2_values)
var_cell1 <- var(cell1_values)
var_cell2 <- var(cell2_values)
lsf_b <- cov_cells/var_cell1
lsf_a <- mean(cell2_values)-lsf_b*mean(cell1_values)
x_fitted <- seq(1,max(cell1_values),max(cell1_values)/100)
y_fitted <- lsf_b*x_fitted+lsf_a # y = a + b*x

# Create dataframes for plotting
scatter_2_cells <- data.frame(x=cell1_values,y=cell2_values)
fitline_df      <- data.frame(x_fitted=x_fitted,y_fitted=y_fitted)

# Now also fit using standard function
my_lsf2 <- lm(cell2_values ~ cell1_values)
lsf_a2 <- my_lsf2$coefficients[1] # y = a + b*x
lsf_b2 <- my_lsf2$coefficients[2] 
# Create params to plot again
x_fitted2 <- seq(1,max(cell1_values),max(cell1_values)/100)
y_fitted2 <- lsf_b2*x_fitted2+lsf_a2
fitline_df2      <- data.frame(x_fitted=x_fitted2,y_fitted=y_fitted2)
# Now see what happens if we base fit on overlap only
my_lsf3 <- lm(cell2_values[neitherzero_idxs] ~ cell1_values[neitherzero_idxs])
lsf_a3 <- my_lsf3$coefficients[1] # y = a + b*x
lsf_b3 <- my_lsf3$coefficients[2]
# Create params to plot again
x_fitted3 <- seq(1,max(cell1_values),max(cell1_values)/100)
y_fitted3 <- lsf_b3*x_fitted3+lsf_a3
fitline_df3 <- data.frame(x_fitted=x_fitted3,y_fitted=y_fitted3)

neitherzero_idxs

# normal plot (including zero values) ---
TEXTSIZE=15
ggplot(data=scatter_2_cells, aes(x=x,y=y))+
  geom_point()+
  geom_line(data=fitline_df, aes(x=x_fitted,y=y_fitted))+
  #geom_line(data=fitline_df2, aes(x=x_fitted,y=y_fitted),linetype = "dashed", size = 2)+
  geom_line(data=fitline_df3, aes(x=x_fitted,y=y_fitted),color='red')+
  theme(legend.position="none",
        text = element_text(size=TEXTSIZE),
        axis.text = element_text(size=TEXTSIZE),
        plot.title = element_text(size=TEXTSIZE))+
  ggtitle(paste("Gene expression cell ", toString(ID1)," vs ", toString(ID2), " (selection)
                Points w/ 2 positive values = ", toString(overlap_of_postives), " of " , toString(nr_genes_sel_data)))+
  xlab(paste("Cell ",toString(ID1))) + ylab(paste("Cell ",toString(ID1)))
ggsave("./plots/data_exploration_scatter_cell_vs_cell.pdf", width=6, height=4)
ggsave("./plots/data_exploration_scatter_cell_vs_cell.png", width=6, height=4)

# log plot ; no zero values---
TEXTSIZE=15
ggplot(data=filter(scatter_2_cells,x>0,y>0), aes(x=x,y=y))+
  geom_point(aes(colour='Scatter'))+
  geom_line(data=fitline_df, aes(x=x_fitted,y=y_fitted,colour='Trendline incl 0'))+
  geom_line(data=fitline_df3, aes(x=x_fitted,y=y_fitted,colour='Trendline excl 0'))+
  coord_trans(x="log10",y="log10")+
  theme(legend.position="bottom",
        text = element_text(size=TEXTSIZE),
        axis.text = element_text(size=TEXTSIZE),
        plot.title = element_text(size=TEXTSIZE))+
  ggtitle(paste("Gene expression cell ", toString(ID1)," vs ", toString(ID2), " (selection; zero values genes ommitted)
                Points w/ 2 positive values = ", toString(overlap_of_postives), " of " , toString(nr_genes_sel_data)))+
  xlab(paste("Cell ",toString(ID1))) + ylab(paste("Cell ",toString(ID2)))+
  scale_colour_manual(name="Key", 
                      values=c("black", "red", "blue"),
                      breaks=c("Scatter", "Trendline incl 0", "Trendline excl 0"))
ggsave("./plots/data_exploration_scatter_cell_vs_cell_loglog.pdf", width=6, height=4)
ggsave("./plots/data_exploration_scatter_cell_vs_cell_loglog.png", width=6, height=4)


