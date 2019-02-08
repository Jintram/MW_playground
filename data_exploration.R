
# Load test data set and libraries =======================================================

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
