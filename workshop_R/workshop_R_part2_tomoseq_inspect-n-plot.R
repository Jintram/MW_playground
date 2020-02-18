
################################################################################

library(ggplot2)

################################################################################

file_location = '/Users/m.wehrens/Data/_2019_12_R_workshop_data/Data_Eirini_Tomoseq/'
file_name     = 'HR-tomo2_AHTYKHBGX2_S2_R2.ReadCounts.tsv'
file_path     = paste0(file_location,file_name)

# Import data 
tomoseq_data = as.matrix(read.csv(file = file_path, row.names = 1, sep = '\t'))

# Another way to load the data
# library(readr)
#tomoseq_data = as.data.frame(read_tsv(file = file_path))
#rownames(tomoseq_data) = tomoseq_data[,1]
#tomoseq_data = as.matrix(tomoseq_data[,2:dim(tomoseq_data)[2]])

# to get some idea of the data:
head(tomoseq_data)
summary(tomoseq_data)
tomoseq_data[1:10,1:10]

################################################################################
# Exercise: calculate the totals per row (i.e. for genes) and the totals
# per column (i.e. per slice) and find some way to visualize this
# Hint: use a loop (I'll later show a more efficient way)
# Hint: use the dim() function to get the size of the dataframe
# Hint: use the "sum()" function
# Hint: remember to use ?function() to get help; e.g. "?dim()"

tomoseq_data_slice_totals = double()
for (idx in 1:dim(tomoseq_data)[2]) {
  tomoseq_data_slice_totals[idx] = sum(tomoseq_data[,idx])
}

tomoseq_data_slice_totals



# Exercise: Now also calculate the totals per gene
tomoseq_data_gene_totals = double()
for (idx in 1:dim(tomoseq_data)[1]) {
  tomoseq_data_gene_totals[idx] = sum(tomoseq_data[idx,])
}

tomoseq_data_gene_totals

# Now there is a shorter way to do stuff like this
tomoseq_data_gene_totals2 = apply(tomoseq_data,1,sum)
tomoseq_data_slice_totals2 = apply(tomoseq_data,2,sum)

# Exercise: calculate the standard deviations
tomoseq_data_gene_totals2 = apply(tomoseq_data,1,sd)
tomoseq_data_slice_totals2 = apply(tomoseq_data,2,sd)

# lapply() does the same as apply, but for each element in
# a list instead of for each row/column of a matrix;#
# e.g. lapply(list(1,2,3),sqrt) calculates 3 roots
# Exercise:
# (a) write a function to take the sqrt and add
# 1 to a value
# (b) use lapply to apply this function to a vector
my_function = function(x) {x^2+1}
my_list = list(1,2,3,4)
lapply_out = lapply(my_list,my_function)
lapply_out_vector = unlist(lapply_out)

################################################################################
# Exercises with plotting

# Now make a histogram of the totals you calculated earlier
# Hint: remember that the ggplot function wants a dataframe as
# input
ggplot(data.frame(log_totals=tomoseq_data_slice_totals))+
  geom_histogram(aes(x=log_totals), bins = 100)+
  theme_bw()+
  theme(text = element_text(size=25))

# Now make another histogram, but first transform the data logarithmically
# Do this for both gene and slice totals
# Hint: you need the log() or log10() function
# Hint: log functions can't process zeroes; so usually people just
# add a small value, like log(mydata+.1)
ggplot(data.frame(log_totals=log10(.1+tomoseq_data_slice_totals)))+
  geom_histogram(aes(x=log_totals), bins = 100)+
  theme_bw()+
  theme(text = element_text(size=25))+
  xlab('log(slice total reads)')

ggplot(data.frame(log_totals=log10(.1+tomoseq_data_gene_totals)))+
  geom_histogram(aes(x=log_totals), bins = 100)+
  theme_bw()+
  theme(text = element_text(size=25))+
  xlab('log(gene total reads)')

################################################################################
# Exercises on plotting gene expression over slices

# Exercise: which gene is most expressed in this data?
# Hint: use the internet for this; perhaps View() or sort() are
# convenient functions (or order() if you wanna make it difficult)
# Hint: use the output from the apply function, as
# the rownames are conserved there.
#(i.e. tomoseq_data_gene_totals2)

# Note: when using manually calculated totals, we first need also 
# assign the gene names
names(tomoseq_data_slice_totals) = colnames(tomoseq_data)
names(tomoseq_data_gene_totals) = rownames(tomoseq_data)

# Option 1: manually inspect the totals
View(tomoseq_data_gene_totals2)
# Option 2: sort the data and look at the top genes
top10_genes = sort(tomoseq_data_gene_totals2, decreasing = T)[1:10]
top10_genes
names(top10_genes)
# Option 3: use order() -- this is not so convenient but order
# is a function that might come in handy later
top10_genes_indices = order(tomoseq_data_gene_totals2, decreasing = T)[1:10]
top10_genes_indices
top10_genes = tomoseq_data_gene_totals2[top10_genes_indices]
top10_genes

# Exercise: Plot the expression of the top-expressed gene over slices
# Hint: as x-parameter you could use a series of numbers, i.e. "1:384"
# Hint: google for "grdevices::colors cheat sheet" to get
# an overview of colors you can use, see also:
# https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf
expression_MT_ATP6 = tomoseq_data[top10_genes_indices[1],]
typeof(tomoseq_data['MT-ATP6__chrM',])
ggplot(data.frame(expression_MT_ATP6=expression_MT_ATP6,
                  slice_nr = 1:384))+
  geom_line(aes(x=slice_nr,y=expression_MT_ATP6),color='blue4')+
  theme(text = element_text(size=25))+
  xlim(c(0,200))

# Exercise: Compare the expression of the two top-expressed genes
expression_MT_ATP6 = tomoseq_data[top10_genes_indices[1],]
expression_MT_ND3 = tomoseq_data[top10_genes_indices[2],]
# In case you're working with a dataframe
#expression_MT_ND3 = as.double(tomoseq_data[top10_genes_indices[2],])
expression_MT_ND3_normalize = 
    expression_MT_ND3/sum(expression_MT_ND3)
ggplot(data.frame(expression_MT_ATP6=expression_MT_ATP6,
                  expression_MT_ND3=expression_MT_ND3,
                  slice_nr = 1:384))+
  geom_line(aes(x=slice_nr,y=expression_MT_ATP6),color='blue4')+
  geom_line(aes(x=slice_nr,y=expression_MT_ND3),color='red4')+
  theme(text = element_text(size=25))+
  xlim(c(0,200))+ylab('gene expression')

# We can plot the total slice counts also per slice
ggplot(data.frame(tomoseq_data_slice_totals2=tomoseq_data_slice_totals2,
                  slice_nr = 1:384))+
  geom_line(aes(x=slice_nr,y=tomoseq_data_slice_totals2),color='black')+
  theme(text = element_text(size=20))+
  xlim(c(0,200))

################################################################################
# normalization of gene expression

# For another time, but good to know:
example_matrix = matrix(c(1,12,2,11),nrow = 2)
# normalize per rows:
row_totals = apply(example_matrix,1,sum)
rownorm_matrix = example_matrix / row_totals
# Normalize per columns:
col_totals = apply(example_matrix,2,sum)
colnorm_matrix = t(t(example_matrix) / col_totals)
  # note that t 'transposes' the matrix, meaning flipping rows and columns
  # this is necessary because of the 'rules' for matrix divisions by vectors
# We can check whether it worked:
apply(rownorm_matrix,1,sum)
apply(colnorm_matrix,2,sum)

# now apply to tomoseq data
col_totals = apply(tomoseq_data,2,sum)
tomoseq_data_norm = t(t(tomoseq_data) / col_totals)
# and plot again
expression_MT_ATP6 = tomoseq_data_norm[top10_genes_indices[1],]
expression_MT_ND3 = tomoseq_data_norm[top10_genes_indices[2],]
ggplot(data.frame(expression_MT_ATP6=expression_MT_ATP6,
                  expression_MT_ND3=expression_MT_ND3,
                  slice_nr = 1:384))+
  geom_line(aes(x=slice_nr,y=expression_MT_ATP6),color='blue4')+
  geom_line(aes(x=slice_nr,y=expression_MT_ND3),color='red4')+
  theme(text = element_text(size=25))+
  xlim(c(0,200))+ylim(c(0,.2))

################################################################################
# exercise on plotting per group

# Exercise
# Compare the expression of the two top-expressed genes in a box plot
# Hint: this is a bit more difficult; the dataframe needs to look like
# this:
# gene_label = 1 1 1 1 1 .... 2 2 2 2 2 ... <---- label for the gene
# expression = 3 5 6 3 5 .... 9 8 7 9 8 ... <---- expression BOTH genes
# slices     = 1 2 3 4 5 .... 1 2 3 4 5 ... <---- slice nrs BOTH genes
# Hint: when you make a dataframe, single values will be repeated
# if you assign them to a slot
# Hint: you can paste two dataframes together by using
# rbind(dataframe1, dataframe2) (rbind stands for row-bind)
# Hint: put the "gene_label" on the x-axis
# Hint: try to make a dataframe like example above for 1 gene first,
# and a second gene second, then paste them together
# create a data frame
df_1 = data.frame(expression_gene=expression_MT_ATP6,
                  slice_nr = 1:384,
                  gene_nr=1)
df_2 = data.frame(expression_gene=expression_MT_ND3,
                  slice_nr = 1:384,
                  gene_nr=2)
df_combined = rbind(df_1,df_2)
# box plot
ggplot(df_combined)+
  geom_boxplot(aes(x=gene_nr,y=expression_gene,fill=as.factor(gene_nr)))+
  theme(text = element_text(size=15),
    legend.position = 'none')
# violin plot
ggplot(df_combined)+
  geom_violin(aes(x=gene_nr,y=expression_gene,fill=as.factor(gene_nr)))+
  theme(text = element_text(size=15),
    legend.position = 'none')






