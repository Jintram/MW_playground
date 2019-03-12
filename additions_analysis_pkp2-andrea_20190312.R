


# Histogram for expression level of pkp2 =============================================

pkp2_expression<-get_expression_gene(groupedSCS$Combined@ndata, '^PKP2')
mean_pkp2_expression<-mean(as.numeric(pkp2_expression))
# determine 0.95 value
sorted_expression<-sort(as.numeric(pkp2_expression))
pkp2_expression_098<-sorted_expression[round(length(sorted_expression)*.98)]

# Also show histogram to validate choice
df_pkp2_expression<-data_frame(x=as.numeric(pkp2_expression))
ggplot()+
  geom_histogram(data=df_pkp2_expression,aes(x=x))+
  geom_vline(xintercept=mean_pkp2_expression)+
  geom_vline(xintercept=pkp2_expression_098)+
  xlab('Expression value')+ylab('Observed in # cells')+
  give_better_textsize_plot(15)

# Now also VIM1 --------------------------------------------------------------
# (We also plotted this one to illustrate it falls outside of the selected range,
# which makes those datapoints grey in the plots.)

gene_expression<-get_expression_gene(groupedSCS$Combined@ndata, '^VIM_')
mean_gene_expression<-mean(as.numeric(gene_expression))
# determine 0.95 value
sorted_expression<-sort(as.numeric(gene_expression))
gene_expression_098<-sorted_expression[round(length(sorted_expression)*.98)]

# Also show histogram to validate choice
df_gene_expression<-data_frame(x=as.numeric(gene_expression))
ggplot()+
  geom_histogram(data=df_gene_expression,aes(x=x))+
  geom_vline(xintercept=mean_gene_expression)+
  geom_vline(xintercept=gene_expression_098)+
  xlab('Expression value')+ylab('Observed in # cells')+
  give_better_textsize_plot(15)



# ============================================================================================
# Compare gene expression (for genes of interest) over cells =================================
# ============================================================================================

# Parameters that need to be present (result from previous sections)
list_of_genes_of_interest

# One could use the order of the clusters to sort the cells later
cluster_assignments <- as.factor(groupedSCS$Combined@cluster$kpart)
order_of_cells <- order(as.numeric(cluster_assignments)) # ascending, i.e. 1,2,3,..
# Or the expression of the PKP2 gene
pkp2_expression<-get_expression_gene(groupedSCS$Combined@ndata, '^PKP2')
order_of_cells <- order(pkp2_expression)

# Select genes of interest
selected_gene_matrix <- as.matrix(all_gene_expression[list_of_genes_of_interest,])
selected_gene_matrix <- selected_gene_matrix[,order_of_cells]
selected_gene_matrix_log10 <- log10(selected_gene_matrix)

# Create dataframe and plot
the_df<-melt(selected_gene_matrix_log10)
ggplot(the_df, aes(Var1,Var2, fill=value)) + 
  geom_raster()+
  ylab('Cells')+
  xlab('Genes')+
  ggtitle('Log10 gene expressoion')+
  theme(axis.text.y=element_blank(),
        axis.text.x=element_text(angle = 90, hjust = 1))

# Create save directory MW
#subMW_outputDir_path<-paste0(directory_with_data,outputDir,'plots_MW/')
#if(!dir.exists(subMW_outputDir_path)) {dir.create(subMW_outputDir_path, showWarnings = TRUE, recursive = TRUE, mode = "0777")}

ggsave(paste(directory_with_data, 'plots_MW/selected_genes_heatmap_expression_in_cells.pdf',sep=""), width=10, height=6)
ggsave(paste(directory_with_data, 'plots_MW/selected_genes_heatmap_expression_in_cells.png',sep=""), width=10, height=6)

# We might as well calculate the correlation matrix =========================

# get the correlation matrix
res <- cor(t(selected_gene_matrix),t(selected_gene_matrix))
round(res, 2)

# melt it (creates dataframe with all X,Y pairs and their Y value
melt_res <- melt(res)

# plot this using ggplot, most simple way
TEXTSIZE=8
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

ggsave(paste(directory_with_data, 'plots_MW/correlation_yeast2hybrid.pdf',sep=""), width=10, height=6)
ggsave(paste(directory_with_data, 'plots_MW/correlation_yeast2hybrid.png',sep=""), width=10, height=6)

# Plot two arbitrary genes

gene_names[list_of_genes_of_interest[1]]
gene_names[list_of_genes_of_interest[2]]

scatter_2genes<-data_frame(x=selected_gene_matrix[1,],y=selected_gene_matrix[2,])
ggplot(data=scatter_2genes)+
  geom_point(aes(x=x,y=y))+
  xlab(gene_names[list_of_genes_of_interest[1]])+
  ylab(gene_names[list_of_genes_of_interest[2]])