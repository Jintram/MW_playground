
# Parameters that come from earlier analysis
condition_factors # this holds attribution to datasets
datasetnames # matches above
shortdatasetnames # matches above

# these were determined earlier, but can be taken immediately from the data
cluster_assignments <- as.factor(groupedSCS$Combined@cluster$kpart)
all_gene_expression <- groupedSCS$Combined@ndata

# For convenience, some extra parameters
tsne_locations      <- groupedSCS$Combined@tsne

# re-build earlier dataframe
dataframe_cells <- data_frame(V1=tsne_locations[,1],V2=tsne_locations[,2],
                              cluster=cluster_assignments,condition=condition_factors)

# Make some plots to re-assure ourselves ====================================================================

# Re-make earlier clustering plot
ggplot(data=dataframe_cells)+
  geom_point(aes(x=V1,y=V2,color=cluster,shape=condition))+
  ggtitle('Gene expression space')+
  give_better_textsize_plot(20)

# Re-assure ourselves we're looking at the right stuff
ggplot(data=dataframe_cells)+
  geom_point(aes(x=V1,y=V2,color=cluster,shape=condition))+
  geom_point(data=dataframe_cells[dataframe_cells$cluster==5,],aes(x=V1,y=V2))+
  ggtitle('Gene expression space')+
  give_better_textsize_plot(20)

# Now look at cluster 5 specifically ====================================================================

# Get the indices of the cells we want to compare
indices_cluster5_mutant    <- which(dataframe_cells$cluster==5 & dataframe_cells$condition==2)
indices_cluster5_wildtype  <- which(dataframe_cells$cluster==5 & dataframe_cells$condition==1)

# Get the gene expression of those cells
gene_expression_cluster5_mutant    <- all_gene_expression[,indices_cluster5_mutant]
gene_expression_cluster5_wildtype  <- all_gene_expression[,indices_cluster5_wildtype]

# Calculate mean expression values
mean_gene_expression_cluster5_mutant    <-rowMeans(as.matrix(gene_expression_cluster5_mutant))
mean_gene_expression_cluster5_wildtype  <-rowMeans(as.matrix(gene_expression_cluster5_wildtype))

# Calculate standard deviations
sd_gene_expression_cluster5_mutant    <- rowSds(as.matrix(gene_expression_cluster5_mutant))
sd_gene_expression_cluster5_wildtype  <- rowSds(as.matrix(gene_expression_cluster5_wildtype))

# Now also calculate counts of positive in cells (similar to done before)
detectioncount_gene_expression_cluster5_mutant<-rowSums(1*(gene_expression_cluster5_mutant>0.1))
detectioncount_gene_expression_cluster5_wildtype<-rowSums(1*(gene_expression_cluster5_wildtype>0.1))

# Now count the differential expression
differential_gene_expression <- detectioncount_gene_expression_cluster5_mutant/detectioncount_gene_expression_cluster5_wildtype



# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# I was editing here, need to calculate std dev, min(counts), and add to bar plot

# 
barplot_differential_expression(selected_data_df=selected_data_df,centers_varname='n123ro',
                                differential_expression_varname='differential_expression',
                                all_gene_names=all_gene_names_race_short,
                                lowcol='red',highcol='firebrick4',ylabtext='Times higher in cluster',
                                mytitle=paste('Differential gene expression cluster ',toString(ii),'',sep=''))






# ========================================

pkp2_gene_expr_rev <- get_expression_gene(all_gene_expression[,which(dataframe_cells$condition==1)], '^PKP2_')
pkp2_gene_expr_mut <- get_expression_gene(all_gene_expression[,which(dataframe_cells$condition==2)], '^PKP2_')

mean_pkp2_rev <- mean(as.numeric(pkp2_gene_expr_rev))
mean_pkp2_mut <- mean(as.numeric(pkp2_gene_expr_mut))






