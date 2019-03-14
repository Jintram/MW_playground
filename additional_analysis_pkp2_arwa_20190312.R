
# Some setup ================================================================================================

# Parameters that come from earlier analysis
condition_factors # this holds attribution to datasets
datasetnames # matches above
shortdatasetnames # matches above

# these were determined earlier, but can be taken immediately from the data
cluster_assignments <- as.factor(groupedSCS$Combined@cluster$kpart)
all_gene_expression             <- groupedSCS$Combined@ndata
all_gene_expression_raw         <- groupedSCS$Combined@expdata[,names(all_gene_expression)]
all_gene_expression_downsampled <- groupedSCS$Combined@fdata
gene_names                      <- rownames(groupedSCS$Combined@ndata)

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

# Calculate mean and median expression values
mean_gene_expression_cluster5_mutant    <- rowMeans(as.matrix(gene_expression_cluster5_mutant))
mean_gene_expression_cluster5_wildtype  <- rowMeans(as.matrix(gene_expression_cluster5_wildtype))
median_gene_expression_cluster5_mutant    <- rowMedians(as.matrix(gene_expression_cluster5_mutant))
median_gene_expression_cluster5_wildtype  <- rowMedians(as.matrix(gene_expression_cluster5_wildtype))

# Calculate standard deviations
sd_gene_expression_cluster5_mutant    <- rowSds(as.matrix(gene_expression_cluster5_mutant))
sd_gene_expression_cluster5_wildtype  <- rowSds(as.matrix(gene_expression_cluster5_wildtype))

# Now also calculate counts of positive in cells (similar to done before)
detectioncount_gene_expression_cluster5_mutant<-rowSums(1*(gene_expression_cluster5_mutant>0.1))
detectioncount_gene_expression_cluster5_wildtype<-rowSums(1*(gene_expression_cluster5_wildtype>0.1))

# Calculate rescaling factors as done Grun & Van Oudenaarden (no pro'lly stands for normalization)
# IMPORTANT NOTE: this value critically depends on whether "min" or "median" is used for normalization;
# this is done in the racid code, where ndata is calculated
no_wildtype <- (median(apply(all_gene_expression_raw[,indices_cluster5_wildtype],2,sum))) / (min(apply(all_gene_expression_raw,2,sum)))
no_mutant   <- (median(apply(all_gene_expression_raw[,indices_cluster5_mutant],2,sum)))   / (min(apply(all_gene_expression_raw,2,sum)))

# Now rescale them accordingly
median_gene_expression_cluster5_mutant_rescaled   <-
  no_mutant*median_gene_expression_cluster5_mutant
median_gene_expression_cluster5_wildtype_rescaled <-
  no_wildtype*median_gene_expression_cluster5_wildtype

# Now count the differential expression
differential_gene_expression <- median_gene_expression_cluster5_mutant_rescaled / 
  median_gene_expression_cluster5_wildtype_rescaled
    
# Now calculate standard deviations
differential_gene_expression_stdev <-
  differential_gene_expression*
  sqrt((sd_gene_expression_cluster5_mutant*abs(no_mutant)/detectioncount_gene_expression_cluster5_mutant_rescaled)^2+
         (sd_gene_expression_cluster5_wildtype*abs(no_wildtype)/detectioncount_gene_expression_cluster5_wildtype_rescaled)^2)

# now calculate p-values
pv <- binompval(median_gene_expression_cluster5_wildtype_rescaled/sum(median_gene_expression_cluster5_wildtype_rescaled),
                sum(median_gene_expression_cluster5_mutant_rescaled),median_gene_expression_cluster5_mutant_rescaled)

# create dataframe similar grun & van oudenaarden
cl5_diff_expr_df <- data.frame(mean.cl5_wildtype=median_gene_expression_cluster5_wildtype_rescaled,mean.cl5_mutant=median_gene_expression_cluster5_mutant_rescaled,
                fc=differential_gene_expression,
                fc_inv=1/differential_gene_expression,
                pv=pv,
                stdev=differential_gene_expression_stdev,
                cellcount.cl5_mutant=detectioncount_gene_expression_cluster5_mutant,
                cellcount.cl5_wildtype=detectioncount_gene_expression_cluster5_wildtype,
                row.names = gene_names,
                gene_name = gene_names)
                #n123original = factor(seq(1,length(differential_gene_expression)))
cl5_diff_expr_df_filterp05 <- cl5_diff_expr_df[cl5_diff_expr_df$pv<0.01,]
cl5_diff_expr_df_filterp05 <- cl5_diff_expr_df_filterp05[order(cl5_diff_expr_df_filterp05$fc,decreasing=T),]
cl5_diff_expr_df_filterp05 <- mutate(cl5_diff_expr_df_filterp05,n123=factor(1:nrow(cl5_diff_expr_df_filterp05)))

# ==================================================================================================

# Select and plot top increased genes
df_top_selection<-cl5_diff_expr_df_filterp05[1:10,]
df_top_selection<-mutate(df_top_selection,n321=as.factor(seq(nrow(df_top_selection),1,-1)))
barplot_differential_expression_v2(df_top_selection,
                                   differential_expression_varname='fc',
                                   center_varname='n321',
                                   gene_name_varname='gene_name',
                                   lowcol='red',highcol='firebrick4',
                                   ylabtext='Times higher in mutant',
                                   mytitle=paste('Differential gene expression cluster 5 mutant vs. wildtype',sep=''))

# ==================================================================================================

# Select and plot top decreased genes
df_top_decr_selection<-cl5_diff_expr_df_filterp05[nrow(cl5_diff_expr_df_filterp05):(nrow(cl5_diff_expr_df_filterp05)-9),]
df_top_decr_selection<-mutate(df_top_decr_selection,
                              n123=as.factor(seq(1,nrow(df_top_decr_selection))),
                              n321=as.factor(seq(nrow(df_top_decr_selection),1,-1)))
barplot_differential_expression_v2(df_top_decr_selection,
                                   differential_expression_varname='fc_inv',
                                   center_varname='n321',
                                   gene_name_varname='gene_name',
                                   lowcol='skyblue',highcol='royalblue4',
                                   ylabtext='Times lower in mutant',
                                   mytitle=paste('Differential gene expression cluster 5 mutant vs. wildtype',sep=''))


# Check by plotting one vs. other (or mean? see below)

# Let's do a sanity check --------------------------------------------------------------------------------

vector_rev_mean<-vector()
vector_mut_mean<-vector()
for (gene_name in df_top_selection$gene_name) {
  
  expression <- get_expression_gene(all_gene_expression, gene_name)
  
  # reverted
  rev_mean<-mean(as.numeric(expression[which(cluster_assignments==5 & condition_factors==1)]))
  # mutant
  mut_mean<-mean(as.numeric(expression[which(cluster_assignments==5 & condition_factors==2)]))
  
  vector_rev_mean[length(vector_rev_mean)+1]<-rev_mean
  vector_mut_mean[length(vector_mut_mean)+1]<-mut_mean
}

# these two should be consistent (but not equal due to rescaling)
df_top_selection$mean.cl5_wildtype
vector_rev_mean
# these two should be consistent (but not equal due to rescaling)
df_top_selection$mean.cl5_mutant
vector_mut_mean
df_xy_line<-data.frame(x=c(0,max(c(df_scat$ratio1,df_scat$ratio2))),y=c(0,max(c(df_scat$ratio1,df_scat$ratio2))))
df_scat<-data.frame(x1=df_top_selection$mean.cl5_wildtype,x2=vector_rev_mean,
                    y1=df_top_selection$mean.cl5_mutant,  y2=vector_mut_mean,
                    ratio1=df_top_selection$mean.cl5_mutant/df_top_selection$mean.cl5_wildtype,
                    ratio2=vector_mut_mean/vector_rev_mean)
ggplot(data=df_scat)+geom_point(aes(x=ratio1,y=ratio2))+
  xlab('ratio (grun method)')+ylab('ratio (means of normalized data)')+
  coord_fixed(ratio = 1)+xlim(c(0,max(c(df_scat$ratio1,df_scat$ratio2))))+ylim(c(0,max(c(df_scat$ratio1,df_scat$ratio2))))+
  geom_line(data=df_xy_line,aes(x=x,y=y))+
  give_better_textsize_plot(20)


# ========================================

pkp2_gene_expr_rev <- get_expression_gene(all_gene_expression[,which(dataframe_cells$condition==1)], '^PKP2_')
pkp2_gene_expr_mut <- get_expression_gene(all_gene_expression[,which(dataframe_cells$condition==2)], '^PKP2_')

mean_pkp2_rev <- mean(as.numeric(pkp2_gene_expr_rev))
mean_pkp2_mut <- mean(as.numeric(pkp2_gene_expr_mut))






