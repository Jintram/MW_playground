
# Some setup ================================================================================================

# Parameters that come from earlier analysis
condition_factors # this holds attribution to datasets
datasetnames # matches above
shortdatasetnames # matches above

# these were determined earlier, but can be taken immediately from the data
cluster_assignments             <- as.factor(groupedSCS$Combined@cluster$kpart)
cluster_assignments_raceID2     <- as.factor(groupedSCS$Combined@cpart)
all_gene_expression             <- groupedSCS$Combined@ndata
all_gene_expression_raw         <- groupedSCS$Combined@expdata[,names(all_gene_expression)]
all_gene_expression_downsampled <- groupedSCS$Combined@fdata
gene_names                      <- rownames(groupedSCS$Combined@ndata)

# For convenience, some extra parameters
tsne_locations      <- groupedSCS$Combined@tsne

# re-build earlier dataframe
dataframe_cells <- data_frame(V1=tsne_locations[,1],V2=tsne_locations[,2],
                              cluster=cluster_assignments,condition=condition_factors,
                              cluster_raceID2=cluster_assignments_raceID2)

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

# define sets
indices_cluster5_mutant    <- which(dataframe_cells$cluster==5 & dataframe_cells$condition==2)
indices_cluster5_wildtype  <- which(dataframe_cells$cluster==5 & dataframe_cells$condition==1)

# calculate differential expression
fn_output<-get_differential_gene_expression(indices_cluster5_mutant,indices_cluster5_wildtype,
                                            all_gene_expression,all_gene_expression_raw,
                                            method='min',pcutoff=0.01)
diff_expr_df<-fn_output[[1]]
diff_expr_df_filterpv<-fn_output[[2]]

# write to excel files
write.xlsx(diff_expr_df, paste0(directory_with_data,'plots_MW/differential_expression_cluster5_wt_vs_mut.xlsx'))
write.xlsx(diff_expr_df_filterpv, paste0(directory_with_data,'plots_MW/differential_expression_cluster5_wt_vs_mut_selection.xlsx'))

# some additional stats

# ==================================================================================================

# Select and plot top increased genes
df_top_selection<-diff_expr_df_filterpv[1:10,]
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
df_top_decr_selection<-diff_expr_df_filterpv[nrow(diff_expr_df_filterpv):(nrow(diff_expr_df_filterpv)-9),]
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
for (gene_name in c(df_top_selection$gene_name,df_top_decr_selection$gene_name)) {
  
  expression <- get_expression_gene(all_gene_expression, gene_name)
  
  # reverted
  rev_mean<-mean(as.numeric(expression[which(cluster_assignments==5 & condition_factors==1)]))
  # mutant
  mut_mean<-mean(as.numeric(expression[which(cluster_assignments==5 & condition_factors==2)]))
  
  vector_rev_mean[length(vector_rev_mean)+1]<-rev_mean
  vector_mut_mean[length(vector_mut_mean)+1]<-mut_mean
}

# now calculate raw means for all data (and allow comparison with resacaled fc)
df_all_fc<-data.frame(fc_raw=mean_gene_expression_cluster5_mutant/mean_gene_expression_cluster5_wildtype,
                          fc_rescaled=cl5_diff_expr_df$fc)
df_sel_pv_fc<-df_all_fc[cl5_diff_expr_df$pv<0.01,]

rescale_plot_lims_by <- max(ratio_rescaling_factors,1/ratio_rescaling_factors)
print(paste0('ratio_rescaling_factors = ',toString(ratio_rescaling_factors)))

# these two should be consistent (but not equal due to rescaling)
df_top_selection$mean.cl5_wildtype
vector_rev_mean
# these two should be consistent (but not equal due to rescaling)
df_top_selection$mean.cl5_mutant
vector_mut_mean
df_xy_line<-data.frame(x=c(0,max(c(df_scat$ratio1,df_scat$ratio2))),y=c(0,max(c(df_scat$ratio1,df_scat$ratio2))))
joined_mut<-c(df_top_selection$mean.cl5_mutant,df_top_decr_selection$mean.cl5_mutant)
joined_wt <-c(df_top_selection$mean.cl5_wildtype,df_top_decr_selection$mean.cl5_wildtype)
df_scat<-data.frame(x1=joined_wt,
                    x2=vector_rev_mean,
                    y1=joined_mut,  
                    y2=vector_mut_mean,
                    ratio1=joined_mut/joined_wt,
                    ratio2=vector_mut_mean/vector_rev_mean)
ggplot(data=df_scat)+
  geom_line(data=df_xy_line,aes(x=x,y=y*ratio_rescaling_factors))+
  geom_line(data=df_xy_line,aes(x=-x,y=-y/ratio_rescaling_factors))+
  geom_point(data=df_all_fc,aes(x=fc_rescaled,y=fc_raw),color='grey',shape=4)+
  geom_point(data=df_all_fc,aes(x=-1/fc_rescaled,y=-1/fc_raw),color='grey',shape=4)+
  geom_point(data=df_sel_pv_fc,aes(x=fc_rescaled,y=fc_raw),color='grey')+
  geom_point(data=df_sel_pv_fc,aes(x=-1/fc_rescaled,y=-1/fc_raw),color='grey')+
  geom_point(aes(x=ratio1,y=ratio2),color='purple')+
  geom_point(aes(x=-1/ratio1,y=-1/ratio2),color='orange')+
  xlab('ratio (grun method)')+ylab('ratio (means of normalized data)')+
  coord_fixed(ratio = 1)+
  xlim(c(-rescale_plot_lims_by*max(c(df_scat$ratio1,df_scat$ratio2)),rescale_plot_lims_by*max(c(df_scat$ratio1,df_scat$ratio2))))+
  ylim(c(-rescale_plot_lims_by*max(c(df_scat$ratio1,df_scat$ratio2)),rescale_plot_lims_by*max(c(df_scat$ratio1,df_scat$ratio2))))+
  geom_line(data=df_xy_line,aes(x=x,y=y))+
  geom_line(data=df_xy_line,aes(x=-x,y=-y))+
  give_better_textsize_plot(20)



# ========================================

pkp2_gene_expr_rev <- get_expression_gene(all_gene_expression[,which(dataframe_cells$condition==1)], '^PKP2_')
pkp2_gene_expr_mut <- get_expression_gene(all_gene_expression[,which(dataframe_cells$condition==2)], '^PKP2_')

mean_pkp2_rev <- mean(as.numeric(pkp2_gene_expr_rev))
mean_pkp2_mut <- mean(as.numeric(pkp2_gene_expr_mut))

# now check whether my function works correctly by calculating cluster vs. other ===================

# define sets
indices_cluster5     <- which(cluster_assignments_raceID2==5)
indices_not_cluster5 <- which(cluster_assignments_raceID2!=5)

# calculate differential expression
fn_output<-get_differential_gene_expression(indices_cluster5,indices_not_cluster5,
                                            all_gene_expression,all_gene_expression_raw,
                                            method='min',pcutoff=0.01)
diff_expr_df_cl5_vs_ncl5 <-fn_output[[1]]
diff_expr_df_cl5_vs_ncl5_filterpv<-fn_output[[2]]

df_top_selection<-diff_expr_df_cl5_vs_ncl5_filterpv[1:20,]
df_top_selection<-mutate(df_top_selection,n321=as.factor(seq(nrow(df_top_selection),1,-1)))
barplot_differential_expression_v2(df_top_selection,
                                   differential_expression_varname='fc',
                                   center_varname='n321',
                                   gene_name_varname='gene_name',
                                   lowcol='red',highcol='firebrick4',
                                   ylabtext='Times higher in mutant',
                                   mytitle=paste('CALCULATION MW -- differential expr. cluster 5',sep=''))

# using method that i think is buggy ========================================================================

cluster_assignments_raceID2 <- as.factor(groupedSCS$Combined@cpart)

# define sets
indices_cluster5     <- which(cluster_assignments_raceID2==5)
indices_not_cluster5 <- which(cluster_assignments_raceID2!=5)

# calculate differential expression
fn_output<-get_differential_gene_expression(indices_cluster5,indices_not_cluster5,
                                            all_gene_expression,all_gene_expression_raw,
                                            method='bug',pcutoff=0.01)
diff_expr_df_cl5_vs_ncl5 <-fn_output[[1]]
diff_expr_df_cl5_vs_ncl5_filterpv<-fn_output[[2]]

df_top_selection<-diff_expr_df_cl5_vs_ncl5_filterpv[1:20,]
df_top_selection<-mutate(df_top_selection,n321=as.factor(seq(nrow(df_top_selection),1,-1)))
barplot_differential_expression_v2(df_top_selection,
                                   differential_expression_varname='fc',
                                   center_varname='n321',
                                   gene_name_varname='gene_name',
                                   lowcol='red',highcol='firebrick4',
                                   ylabtext='Times higher in mutant',
                                   mytitle=paste('CALCULATION MW (\'bug\')-- differential expr. cluster 5',sep=''))

# using no rescaling ===============

# define sets
indices_cluster5     <- which(cluster_assignments_raceID2==5)
indices_not_cluster5 <- which(cluster_assignments_raceID2!=5)

# calculate differential expression
fn_output<-get_differential_gene_expression(indices_cluster5,indices_not_cluster5,
                                            all_gene_expression,all_gene_expression_raw,
                                            method='none',pcutoff=0.01)
diff_expr_df_cl5_vs_ncl5 <-fn_output[[1]]
diff_expr_df_cl5_vs_ncl5_filterpv<-fn_output[[2]]

df_top_selection<-diff_expr_df_cl5_vs_ncl5_filterpv[1:20,]
df_top_selection<-mutate(df_top_selection,n321=as.factor(seq(nrow(df_top_selection),1,-1)))
barplot_differential_expression_v2(df_top_selection,
                                   differential_expression_varname='fc',
                                   center_varname='n321',
                                   gene_name_varname='gene_name',
                                   lowcol='red',highcol='firebrick4',
                                   ylabtext='Times higher in mutant',
                                   mytitle=paste('CALCULATION MW (\'none\')-- differential expr. cluster 5',sep=''))

# original method =========

diffExp <- clustdiffgenes(groupedSCS$Combined,pvalue=0.01)
View(diffExp$cl.5)











# =================================================================================================================
# =================================================================================================================
# Code below is rather repetitive and repeats analyses for specific situations
# =================================================================================================================
# =================================================================================================================



# Repeat for cluster 4 vs. cluster 5 ==========

# Re-assure ourselves we're looking at the right stuff
ggplot(data=dataframe_cells)+
  geom_point(aes(x=V1,y=V2,shape=condition),color='grey')+
  geom_point(data=dataframe_cells[dataframe_cells$cluster==5&dataframe_cells$condition==2,],aes(x=V1,y=V2,shape=condition),color='orange')+
  geom_point(data=dataframe_cells[dataframe_cells$cluster==4&dataframe_cells$condition==2,],aes(x=V1,y=V2,shape=condition),color='red')+
  ggtitle('Gene expression space')+
  give_better_textsize_plot(20)

# Perform differential gene expression analysis ====================================================================

# define sets
indices_cluster4_mutant <- which(dataframe_cells$cluster==4 & dataframe_cells$condition==2)
indices_cluster5_mutant <- which(dataframe_cells$cluster==5 & dataframe_cells$condition==2)

# calculate differential expression
fn_output<-get_differential_gene_expression(indices_cluster4_mutant,indices_cluster5_mutant,
                                            all_gene_expression,all_gene_expression_raw,
                                            method='min',pcutoff=0.01)
diff_expr_df<-fn_output[[1]]
diff_expr_df_filterpv<-fn_output[[2]]

# write to excel files
write.xlsx(diff_expr_df, paste0(directory_with_data,'plots_MW/differential_expression_cluster_mut4vs5.xlsx'))
write.xlsx(diff_expr_df_filterpv, paste0(directory_with_data,'plots_MW/differential_expression_cluster_mut4vs5_selection.xlsx'))

# Plot 1

# Select and plot top increased genes
df_top_selection<-diff_expr_df_filterpv[1:10,]
df_top_selection<-mutate(df_top_selection,n321=as.factor(seq(nrow(df_top_selection),1,-1)))
barplot_differential_expression_v2(df_top_selection,
                                   differential_expression_varname='fc',
                                   center_varname='n321',
                                   gene_name_varname='gene_name',
                                   lowcol='red',highcol='firebrick4',
                                   ylabtext='Times higher in cluster 4',
                                   mytitle=paste('Differential gene expression (cluster 4 vs 5)',sep=''))

# Plot 2

# Select and plot top decreased genes
df_top_decr_selection<-diff_expr_df_filterpv[nrow(diff_expr_df_filterpv):(nrow(diff_expr_df_filterpv)-9),]
df_top_decr_selection<-mutate(df_top_decr_selection,
                              n123=as.factor(seq(1,nrow(df_top_decr_selection))),
                              n321=as.factor(seq(nrow(df_top_decr_selection),1,-1)))
barplot_differential_expression_v2(df_top_decr_selection,
                                   differential_expression_varname='fc_inv',
                                   center_varname='n321',
                                   gene_name_varname='gene_name',
                                   lowcol='skyblue',highcol='royalblue4',
                                   ylabtext='Times lower in cluster 4',
                                   mytitle=paste('Differential gene expression (cluster 4 vs 5)',sep=''))









# Repeat for cluster 7 vs. cluster 4 ==========

# Re-assure ourselves we're looking at the right stuff
ggplot(data=dataframe_cells)+
  geom_point(aes(x=V1,y=V2,shape=condition),color='grey')+
  geom_point(data=dataframe_cells[dataframe_cells$cluster==4&dataframe_cells$condition==2,],aes(x=V1,y=V2,shape=condition),color='orange')+
  geom_point(data=dataframe_cells[dataframe_cells$cluster==7&dataframe_cells$condition==2,],aes(x=V1,y=V2,shape=condition),color='red')+
  ggtitle('Gene expression space')+
  give_better_textsize_plot(20)

# Perform differential gene expression analysis ====================================================================

# define sets
indices_set1 <- which(dataframe_cells$cluster==7 & dataframe_cells$condition==2)
indices_set2 <- which(dataframe_cells$cluster==4 & dataframe_cells$condition==2)

# calculate differential expression
fn_output<-get_differential_gene_expression(indices_set1,indices_set2,
                                            all_gene_expression,all_gene_expression_raw,
                                            method='min',pcutoff=0.01)
diff_expr_df<-fn_output[[1]]
diff_expr_df_filterpv<-fn_output[[2]]

# write to excel files
write.xlsx(diff_expr_df, paste0(directory_with_data,'plots_MW/differential_expression_cluster_mut7vs4.xlsx'))
write.xlsx(diff_expr_df_filterpv, paste0(directory_with_data,'plots_MW/differential_expression_cluster_mut7vs4_selection.xlsx'))

# Plot 1

# Select and plot top increased genes
df_top_selection<-diff_expr_df_filterpv[1:10,]
df_top_selection<-mutate(df_top_selection,n321=as.factor(seq(nrow(df_top_selection),1,-1)))
barplot_differential_expression_v2(df_top_selection,
                                   differential_expression_varname='fc',
                                   center_varname='n321',
                                   gene_name_varname='gene_name',
                                   lowcol='red',highcol='firebrick4',
                                   ylabtext='Times higher in cluster 7',
                                   mytitle=paste('Differential gene expression (cluster 7 vs 4)',sep=''))

# Plot 2

# Select and plot top decreased genes
df_top_decr_selection<-diff_expr_df_filterpv[nrow(diff_expr_df_filterpv):(nrow(diff_expr_df_filterpv)-9),]
df_top_decr_selection<-mutate(df_top_decr_selection,
                              n123=as.factor(seq(1,nrow(df_top_decr_selection))),
                              n321=as.factor(seq(nrow(df_top_decr_selection),1,-1)))
barplot_differential_expression_v2(df_top_decr_selection,
                                   differential_expression_varname='fc_inv',
                                   center_varname='n321',
                                   gene_name_varname='gene_name',
                                   lowcol='skyblue',highcol='royalblue4',
                                   ylabtext='Times lower in cluster 7',
                                   mytitle=paste('Differential gene expression (cluster 7 vs 4)',sep=''))





# Repeat for cluster 6 vs. cluster 7 ==========

# Re-assure ourselves we're looking at the right stuff
ggplot(data=dataframe_cells)+
  geom_point(aes(x=V1,y=V2,shape=condition),color='grey')+
  geom_point(data=dataframe_cells[dataframe_cells$cluster==7&dataframe_cells$condition==2,],aes(x=V1,y=V2,shape=condition),color='orange')+
  geom_point(data=dataframe_cells[dataframe_cells$cluster==6&dataframe_cells$condition==2,],aes(x=V1,y=V2,shape=condition),color='red')+
  ggtitle('Gene expression space')+
  give_better_textsize_plot(20)

# Perform differential gene expression analysis ====================================================================

# define sets
indices_set1 <- which(dataframe_cells$cluster==6 & dataframe_cells$condition==2)
indices_set2 <- which(dataframe_cells$cluster==7 & dataframe_cells$condition==2)

# calculate differential expression
fn_output<-get_differential_gene_expression(indices_set1,indices_set2,
                                            all_gene_expression,all_gene_expression_raw,
                                            method='min',pcutoff=0.01)
diff_expr_df<-fn_output[[1]]
diff_expr_df_filterpv<-fn_output[[2]]

# write to excel files
write.xlsx(diff_expr_df, paste0(directory_with_data,'plots_MW/differential_expression_cluster_mut6vs7.xlsx'))
write.xlsx(diff_expr_df_filterpv, paste0(directory_with_data,'plots_MW/differential_expression_cluster_mut6vs7_selection.xlsx'))

# Plot 1

# Select and plot top increased genes
df_top_selection<-diff_expr_df_filterpv[1:10,]
df_top_selection<-mutate(df_top_selection,n321=as.factor(seq(nrow(df_top_selection),1,-1)))
barplot_differential_expression_v2(df_top_selection,
                                   differential_expression_varname='fc',
                                   center_varname='n321',
                                   gene_name_varname='gene_name',
                                   lowcol='red',highcol='firebrick4',
                                   ylabtext='Times higher in cluster 6',
                                   mytitle=paste('Differential gene expression (cluster 6 vs 7)',sep=''))

# Plot 2

# Select and plot top decreased genes
df_top_decr_selection<-diff_expr_df_filterpv[nrow(diff_expr_df_filterpv):(nrow(diff_expr_df_filterpv)-9),]
df_top_decr_selection<-mutate(df_top_decr_selection,
                              n123=as.factor(seq(1,nrow(df_top_decr_selection))),
                              n321=as.factor(seq(nrow(df_top_decr_selection),1,-1)))
barplot_differential_expression_v2(df_top_decr_selection,
                                   differential_expression_varname='fc_inv',
                                   center_varname='n321',
                                   gene_name_varname='gene_name',
                                   lowcol='skyblue',highcol='royalblue4',
                                   ylabtext='Times lower in cluster 6',
                                   mytitle=paste('Differential gene expression (cluster 6 vs 7)',sep=''))










# Investigating pkp2 expression=================================================================================

# Get gene expression
gene_expression<-as.numeric(get_expression_gene(all_gene_expression, '^PKP2_'))
# Build dataframe
gene_counts_df=data.frame(counts=gene_expression,
                          dataset_id=factor(condition_factors),
                          cluster=cluster_assignments)

# Plot boxplot 
TEXTSIZE=15
ggplot(gene_counts_df, aes(factor(dataset_id), counts)) + 
  #geom_violin(aes(fill = dataset_id))+
  geom_boxplot(aes(fill = dataset_id))+
  scale_x_discrete(breaks=seq(1,length(datasetnames)),
                   labels=shortdatasetnames)+
  ggtitle(paste('Expression of PKP2')) +
  xlab(element_blank())+ylab('Transcript count')+
  theme(legend.position="none",
        text = element_text(size=TEXTSIZE),
        axis.text = element_text(size=TEXTSIZE),
        plot.title = element_text(size=TEXTSIZE),
        legend.text = element_text(size=TEXTSIZE),
        axis.text.x = element_text(angle = 90, hjust = 1, size=TEXTSIZE))

shortdatasetnames[1]
mean(gene_counts_df$counts[gene_counts_df$dataset_id==1])

# ========

# Plot boxplot 
TEXTSIZE=15
ggplot(gene_counts_df, aes(factor(cluster), counts)) + 
  #geom_violin(aes(fill = dataset_id))+
  geom_boxplot(aes(fill = dataset_id))+
  scale_x_discrete(breaks=seq(1,max(levels(cluster_assignments))),
                   labels=lapply(seq(1,max(levels(cluster_assignments))),toString) )+
  #scale_color_manual(values=col_vector[1:2],labels=shortdatasetnames)+
  #scale_fill_manual(values=col_vector[1:2],labels=shortdatasetnames)+\
  scale_fill_hue(labels=shortdatasetnames)+
  ggtitle(paste('Expression of PKP2')) +
  xlab('Cluster')+ylab('Transcript count')+
  theme(#legend.position="none",
        text = element_text(size=TEXTSIZE),
        axis.text = element_text(size=TEXTSIZE),
        plot.title = element_text(size=TEXTSIZE),
        legend.text = element_text(size=TEXTSIZE),
        axis.text.x = element_text(angle = 90, hjust = 1, size=TEXTSIZE))

ggplot(gene_counts_df, aes(factor(cluster), counts)) + 
  geom_jitter(aes(color = dataset_id),width = 0.25)+
  scale_x_discrete(breaks=seq(1,max(levels(cluster_assignments))),
                   labels=lapply(seq(1,max(levels(cluster_assignments))),toString) )+
  scale_color_hue(labels=shortdatasetnames)+ # note: use fill or color; hue or manual
  ggtitle(paste('Expression of PKP2')) +
  xlab('Cluster')+ylab('Transcript count')+
  theme(#legend.position="none",
    text = element_text(size=TEXTSIZE),
    axis.text = element_text(size=TEXTSIZE),
    plot.title = element_text(size=TEXTSIZE),
    legend.text = element_text(size=TEXTSIZE),
    axis.text.x = element_text(angle = 90, hjust = 1, size=TEXTSIZE))

ggplot()+
  geom_histogram(data=gene_counts_df,aes(x=as.numeric(cluster),fill=dataset_id),position='dodge',binwidth=1)+
  scale_x_continuous(breaks=seq(1,max(levels(cluster_assignments))),
                   labels=lapply(seq(1,max(levels(cluster_assignments))),toString) )+
  xlab('Cluster')+
scale_fill_hue(labels=shortdatasetnames)
  
