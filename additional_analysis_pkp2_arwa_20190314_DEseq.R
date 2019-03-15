


# Now let's try to use the DEseq package on Arwa's dataset.

require(DESeq2)

# Again collect some parameters  =================================================================

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

# Determine the conditions ==============================================================================

# wild type, cluster 5
indices_set1 <- which(dataframe_cells$cluster==5 & dataframe_cells$condition==1)
# mutant, cluster 5
indices_set2 <- which(dataframe_cells$cluster==5 & dataframe_cells$condition==2) 
  
# combine data
df_set1_and_set2 <- all_gene_expression[,c(indices_set1, indices_set2)]



# Run the DESeq analysis ================================================================================

# determine condition
mycondition <-factor(c(rep(1,length(indices_set1)),rep(2,length(indices_set2))))
# colnames
mycolnames  <- colnames(df_set1_and_set2)
# libType
my_libType  <- rep("single-end", dim(df_set1_and_set2)[2])

# run on sc@expdata
des <- data.frame( row.names = mycolnames, 
                   condition = mycondition, 
                   libType = my_libType)


cds <- DESeqDataSetFromMatrix(countData=round(df_set1_and_set2,0),colData=des,design =~ condition)#,...) 
  # note: dominique rounds the data
cds <- DESeq(cds,fitType='local')
res <- results(cds)

# convert to dataframe
df_res<-as.data.frame(res)

# now select & process
# establish selection indices (ones with a p-value<0.1 and not NA)
sel_idxs<-df_res$pvalue<0.1&(!is.na(df_res$pvalue))
# apply selection
df_res_sel <- df_res[sel_idxs,]
# obtain gene names
gene_names_sel <- rownames(df_res)[sel_idxs]
# add fold-change column
df_res_sel<-mutate(df_res_sel,
                   fc=2^df_res_sel$log2FoldChange,
                   fc_inv=2^-df_res_sel$log2FoldChange,
                   gene_name=gene_names_sel)
# re-establish rownames
rownames(df_res_sel)<-gene_names_sel
# order by FC
df_res_sel<-df_res_sel[order(df_res_sel$fc,decreasing = T),]


# ########################################################################
# TODO: Also save these results to an excel file here!
# ########################################################################


# Now make the bar plots also ====================================

# Plot 1

# Select and plot top increased genes
df_top_selection<-df_res_sel[1:10,]
df_top_selection<-mutate(df_top_selection,n321=as.factor(seq(nrow(df_top_selection),1,-1)))
barplot_differential_expression_v2(df_top_selection,
                                   differential_expression_varname='fc',
                                   center_varname='n321',
                                   gene_name_varname='gene_name',
                                   lowcol='red',highcol='firebrick4',
                                   ylabtext='Times higher in mutant',
                                   mytitle=paste('Differential gene expression ()',sep=''))

# Plot 2 ===

# Select and plot top decreased genes
df_top_decr_selection<-df_res_sel[nrow(df_res_sel):(nrow(df_res_sel)-9),]
df_top_decr_selection<-mutate(df_top_decr_selection,
                              n123=as.factor(seq(1,nrow(df_top_decr_selection))),
                              n321=as.factor(seq(nrow(df_top_decr_selection),1,-1)))
barplot_differential_expression_v2(df_top_decr_selection,
                                   differential_expression_varname='fc_inv',
                                   center_varname='n321',
                                   gene_name_varname='gene_name',
                                   lowcol='skyblue',highcol='royalblue4',
                                   ylabtext='Times lower in mutant',
                                   mytitle=paste('Differential gene expression ()',sep=''))











# Now cross-reference DESeq w/ previous results ==========================================================

# Run DESeq on cluster 5 vs. non cluster 5 ===============================================================

# wild type, cluster 5
indices_set1 <- which(dataframe_cells$cluster==5)
# mutant, cluster 5
indices_set2 <- which(dataframe_cells$cluster!=5) 

# combine data
df_set1_and_set2 <- all_gene_expression[,c(indices_set1, indices_set2)]

# determine condition
mycondition <-factor(c(rep(1,length(indices_set1)),rep(2,length(indices_set2))))
# colnames
mycolnames  <- colnames(df_set1_and_set2)
# libType
my_libType  <- rep("single-end", dim(df_set1_and_set2)[2])

# run on sc@expdata
des <- data.frame( row.names = mycolnames, 
                   condition = mycondition, 
                   libType = my_libType)


cds <- DESeqDataSetFromMatrix(countData=round(df_set1_and_set2,0),colData=des,design =~ condition)#,...) 
# note: dominique rounds the data
cds <- DESeq(cds,fitType='local')
res <- results(cds)


# Re-do previous analysis -- use cluster 5 vs. not cluster 5 ===========================================

# define sets
indices_set1    <- which(dataframe_cells$cluster==5)# & dataframe_cells$condition==2)
indices_set2  <- which(dataframe_cells$cluster!=5)# & dataframe_cells$condition==1)
# calculate differential expression
fn_output<-get_differential_gene_expression(indices_set1,indices_set2,
                                            all_gene_expression,all_gene_expression_raw,
                                            method='min',pcutoff=0.01)
df_fc_cluster5_mw_method<-fn_output[[1]]

# Now compare ===========================================================================================

# now create params to use for comparison 
fc_mw    <- df_fc_cluster5_mw_method$fc
pv_mw    <- df_fc_cluster5_mw_method$pv
fc_deseq <- 2^df_res$log2FoldChange
pv_deseq <- df_res$pvalue
# combined pv
pv_color <- (pv_mw<0.01)*0.5+(pv_deseq<0.01)*0.5

# create dataframe for scatter plot
xyline=data.frame(x=seq(0,10,1),y=seq(0,10,1))
df_scatter=data.frame(fc_mw=fc_mw,fc_deseq=fc_deseq,pv_color=pv_color)
# plot
ggplot(data=df_scatter)+
  geom_point(aes(x=fc_mw,y=fc_deseq,color=pv_color))+
  scale_colour_gradientn(colors=c('red','orange','green'))+
  coord_fixed(ratio = 1)+
  geom_line(data=xyline,aes(x=x,y=y))+
  give_better_textsize_plot(20)+
  xlab('Martijn calculation')+ylab('DESeq calculation')

















