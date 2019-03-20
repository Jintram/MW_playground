


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









################################################################################################
# FINISH RUNNING ANALYSIS BELOW -- DESeq takes ages so needs to run for longer.
################################################################################################

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

df_res<-as.data.frame(res)

# Re-do previous analysis -- use cluster 5 vs. not cluster 5 ===========================================

# define sets
indices_set1 <- which(dataframe_cells$cluster==5)# & dataframe_cells$condition==2)
indices_set2 <- which(dataframe_cells$cluster!=5)# & dataframe_cells$condition==1)
# calculate differential expression
fn_output<-get_differential_gene_expression(indices_set1,indices_set2,
                                            all_gene_expression,all_gene_expression_raw,
                                            method='none',pcutoff=0.01) # min_same
df_fc_cluster5_mw_method<-fn_output[[1]]

# Also look at built-in RaceID2 results =================================================================

diffExp <- clustdiffgenes(groupedSCS$Combined,pvalue=1)

# Now compare ===========================================================================================

darkColors <- brewer.pal(brewer.pal.info["Dark2", "maxcolors"], "Dark2")

# now create params to use for comparison 
#mw
fc_mw    <- df_fc_cluster5_mw_method$fc[order(df_fc_cluster5_mw_method$fc,decreasing=T)]
pv_mw    <- df_fc_cluster5_mw_method$pv[order(df_fc_cluster5_mw_method$fc,decreasing=T)]
#raceID2
fc_ra  <- diffExp$cl.5$fc[order(diffExp$cl.5$fc,decreasing=T)]
pv_ra  <- diffExp$cl.5$pv[order(diffExp$cl.5$fc,decreasing=T)]
#DESeq
fc_de <- 2^-df_res$log2FoldChange[order(2^-df_res$log2FoldChange,decreasing=T)]
pv_de <- df_res$pvalue[order(2^-df_res$log2FoldChange,decreasing=T)]
# combined pv
pv_color_de_mw <- (pv_mw<0.01)*0.33+(pv_de<0.01)*0.66
pv_color_de_ra <- (pv_ra<0.01)*0.33+(pv_de<0.01)*0.66
pv_color_mw_ra <- (pv_ra<0.01)*0.33+(pv_mw<0.01)*0.66
# keys
keys_lab=list()
keys_pos <- c(0,0.33,0.66,.99)
keys_lab[[1]] <- c('not sign.','mw sign.','deseq sign.','both sign.')
keys_lab[[2]] <- c('not sign.','race sign.','deseq sign.','both sign.')
keys_lab[[3]] <- c('not sign.','race sign.','mw sign.','both sign.')

# create dataframe for scatter plot
df_scatter=data.frame(fc_mw=fc_mw,
                      fc_de=fc_de,
                      fc_ra=fc_ra,
                      pv_color_de_mw=pv_color_de_mw,
                      pv_color_de_ra=pv_color_de_ra,
                      pv_color_mw_ra=pv_color_mw_ra)

for (idx in 1:3) {

  if (idx==1) {
    myxlab<-'MW'; myylab<-'DESeq'
    x_str<-'fc_mw';y_str<-'fc_de';
    pv_str<-'pv_color_de_mw'
  } else if (idx==2) {
    myxlab<-'RaceID2'; myylab<-'DESeq'
    x_str<-'fc_ra';y_str<-'fc_de';
    pv_str<-'pv_color_de_ra'
  } else if (idx==3) {
    myxlab<-'MW'; myylab<-'RaceID2'
    x_str<-'fc_mw';y_str<-'fc_ra';
    pv_str<-'pv_color_mw_ra'
  }
  
  # plot
  xyline=data.frame(x=seq(0,10,1),y=seq(0,10,1))
  p<-ggplot(data=df_scatter)+
    geom_point(aes_string(x=x_str,y=y_str,color=factor(pv_color_mw_ra)))+
    xlab(paste0('fold change calculated by ',myxlab))+ylab(paste0('fold change calculated by ',myylab))+
    scale_color_manual(values=c('grey','red','orange','blue'),
                                               breaks=factor(keys_pos),
                                               labels=keys_lab[[idx]])+
    #scale_colour_gradientn(colors=c('grey','orange','blue','black'),    
    #                       breaks=keys_pos,
    #                       labels=keys_lab[[idx]])+
    coord_fixed(ratio = 1)+
    geom_line(data=xyline,aes(x=x,y=y))+
    give_better_textsize_plot(20)
  
  print(p)

#geom_point(aes(x=fc_mw,y=fc_de,color=pv_color_de_mw))+
#xlab('MW')+ylab('DESeq')+
#geom_point(aes(x=fc_ra,y=fc_de,color=pv_color_de_ra))+
#xlab('RaceID2')+ylab('DESeq')+
#geom_point(aes(x=fc_mw,y=fc_ra,color=pv_color_mw_ra))+
#xlab('MW')+ylab('RaceID2')+

}
















