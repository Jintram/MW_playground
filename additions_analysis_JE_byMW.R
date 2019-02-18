
require('scales')
require("RColorBrewer")

# Create wide array of colors
n <- 60
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

# Gene expression compared between conditions ==========================================

gene_of_interest <- 'LEPROTL1'

datasetnames <- c('IPS_pkp2_reverted', 'IPS_pkp2_mutant')
shortdatasetnames <- c('reverted', 'mutant')
  
# editing here to combine the two dataset distributions in one plot!
gene_counts=list()
freq_list=list()
freq_frame <- data.frame(centers=list(), counts=list(), dataset_id=factor())
gene_counts_df <- data.frame(counts=numeric(), dataset_id=factor())
for (ii in seq(1,length(datasetnames))) {

  datasetname<-datasetnames[ii]
  
  #gene_data_df <- groupedSCS$IPS_pkp2_reverted@expdata
  #gene_data_df <- groupedSCS$IPS_pkp2_mutant@expdata
  gene_data_df <- groupedSCS[[datasetname]]@expdata
  
  gene_names <- rownames(gene_data_df)
  gene_of_interest_idx[ii] <- pmatch(gene_of_interest,gene_names)
  gene_oi_realname[ii] <- gene_names[gene_of_interest_idx[ii]]
  
  if (length(gene_of_interest_idx[ii])>1) {
    print('WARNING: Found multiple instances of gene')
  }
  
  #gene_counts <- as.numeric(gene_data_df[gene_of_interest_idx,])
  gene_counts[[ii]] <- as.numeric(gene_data_df[gene_of_interest_idx[ii],])
  gene_counts_df <- rbind(gene_counts_df,data.frame(counts    =gene_counts[[ii]], 
                                                    dataset_id=as.factor(rep(ii, length(gene_counts[[ii]]) ))))
  sum_gene_counts[ii] <- sum(gene_counts[[ii]])
  zero_count[ii] <- sum(gene_counts[[ii]]==0)
  mybinwidth=ceiling((max(gene_counts[[ii]])+1)/40)
  freq <- hist(x=gene_counts[[ii]],
               breaks=seq(-0.5,max(gene_counts[[ii]])+mybinwidth,mybinwidth),
               plot=FALSE)
  freq_list[[length(freq_list)+1]]<-freq
  
  # Create df of freq for later plotting
  freq_frame <- rbind(freq_frame,data.frame(centers = freq$mids, counts = freq$counts, dataset_id=as.factor(ii)))
  
}
  
# plot ------------------------------------------------------------------------------

# Plot the last one
TEXTSIZE=15
ggplot(data=freq_frame, mapping=aes(x=centers, y=counts,fill=dataset_id)) +
  geom_bar(stat="identity")+
  #geom_line()+
  #geom_point()+
  xlab("Transcript count")+
  ylab("Number of times observed")+
  ggtitle(paste("Distribution of transcript counts of ", gene_oi_realname,'\nZero count = ',toString(zero_count))) +
  theme(text = element_text(size=TEXTSIZE),
        axis.text = element_text(size=TEXTSIZE),
        plot.title = element_text(size=TEXTSIZE))+
  scale_x_continuous(labels = comma)+
  scale_y_continuous(labels = comma)
  
# Plot violin ------------------------------------------------------------------------------
TEXTSIZE=15
ggplot(gene_counts_df, aes(factor(dataset_id), counts)) + 
  geom_violin(aes(fill = dataset_id))+
  scale_x_discrete(breaks=seq(1,length(datasetnames)),
                   labels=shortdatasetnames)+
  ggtitle(paste("Distribution of transcript counts of ", gene_oi_realname,'\nZero count = ',toString(zero_count))) +
  xlab(element_blank())+ylab('Transcript count')+
  theme(legend.position="none",
    text = element_text(size=TEXTSIZE),
    axis.text = element_text(size=TEXTSIZE),
    plot.title = element_text(size=TEXTSIZE),
    legend.text = element_text(size=TEXTSIZE),
    axis.text.x = element_text(angle = 90, hjust = 1, size=TEXTSIZE))


# ==================================================================================================
# Plotting dimensionality-reduced data, giving markup according to condition =======================
# ==================================================================================================

# Create cluster assignments in factor variable
NR_CONDITIONS=2
CONDITION_MARKERS <- c(0,8)

conversion_searchterms <- c('HUB-AK-003', 'HUB-AK-004', 'HUB-AK-005' , 'HUB-AK-006')
conversion_numbers     <- factor(c(1,1,2,2), levels= seq(0,NR_CONDITIONS))
names_to_convert_factors = rownames(groupedSCS$Combined@tsne)
condition_factors <- factor(rep(0,length(names_to_convert_factors)), 
       levels= seq(0,NR_CONDITIONS))
for (ii in seq(1,length(conversion_numbers))) {
  hit_idxs<-which(grepl(conversion_searchterms[ii],names_to_convert_factors))
  condition_factors[hit_idxs]<-conversion_numbers[ii]
}

# Put clustering and assignments in plotable dataframe
cluster_assignments <- as.factor(groupedSCS$Combined@cluster$kpart)
nr_clusters <- max(levels(cluster_assignments))
df_tsne <- data_frame(V1=groupedSCS$Combined@tsne$V1,
                      V2=groupedSCS$Combined@tsne$V2,
                      cluster_assignments=cluster_assignments,
                      condition=condition_factors)

# Get some colors
n <- 60
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# show color palette in pie chart
pie(rep(1,n), col=sample(col_vector, n))

# Plot the scatter plot
p_tsne_cond_clust<-plot_scatter_w_highlighted_clusters_condition(df_tsne,'V1','V2','cluster_assignments','condition_factors',
                                              condition_names=shortdatasetnames,condition_markers=CONDITION_MARKERS,
                                              'tSNE1','tSNE2','Gene expression space',col_vector)
ggsave(paste(directory_with_data,"plots_MW/tsne_highlights_cond_clust.pdf",sep=""), width=10, height=6)
ggsave(paste(directory_with_data,"plots_MW/tsne_highlights_cond_clust.png",sep=""), width=10, height=6)


# ==================================================================================================
# Making a plot with gradient according to gene expression =========================================
# You need to have run the previous section to do this =============================================
# ==================================================================================================

celltype_for_these_markers <- 'Epicardial'
list_of_interesting_genes <- c('WT1','TBX18','ADLH1A2','ZO1','BNC1','ANXA8','K18','KRT8','KRT19','GPM6A','UP1KB','CDH1','UPK3B')

celltype_for_these_markers <- 'Fat'
list_of_interesting_genes <- c('PPARG','PPARGC1A','UCP1','EDNRB','CEBPA','CEBPB','EBF3','RORA','FABP4','PLIN','PDGFRA','ADIPOQ','LEP','DLK1','APOE','LIPE','GLUT4','KLF5')	

celltype_for_these_markers <- 'Fibroblast'
list_of_interesting_genes <- c('FN1','POSTN','VIM','ACTA2','COL1A1','COL1A2','COL2A1','COL9A1','CDH2','FSTL1','COL3A1','GSN','FBLN2','SPARC','MMP','DDR2','FSP1','PDGFRA','THY1','FLNA','KLF5')

celltype_for_these_markers <- 'Differentatiors'
list_of_interesting_genes <- c('TFAP2A','TFAP2B','TFAP2C','TFAP2D','TFAP2E')

celltype_for_these_markers <- 'Desmosome'
list_of_interesting_genes <- c('PKP2','DSP','DSC2','DSG2','JUP','GJA1')

freq_df<-data.frame(centers = numeric(), counts = numeric(), my_gene_nr=factor(levels=seq(1,length(list_of_interesting_genes))))
found_genes<-list()
for (ii in seq(1,length(list_of_interesting_genes))) {
  
  current_gene_interest<-list_of_interesting_genes[ii]
  
  # Go over interesting genes and gather their expression
  all_gene_expression <- groupedSCS$Combined@fdata
  gene_names <- rownames(all_gene_expression)
  gene_idxs<-which(grepl(current_gene_interest,gene_names))
  
  if (length(gene_idxs)==0){
    print(paste(current_gene_interest, ": None found -- skipping")) 
    next
  } else if (length(gene_idxs)>1) {
    print(paste(current_gene_interest, ": Multiple found -- skipping")) 
    next
  } else {
    print(paste(gene_names[gene_idxs], ' FOUND; index= ', gene_idxs, '.'),sep="")
  }
  
  found_genes[length(found_genes)+1] <- gene_names[gene_idxs] # note there should only be one!
  
  selected_gene_expression <- all_gene_expression[gene_idxs,]
  selected_gene_expression_resized<-selected_gene_expression/max(selected_gene_expression)*2
  
  current_df <- pump_out_freq_df(p,selected_gene_expression, 'blue', 0)
  freq_df <- rbind(freq_df,current_df)

}

# Create a combined histogram plot
TEXTSIZE=15
p_overview_hist<-ggplot()+
  geom_line(data=freq_df,stat="identity", mapping=aes(x=centers, y=counts,color=my_gene_nr))+
  scale_color_manual(values=col_vector,labels=found_genes)+
  ggtitle(paste('Gene expression for ',celltype_for_these_markers,' markers'))+
  theme(#legend.position="none",
    text = element_text(size=TEXTSIZE),
    axis.text = element_text(size=TEXTSIZE),
    plot.title = element_text(size=TEXTSIZE),
    legend.text = element_text(size=TEXTSIZE))
p_overview_hist
# Save
ggsave(paste(directory_with_data,'plots_MW/histogram_gene_expression2_',celltype_for_these_markers,'.pdf',sep=""), width=10, height=6)
ggsave(paste(directory_with_data,'plots_MW/histogram_gene_expression2_',celltype_for_these_markers,'.png',sep=""), width=10, height=6)


# OK now go ahead -----------------

GENE_OF_INTEREST<-3625 # KRT19__chr17 (epi)
GENE_OF_INTEREST<-371 # APOE__chr19 (fat)
GENE_OF_INTEREST<-5063 # PDGFRA__chr4 (fibro)
GENE_OF_INTEREST<-1480 # COL1A1__chr17 (fibro)
GENE_OF_INTEREST<-7216 # TFAP2A__chr6
GENE_OF_INTEREST<-1994 # DSG2__chr18

name_of_this_gene<-gene_names[GENE_OF_INTEREST]

all_gene_expression <- groupedSCS$Combined@fdata
selected_gene_expression<-all_gene_expression[GENE_OF_INTEREST,]

# Add the expression to the dataframe in another column
df_tsne <- data_frame(V1=groupedSCS$Combined@tsne$V1,
                      V2=groupedSCS$Combined@tsne$V2,
                      cluster_assignments=cluster_assignments,
                      condition=condition_factors,
                      selected_gene_expression=as.numeric(selected_gene_expression))
#or use mutate(df_tsne,)? I think that's not convenient when we update the parameter

# Make the scatter plots
savelocation<-paste(directory_with_data,'plots_MW/tsne_combined_cluster_genexpr_',celltype_for_these_markers,'_',GENE_OF_INTEREST,'.pdf',sep="")
ppplist<-
  plot_scatter_w_highlighted_clusters_condition_exprgrad(
    df_tsne,'V1','V2','cluster_assignments','condition_factors',
    condition_names=shortdatasetnames,condition_markers=CONDITION_MARKERS,
    'tSNE1','tSNE2',
    paste('Expression of ',name_of_this_gene,' (',celltype_for_these_markers,')', sep=''),
    col_vector,
    selected_gene_expression_varname='selected_gene_expression',
    savelocation=savelocation
    )

# Retrieve the different subplots
p_tsne_cond_clust_expr<-ppplist[1]
p_tsne_cond_clust_normal<-ppplist[2]

# Save gradient only plot
p_tsne_cond_clust_expr
ggsave(paste(directory_with_data,'plots_MW/tsne_gene_expression2_',celltype_for_these_markers,'_',GENE_OF_INTEREST,'.pdf',sep=""), width=10, height=6)
ggsave(paste(directory_with_data,'plots_MW/tsne_gene_expression2_',celltype_for_these_markers,'_',GENE_OF_INTEREST,'.png',sep=""), width=10, height=6)


# Additional standard plot as people made it before ----------------------------------------
CONDITION_MARKERS <- c(15,16)
plot_scatter_gene_expression(
  df_tsne,'V1','V2','cluster_assignments','condition_factors',
  condition_names=shortdatasetnames,condition_markers=CONDITION_MARKERS,
  'tSNE1','tSNE2',
  paste('Expression of ',name_of_this_gene,' (',celltype_for_these_markers,')',sep=''),
  col_vector,
  selected_gene_expression_varname='selected_gene_expression'
)
ggsave(paste(directory_with_data,'plots_MW/tsne_gene_expression_',celltype_for_these_markers,'_',GENE_OF_INTEREST,'.pdf',sep=""), width=10, height=6)
ggsave(paste(directory_with_data,'plots_MW/tsne_gene_expression_',celltype_for_these_markers,'_',GENE_OF_INTEREST,'.png',sep=""), width=10, height=6)



