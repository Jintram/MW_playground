
# Load libraries =========================================

require('scales')
require("RColorBrewer")
require('dplyr')
require('grid')
require('gridExtra')

# My own libraries
source(paste0("/Users/m.wehrens/Documents/git_repos/MW_playing/","my_functions_standard_analysis.R"))
source(paste0("/Users/m.wehrens/Documents/git_repos/MW_playing/","my_functions_standard_plots.R"))

# Some config parameters =========================================

# Create wide array of colors
n <- 60
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

# Extra information is on which plate is what
# This is needed for e.g. comparing expression
conversion_searchterms <- c('HUB-AK-003', 'HUB-AK-004', 'HUB-AK-005' , 'HUB-AK-006')
conversion_numbers     <- factor(c(1,1,2,2), levels= seq(0,NR_CONDITIONS))
datasetnames <- c('IPS_pkp2_reverted', 'IPS_pkp2_mutant')
shortdatasetnames <- c('reverted', 'mutant')

# Create save directory MW
subMW_outputDir_path<-paste0(directory_with_data,outputDir,'plots_MW/')
if(!dir.exists(subMW_outputDir_path)) {dir.create(subMW_outputDir_path, showWarnings = TRUE, recursive = TRUE, mode = "0777")}

# Gene expression compared between conditions ==========================================

gene_of_interest <- 'LEPROTL1'
gene_of_interest <- 'PKP2__chr12'
  
# editing here to combine the two dataset distributions in one plot!
gene_counts<-list()
freq_list<-list()
gene_oi_realname<-list()
sum_gene_counts<-list()
zero_count<-list()
freq_frame <- data.frame(centers=list(), counts=list(), dataset_id=factor())
gene_counts_df <- data.frame(counts=numeric(), dataset_id=factor())
gene_of_interest_idx<-numeric()
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
  #geom_violin(aes(fill = dataset_id))+
  geom_boxplot(aes(fill = dataset_id))+
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

shortdatasetnames[1]
mean(gene_counts_df$counts[gene_counts_df$dataset_id==1])

# ==================================================================================================
# Plotting dimensionality-reduced data, giving markup according to condition =======================
# ==================================================================================================

# Create cluster assignments in factor variable
NR_CONDITIONS=2
CONDITION_MARKERS <- c(0,8)

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
print(p_tsne_cond_clust)

# Create save directory MW
subMW_outputDir_path<-paste0(directory_with_data,outputDir,'plots_MW/')
if(!dir.exists(subMW_outputDir_path)) {dir.create(subMW_outputDir_path, showWarnings = TRUE, recursive = TRUE, mode = "0777")}

# Save
ggsave(paste(subMW_outputDir_path,"tsne_highlights_cond_clust.pdf",sep=""), width=10, height=6)
ggsave(paste(subMW_outputDir_path,"tsne_highlights_cond_clust.png",sep=""), width=10, height=6)

# ==================================================================================================
# Create heat map ==================================================================================
# ==================================================================================================

cclmo<-clustheatmap(groupedSCS$Combined,final=T)
# TODO: still have to find a way to save and edit this plot automatically.. (Maybe check Joep's code?)

#ggtitle('Cellular correlations; 1 minus pearson coefficient')
#ggsave(paste(directory_with_data,"plots_MW/tsne_highlights_cond_clust.pdf",sep=""), width=10, height=6)
#ggsave(paste(directory_with_data,"plots_MW/tsne_highlights_cond_clust.png",sep=""), width=10, height=6)


# ==================================================================================================
# Making a plot with gradient according to gene expression =========================================
# You need to have run the previous section to do this =============================================
# ==================================================================================================

# TODO: is ADLH1A2_ a typo??? ALDH1A2 perhaps? seems so: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5772986/

marker_list_of_lists <- list(list('^WT1_','^TBX18_','^ADLH1A2_','^ALDH1A2_','^ZO1_','^BNC1_','^ANXA8_','^K18_','^KRT8_','^KRT19_','^GPM6A_','^UP1KB','^CDH1_','^UPK3B_'),
                             list('^PPARG_','^PPARGC1A_','^UCP1_','^EDNRB_','^CEBPA_','^CEBPB_','^EBF3_','^RORA_','^FABP4_','^^PLIN__','^PDGFRA_','^ADIPOQ_','^LEP_','^DLK1_','^APOE_','^LIPE_','^GLUT4_','^KLF5_'),
                             list('^FN1_','^POSTN_','^VIM_','^ACTA2_','^COL1A1_','^COL1A2_','^COL2A1_','^COL9A1_','^CDH2_','^FSTL1_','^COL3A1_','^GSN_','^FBLN2_','^SPARC_','^MMP_','^DDR2_','^FSP1_','^PDGFRA_','^THY1_','^FLNA_','^KLF5_'),
                             list('^TFAP2A_','^TFAP2B_','^TFAP2C_','^TFAP2D_','^TFAP2E_'),
                             list('^PKP2_','^DSP_','^DSC2_','^DSG2_','^JUP_','^GJA1_'))

marker_names_list <- list('Epicardial','Fat','Fibroblast','Differentiators','Desmosome')

SUBPLOTDIR='markers_arwa/'

# ==================================================================================================

# List of lists for Iliana markers

marker_list_of_lists <- list(list('^NGF_','^GDNF_','^NTF3_','^LIF_','^FGF1_','^EGF_','^FGF2_','^LAMA2_','^MANF_','^FKBP1B_','^NRG1_','^FKBP3_'),
                        list('^UCP1_','^CPT1B_','^PRDM16_','^PPARGC1A_','^COX4I1_'),
                        list('^ZIC1_','^LHX8_','^MTUS1_'),
                        list('^TBX1_','^SLC36A2_','^TMEM26_','^P2RX5_','^TNFRSF9_','^KCNK3_'),
                        list('^HOXC9_','^SLC7A10_','^SHOX2_'),
                        list('^CYBA_','^HIF1A_','^TGFB1_','^NCF1_'),
                        list('^SLC2A4_','^CEBPA_','^ADIPOQ_','^FABP4_','^PPARG_'),
                        list('^ADRB1_','^ADRB2_','^ADRB3_','^ADRA2A_'),
                        list('^ELOVL3_','^ACADM_','^ACOX1_','^ACADVL_'),
                        list('^HSPD1_','^HSPE1_','^HSPA9_','^TRAP1_'),
                        list('^TFAM_','^CYC1_'),
                        list('^MFN2_'))

# and their names
marker_names_list <- list('Neurotrophic',
                          'Thermogenic',
                          'Brown fat',
                          'Beige fat',
                          'White fat',
                          'Oxidative_stress',
                          'Adipose_tissue_g_f',
                          'Adrenergic_R',
                          'Lipid_synth_ox',
                          'mt-stress response',
                          'mt_biogenesis',
                          'mt_fusion')

SUBPLOTDIR='markers_iliana/'

# ================================================================================================== 

marker_list_of_lists <- list(list('^GATA4_','^NRTN_'))

# and their names
marker_names_list <- list('misc')

SUBPLOTDIR='markers_arwa/'

# ================================================================================================== 

marker_list_of_lists <- list(list('^AXIN2_',
                                  '^TCF4_',
                                  '^ALDH1A1_',
                                  '^CCND1_',
                                  '^CCND2_',
                                  '^WNT3A_',
                                  '^WNT5A_'))

# and their names
marker_names_list <- list('Wnt')

SUBPLOTDIR='markers_arwa/'


# ==================================================================================================

SUBPLOTDIR='markers_iliana2/'

marker_list_of_lists<-list(
list('^SLC22A5_','^CPT1A_','^CPT1B_','^CPT1C_','^SLC25A2_','^CPT2_','^ACADVL_','^ACADM_','^ACADS_','^HADHA_','^HADHB_','^HADH_','^ACAA2_','^ETFDH_','^ETFA_','^ETFB_','^DECR1_'),
list('^PNPLA2_','^LIPE_','^MGLL_','^PNPLA3_','^PNPLA4_','^CES1_','^ABHD5_','^G0S2_','^PLIN1_','^PLIN2_','^PLIN3_','^PLIN5_','^CIDEC_','^FOXO1_','^FABP4_','^CAV1_','^AQP7_'),
list('^ACACA_','^FASN_','^DGAT1_','^AGPAT2_','^SREBF1_'),
list('^PPARGC1A_','^CRTC3_','^NRF1_','^ESRRA_')
)

marker_names_list <- list('FAO',
                          'Lipolysis',
                          'Lipogenesis',
                          'mt biogenesis')
# ==================================================================================================

SUBPLOTDIR='markers_iliana3/'

marker_list_of_lists<-list(
  list('^BDNF_','^NTF4_','^NTF3_'),
  list('^CNTF_','^CTF1_','^OSM_','^TNF_'), # CNTF aka ^HCNTF_ // OSM aka '^MGC20461_',
  list('^NRTN_','^ARTN_','^PSPN_'),
  list('^NRP1_', '^SEMA3A_', '^BMP7_')
)

marker_names_list <- list('Neurotrophins',
                          'Neurokines',
                          'GDNF_family',
                          'Other')

# ==================================================================================================

SUBPLOTDIR='markers_andrea/'

marker_list_of_lists<-list(
  list('^AHCYL1_','^SCEL_','^SPATA22_', '^WT1_')#' spata22 aka ^NYD-SP20_')#
)

marker_names_list <- list('yeast2hybrid')

# ==================================================================================================

SUBPLOTDIR='markers_andrea2/'

marker_list_of_lists<-list(
  list('^PKP2_','^AGR3_','^AHCYl1_','^ALDH9A1_','^ANKRD11_','^ANXA1_','^AOX1_','^ATPF1B_','^C1orf100_','^CCDC80_','^CEP70_','^CHD3_','^COL1a2_','^COL3A1_','^COPS4_','^CTSB_','^CTSL_','^DLD_','^ELP1_','^EMC2_','^FMNL2_','^GAREM1_','^GCOM1_','^GOSR1_','^HECTD3_','^HSD17B2_','^IFT88_','^ITGB1_','^KIAA0586_','^KIZ_','^KLHL20_','^KLHL32_','^LRRC36_','^MCTP2_','^METTL23_','^MLH1_','^MNAT1_','^MYZAP_','^N4BP2L2_','^NCKAP1_','^NSMAF_','^PAN3_','^PDGFRB_','^PDHB_','^PHC1_','^PHKB_','^POLR1C_','^POLR2G_','^POLR3F_','^PPP2R1B_','^PREPL_','^PRR4_','^PSMC1_','^RIPOR1_','^RMND5A_','^SCAF11_','^SCEL_','^SFPQ_','^SPATA22_','^STAMBP_','^TOX4_','^TSPAN7_','^TTN_','^VIM_','^WDR19_','^WDR61_','^ZNF19_','^ZNF251_','^ZNF350_','^ZNF577_')  
  )

marker_names_list <- list('yeast2hybrid')

# ==================================================================================================

mylimits=NULL # can be used to impose custom limits
# mylimits <- c(0,2*pkp2_expression_098)

savesuffix=''

if (!is.null(mylimits)) {
  savesuffix='_customlims'
}

# make output directory
outputDir_path<-paste0(directory_with_data, 'plots_MW/',SUBPLOTDIR)
if(!dir.exists(outputDir_path)) {dir.create(outputDir_path, showWarnings = TRUE, recursive = TRUE, mode = "0777")}

# perform analysis
for (marker_idx in 1:length(marker_names_list)) {
  
  # now run over the marker list 
  # Note: names of 2 params below should be given more clear names
  list_of_interesting_genes<-marker_list_of_lists[[marker_idx]]
  celltype_for_these_markers<-marker_names_list[marker_idx]
  
  freq_df<-data.frame(centers = numeric(), counts = numeric(), my_gene_nr=factor(levels=seq(1,length(list_of_interesting_genes))))
  found_genes<-list()
  list_of_genes_of_interest<-numeric()
  for (ii in seq(1,length(list_of_interesting_genes))) {
    
    current_gene_interest<-list_of_interesting_genes[ii]
    
    # Go over interesting genes and gather their expression
    all_gene_expression <- groupedSCS$Combined@ndata
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
  
    list_of_genes_of_interest[length(list_of_genes_of_interest)+1]<-gene_idxs[1]
    
  }
  
  # Create a combined histogram plot
  TEXTSIZE=15
  p_overview_hist<-ggplot()+
    geom_line(data=freq_df,stat="identity", mapping=aes(x=centers, y=counts,color=my_gene_nr))+
    geom_point(data=freq_df, mapping=aes(x=centers, y=counts,color=my_gene_nr))+
    scale_color_manual(values=col_vector,labels=found_genes)+
    ggtitle(paste('Gene expression for ',celltype_for_these_markers,' markers'))+
    theme(#legend.position="none",
      text = element_text(size=TEXTSIZE),
      axis.text = element_text(size=TEXTSIZE),
      plot.title = element_text(size=TEXTSIZE),
      legend.text = element_text(size=TEXTSIZE))+
    xlab('Transcript count (normalized)')+
    ylab('Counts')
  print(p_overview_hist)
  # Save
  ggsave(paste(directory_with_data,'plots_MW/',SUBPLOTDIR,'histogram_gene_expression2_',celltype_for_these_markers,'.pdf',sep=""), width=10, height=6)
  ggsave(paste(directory_with_data,'plots_MW/',SUBPLOTDIR,'histogram_gene_expression2_',celltype_for_these_markers,'.png',sep=""), width=10, height=6)
    # p_overview_hist<-p_overview_hist+theme(legend.position = 'none'); p_overview_hist
  
  # OK now go ahead with tsne overviews

  # Run over genes automatically
  for (GENE_OF_INTEREST in list_of_genes_of_interest) {
    
    name_of_this_gene<-gene_names[GENE_OF_INTEREST]
    
    all_gene_expression <- groupedSCS$Combined@ndata # normalized data
    #all_gene_expression <- groupedSCS$Combined@expdata
    selected_gene_expression<-all_gene_expression[GENE_OF_INTEREST,]
    
    # Add the expression to the dataframe in another column
    df_tsne <- data_frame(V1=groupedSCS$Combined@tsne$V1,
                          V2=groupedSCS$Combined@tsne$V2,
                          cluster_assignments=cluster_assignments,
                          condition=condition_factors,
                          selected_gene_expression=as.numeric(selected_gene_expression))
    #or use mutate(df_tsne,)? I think that's not convenient when we update the parameter
    
    # Make the scatter plots
    savelocation<-paste(directory_with_data,'plots_MW/',SUBPLOTDIR,'tsne_combined_cluster_genexpr_',savesuffix,celltype_for_these_markers,'_',name_of_this_gene,'.pdf',sep="")
    ppplist<-
      plot_scatter_w_highlighted_clusters_condition_exprgrad(
        df_tsne,'V1','V2','cluster_assignments','condition_factors',
        condition_names=shortdatasetnames,condition_markers=CONDITION_MARKERS,
        'tSNE1','tSNE2',
        paste('Expression of ',name_of_this_gene,' (',celltype_for_these_markers,')', sep=''),
        col_vector,
        selected_gene_expression_varname='selected_gene_expression',
        savelocation=savelocation,
        mylimits=mylimits
        )
    
    # Retrieve the different subplots
    p_tsne_cond_clust_expr<-ppplist[1]
    p_tsne_cond_clust_normal<-ppplist[2]
    
    # Save gradient only plot
    print(p_tsne_cond_clust_expr) 
    ggsave(paste(directory_with_data,'plots_MW/',SUBPLOTDIR,'tsne_1gene_expression2_',savesuffix,celltype_for_these_markers,'_',name_of_this_gene,'.pdf',sep=""), 
          width=10, height=6)
    ggsave(paste(directory_with_data,'plots_MW/',SUBPLOTDIR,'tsne_1gene_expression2_',savesuffix,celltype_for_these_markers,'_',name_of_this_gene,'.png',sep=""), 
           width=10, height=6)
    
    # Also make plot as was done before
    CONDITION_MARKERS <- c(15,16)
    p_rooijstyle<-plot_scatter_gene_expression(
      df_tsne,'V1','V2','cluster_assignments','condition_factors',
      condition_names=shortdatasetnames,condition_markers=CONDITION_MARKERS,
      'tSNE1','tSNE2',
      paste('Expression of ',name_of_this_gene,' (',celltype_for_these_markers,')',sep=''),
      col_vector,
      selected_gene_expression_varname='selected_gene_expression',
      mylimits=mylimits
    )
    print(p_rooijstyle)
    ggsave(paste(directory_with_data,'plots_MW/',SUBPLOTDIR,'/tsne_gene_expression_',savesuffix,celltype_for_these_markers,'_',name_of_this_gene,'.pdf',sep=""), width=10, height=6)
    ggsave(paste(directory_with_data,'plots_MW/',SUBPLOTDIR,'/tsne_gene_expression_',savesuffix,celltype_for_these_markers,'_',name_of_this_gene,'.png',sep=""), width=10, height=6)
    
  }
  print("one markerlist done")
}
print("all markers done")

#rm('SUBPLOTDIR')


# Additional standard plot as people made it before ----------------------------------------

# ============================================================================================
# Now calculate the differential gene expression =============================================
# ============================================================================================

# First for the two conditions ---------------------------------------------------------------

# calculate average gene expressions for conditions

# this could be done more automatized, but for now let's just do it quick'n'dirty
# Get indices of conditions
cond1_idxs <- which(df_tsne$condition==1)
cond2_idxs <- which(df_tsne$condition==2)
# Pull columns with those indices from the experimental data matrix
all_gene_expression <- groupedSCS$Combined@fdata # note this was also done earlier
cond1_cells_gene_expression<-all_gene_expression[,cond1_idxs]
cond2_cells_gene_expression<-all_gene_expression[,cond2_idxs]
# Calculate average and differential gene expression
cond1_average_gene_expression<-rowMeans(cond1_cells_gene_expression,dim=1)
cond2_average_gene_expression<-rowMeans(cond2_cells_gene_expression,dim=1)
differential_expression<-cond2_average_gene_expression/cond1_average_gene_expression
# Get names for all genes
all_gene_names<-rownames(cond2_cells_gene_expression)
all_gene_names_short<-str_replace_all(all_gene_names,'_.*','')
# set up dataframe with that info
differential_conditions_df <- data_frame(all_gene_names=all_gene_names, 
                                         cond1_average_gene_expression=cond1_average_gene_expression,
                                         cond2_average_gene_expression=cond2_average_gene_expression,
                                         differential_expression=differential_expression,
                                         differential_expression_inv=1/differential_expression,
                                         original_nr=factor(1:length(all_gene_names)))
# Now sort
differential_conditions_df<-differential_conditions_df[order(-differential_conditions_df$differential_expression),]
#differential_conditions_df$n123<-factor(differential_conditions_df$n123, levels=differential_conditions_df$n123)

# Select the highest values ------------------------------------------------------------------------------
selected_data_df<-differential_conditions_df[1:20,]
n123=factor(1:nrow(selected_data_df), levels=1:nrow(selected_data_df))
n123ro=factor(1:nrow(selected_data_df), levels=nrow(selected_data_df):1) # ro = reverse order
selected_data_df<-mutate(selected_data_df,
       n123=n123,
       n123ro=n123ro)

# Now make a bar plot
barplot_differential_expression(selected_data_df=selected_data_df,centers_varname='n123ro',
                                differential_expression_varname='differential_expression',
                                all_gene_names=all_gene_names_short,
                                lowcol='red',highcol='firebrick4',ylabtext='Times higher in mutant',
                                mytitle='Differential gene expression conditions')
# Save 'm
ggsave(paste(directory_with_data,'plots_MW/differential_expression_higher_conditions.pdf',sep=""), width=10, height=6)
ggsave(paste(directory_with_data,'plots_MW/differential_expression_higher_conditions.png',sep=""), width=10, height=6)

# Select the lowest values ------------------------------------------------------------------------------
selected_data_df_low<-differential_conditions_df[nrow(differential_conditions_df):(nrow(differential_conditions_df)-19),]
n123=factor(1:nrow(selected_data_df_low), levels=1:nrow(selected_data_df_low))
n123ro=factor(1:nrow(selected_data_df_low), levels=nrow(selected_data_df_low):1)  # ro = reverse order
selected_data_df_low<-mutate(selected_data_df_low,
       n123=n123,
       n123ro=n123ro)

# Now make a bar plot
barplot_differential_expression(selected_data_df=selected_data_df_low,centers_varname='n123ro',
                                differential_expression_varname='differential_expression_inv',
                                all_gene_names=all_gene_names_short,
                                lowcol='skyblue',highcol='midnightblue',ylabtext='Times lower in mutant',
                                mytitle='Differential gene expression conditions')

# Save 'm
ggsave(paste(directory_with_data,'plots_MW/differential_expression_lower_conditions.pdf',sep=""), width=10, height=6)
ggsave(paste(directory_with_data,'plots_MW/differential_expression_lower_conditions.png',sep=""), width=10, height=6)

# Then for the clusters ----------------------------------------------------------------------

nr_of_clusters<-max(as.numeric(levels((df_tsne$cluster_assignments))))

for (ii in seq(1,nr_of_clusters)) {
  
  # loop over the clusters and get the differential values
  inside_cluster_idxs <- which(df_tsne$cluster_assignments==ii)
  outside_cluster_idxs <- which(df_tsne$cluster_assignments!=ii)
  
  # Pull columns with those indices from the experimental data matrix
  all_gene_expression <- groupedSCS$Combined@fdata # note this was also done earlier
  inside_cells_gene_expression<-all_gene_expression[,inside_cluster_idxs]
  outside_cells_gene_expression<-all_gene_expression[,outside_cluster_idxs]
  # Calculate average and differential gene expression
  inside_cells_gene_expression<-rowMeans(inside_cells_gene_expression,dim=1)
  outside_cells_gene_expression<-rowMeans(outside_cells_gene_expression,dim=1)
  differential_expression<-inside_cells_gene_expression/outside_cells_gene_expression
  # Get names for all genes
  all_gene_names<-rownames(all_gene_expression)
  all_gene_names_short<-str_replace_all(all_gene_names,'_.*','')
  # set up dataframe with that info
  differential_cluster_df <- data_frame(all_gene_names=all_gene_names, 
                                           cond1_average_gene_expression=inside_cells_gene_expression,
                                           cond2_average_gene_expression=outside_cells_gene_expression,
                                           differential_expression=differential_expression,
                                           differential_expression_inv=1/differential_expression,
                                           original_nr=factor(1:length(all_gene_names)))
  # Now sort
  differential_cluster_df<-differential_cluster_df[order(-differential_cluster_df$differential_expression),]
  #differential_conditions_df$n123<-factor(differential_conditions_df$n123, levels=differential_conditions_df$n123)
  
  # Select highest values
  selected_data_df<-differential_cluster_df[1:20,]
  n123=factor(1:nrow(selected_data_df), levels=1:nrow(selected_data_df))
  n123ro=factor(1:nrow(selected_data_df), levels=nrow(selected_data_df):1) # ro = reverse order
  selected_data_df<-mutate(selected_data_df,
                           n123=n123,
                           n123ro=n123ro)
  
  # Now make a bar plot
  barplot_differential_expression(selected_data_df=selected_data_df,centers_varname='n123ro',
                                  differential_expression_varname='differential_expression',
                                  all_gene_names=all_gene_names_short,
                                  lowcol='red',highcol='firebrick4',ylabtext='Times higher in cluster',
                                  mytitle=paste('Differential gene expression cluster ',toString(ii),'',sep=''))
  # Save 'm
  ggsave(paste(directory_with_data,'plots_MW/differential_expression_cluster',toString(ii),'_higher.pdf',sep=""), width=10, height=6)
  ggsave(paste(directory_with_data,'plots_MW/differential_expression_cluster',toString(ii),'_higher.png',sep=""), width=10, height=6)
  
  # Select the lowest values
  selected_data_df_low<-differential_cluster_df[nrow(differential_cluster_df):(nrow(differential_cluster_df)-19),]
  n123=factor(1:nrow(selected_data_df_low), levels=1:nrow(selected_data_df_low))
  n123ro=factor(1:nrow(selected_data_df_low), levels=nrow(selected_data_df_low):1)  # ro = reverse order
  selected_data_df_low<-mutate(selected_data_df_low,
                               n123=n123,
                               n123ro=n123ro)
  
  # Now make a bar plot
  barplot_differential_expression(selected_data_df=selected_data_df_low,centers_varname='n123ro',
                                  differential_expression_varname='differential_expression_inv',
                                  all_gene_names=all_gene_names_short,
                                  lowcol='skyblue',highcol='midnightblue',ylabtext='Times lower in cluster',
                                  mytitle=paste('Differential gene expression cluster ',toString(ii),'',sep=''))
  
  # Save 'm
  ggsave(paste(directory_with_data,'plots_MW/differential_expression_cluster',toString(ii),'_lower.pdf',sep=""), width=10, height=6)
  ggsave(paste(directory_with_data,'plots_MW/differential_expression_cluster',toString(ii),'_lower.png',sep=""), width=10, height=6)

}

# ======================================================================================================
# Checking out differential gene expression using built-in raceid code
# ======================================================================================================


cdiff <- clustdiffgenes(groupedSCS$Combined,pvalue=.01)
# now e.g. cdiff$cl.1 gives a table with
# - mean.ncl mean of not in cluster
# - mean.cl  mean of in clusrter
# - fc fold change inside-vs-outside
# - pv p value

# First store these values in text files
for ( n in names(cdiff) ) write.table(
  data.frame(GENEID=rownames(cdiff[[n]]), cdiff[[n]]), paste(
    paste("cell_clust_diff_genes",sub("\\.","\\_",n), sep="_"), ".xls",
    sep=""), row.names=FALSE, col.names=TRUE, sep="\t", quote=FALSE)

# And then plot the outputs
nr_of_clusters_race <- length(cdiff)
for (ii in seq(1,nr_of_clusters_race)) {
  
  current_cluster_name <- paste('cl.',toString(ii),sep="")
  current_cluster_table <- cdiff[[current_cluster_name]]
  
  # Get names for all genes
  all_gene_names_race<-rownames(current_cluster_table)
  all_gene_names_race_short<-str_replace_all(all_gene_names_race,'_.*','')
  # set up dataframe with that info
  differential_cluster_race_df <- data_frame(all_gene_names=all_gene_names_race, 
                                            cond1_average_gene_expression=current_cluster_table$mean.cl,
                                            cond2_average_gene_expression=current_cluster_table$mean.ncl,
                                            differential_expression=current_cluster_table$fc,
                                            differential_expression_inv=1/current_cluster_table$fc,
                                            original_nr=factor(1:length(all_gene_names)))
  # Now sort
  differential_cluster_race_df<-differential_cluster_race_df[order(-differential_cluster_race_df$differential_expression),]
  #differential_conditions_df$n123<-factor(differential_conditions_df$n123, levels=differential_conditions_df$n123)
  
  # Select highest values
  selected_data_df<-differential_cluster_race_df[1:20,]
  n123=factor(1:nrow(selected_data_df), levels=1:nrow(selected_data_df))
  n123ro=factor(1:nrow(selected_data_df), levels=nrow(selected_data_df):1) # ro = reverse order
  selected_data_df<-mutate(selected_data_df,
                           n123=n123,
                           n123ro=n123ro)
  
  # Now make a bar plot
  barplot_differential_expression(selected_data_df=selected_data_df,centers_varname='n123ro',
                                  differential_expression_varname='differential_expression',
                                  all_gene_names=all_gene_names_race_short,
                                  lowcol='red',highcol='firebrick4',ylabtext='Times higher in cluster',
                                  mytitle=paste('Differential gene expression cluster ',toString(ii),'',sep=''))
  # Save 'm
  ggsave(paste(directory_with_data,'plots_MW/differential_expression_race_cluster',toString(ii),'_higher.pdf',sep=""), width=10, height=6)
  ggsave(paste(directory_with_data,'plots_MW/differential_expression_race_cluster',toString(ii),'_higher.png',sep=""), width=10, height=6)
  
  # Select the lowest values
  selected_data_df_low<-differential_cluster_race_df[nrow(differential_cluster_race_df):(nrow(differential_cluster_race_df)-19),]
  n123=factor(1:nrow(selected_data_df_low), levels=1:nrow(selected_data_df_low))
  n123ro=factor(1:nrow(selected_data_df_low), levels=nrow(selected_data_df_low):1)  # ro = reverse order
  selected_data_df_low<-mutate(selected_data_df_low,
                               n123=n123,
                               n123ro=n123ro)
  
  # Now make a bar plot
  barplot_differential_expression(selected_data_df=selected_data_df_low,centers_varname='n123ro',
                                  differential_expression_varname='differential_expression_inv',
                                  all_gene_names=all_gene_names_race_short,
                                  lowcol='skyblue',highcol='midnightblue',ylabtext='Times lower in cluster',
                                  mytitle=paste('Differential gene expression cluster ',toString(ii),'',sep=''))
  
  # Save 'm
  ggsave(paste(directory_with_data,'plots_MW/differential_expression_race_cluster',toString(ii),'_lower.pdf',sep=""), width=10, height=6)
  ggsave(paste(directory_with_data,'plots_MW/differential_expression_race_cluster',toString(ii),'_lower.png',sep=""), width=10, height=6)

}

# ====================================================================================================
# Now some correlation analysis one gene of interest vs. all others
# ====================================================================================================

# Method by Bas to correlate one vs. all

MYGENENAME<-'^NGF_'
MYGENENAME<-'^GATA4_'
MYGENENAME<-'^TFAP2A_'

correlationResult = analyseCorrelation(config, groupedSCS, geneName=MYGENENAME, groupName='Combined', removeNoExpressCells = 'no', percentage=0) # default = 20
# Create plot of less stringent correlation method ------------------------------------------------
vulcan_out1<-plotCorrelationVolcano(config, correlationResult=correlationResult, coefficientCutoff=0.25, pValueCutoff=1/10^5, outputMode='show')
p1<-last_plot()
p1<-p1+xlim(-1,1)
p1
ggsave(paste(directory_with_data,'plots_MW/vulcano/MW_',MYGENENAME,'_',Sys.time(),'.pdf',sep=""), width=10, height=6)
ggsave(paste(directory_with_data,'plots_MW/vulcano/MW_',MYGENENAME,'_',Sys.time(),'.png',sep=""), width=10, height=6)
# Second vulcano with different cutoff
vulcan_out2<-plotCorrelationVolcano(config, correlationResult=correlationResult, coefficientCutoff=0.4, pValueCutoff=0.05, outputMode='show')
p2<-last_plot()
p2<-p2+xlim(-1,1)
p2
ggsave(paste(directory_with_data,'plots_MW/vulcano/MW_',MYGENENAME,'_',Sys.time(),'.pdf',sep=""), width=10, height=6)
ggsave(paste(directory_with_data,'plots_MW/vulcano/MW_',MYGENENAME,'_',Sys.time(),'.png',sep=""), width=10, height=6)

# Put output into excel file
# First sort
vulcan_out1_sorted<-vulcan_out1[order(-vulcan_out1$correlation),]
# Then output
#library(xlsx)
write.xlsx(vulcan_out1_sorted, paste0(directory_with_data,'plots_MW/vulcano/correlation_', MYGENENAME, '_withall.xlsx'))

df<-data_frame(x=vulcan_out1_sorted$correlation,y=-log(vulcan_out1_sorted$pValue))
ggplot(data=df)+
  geom_point(aes(x=x,y=y))

# Create more stringent Bas params  ------------------------------------------------
# We concluded it might not be a good strategy to select for expression of the gene of 
# interest. So these parameter settings might not include all correlated genes that 
# one can find.
#correlationResult_stringent = analyseCorrelation(config, groupedSCS, geneName=MYGENENAME, groupName='Combined', removeNoExpressCells = 'goi', percentage=20) # default = 20
#plotCorrelationVolcano(config, correlationResult=correlationResult_stringent, coefficientCutoff=0.25, pValueCutoff=1/10^5, outputMode='show')
#p3<-last_plot()
#p3<-p3+xlim(-1,1)
#p3
#ggsave(paste(directory_with_data,'plots_MW/vulcano/Bas_',MYGENENAME,'_',Sys.time(),'.pdf',sep=""), width=10, height=6)
#ggsave(paste(directory_with_data,'plots_MW/vulcano/Bas_',MYGENENAME,'_',Sys.time(),'.png',sep=""), width=10, height=6)

# ====================================================================================================
# Method by me to correlate one vs. all

thedatatoday <- groupedSCS$Combined@ndata
gene_names <- rownames(thedatatoday)
nr_of_genes <- nrow(thedatatoday)

gene_of_interest_idx <- get_idx_for_gene_name(gene_names,MYGENENAME)
idxs_of_all_other<-c(1:(gene_of_interest_idx-1),(gene_of_interest_idx+1):nr_of_genes)

gene_of_interest_name <- gene_names[gene_of_interest_idx]

one_gene                   <-thedatatoday[gene_of_interest_idx,]
all_other_genes_expression <-thedatatoday[idxs_of_all_other,]

# this correlation function correlates the expression of two genes against 
# each other. by using the apply function, it can be used for obtaining
# multiple correlation coefficients for 1 vs. many genes.
# cutoff = -1
# cutoff = in range 0 to 1
#     gene of interest w/ values is selected, while : throw out genes 
corrfn<-function(other_gene_expr,one_gene_expr,cutoff=1){
  
  # identify selection criteria
  if (cutoff==-1) {
      idx_sel<-which(one_gene_expr>0.1 & other_gene_expr>0.1)
  } else if(cutoff<=1 & cutoff>0) {
      idx_sel<-which(one_gene_expr>0.1 & sum(as.numeric((other_gene_expr>0.1)))/length(other_gene_expr)>cutoff )
  } else if (cutoff==0){
      idx_sel<-which(one_gene_expr>0.1)
  } else {  stop()  }
  
  # perform correlation using selection criteria
  if (length(idx_sel)>2) {
    out<-cor.test(one_gene_expr[idx_sel],other_gene_expr[idx_sel])
    mycorr<-c(out$estimate, out$p.value)
    names(mycorr)<-c("cor","pvalue")
  } else {
    mycorr<-NaN
  }
  
  # return correlation
  return(mycorr)
}

# How many cells have non-zero value of gene of interest?
p<-histogram(as.numeric(one_gene),
          xlab = paste('Gene expression ',gene_of_interest_name,sep=""))
ggsave(paste(directory_with_data,'plots_MW/vulcano/Histogram_',MYGENENAME,'_',Sys.time(),'.pdf',sep=""), width=10, height=6)
ggsave(paste(directory_with_data,'plots_MW/vulcano/Histogram_',MYGENENAME,'_',Sys.time(),'.png',sep=""), width=10, height=6)
p

# perform correlations
all_other_genes_expression_mat<-as.matrix(all_other_genes_expression)
one_gene_vec<-as.numeric(one_gene)
my_correlations<-apply(all_other_genes_expression_mat,2,corrfn,one_gene_expr=one_gene_vec)

#my_correlations<-apply(all_other_genes_expression_sel_mat,FUN=corrfn,other_gene_expr=one_gene_sel_vec,MARGIN=1)
#test_result<-    apply(test_matrix,2,myfun,y=c(1,2,3))
# histogram(my_correlations)


# cor.test(plot_df$x,plot_df$y)


# ===================================================================
# test code
# ===================================================================

p<-ggplot()+
  xlab('my one gene')+
  ylab('other gene x')+
  coord_fixed(ratio = 1)+
  give_better_textsize_plot(15)

failed<-0
correlations<-list()
for (idx in seq(1,5)){
  #idx=2
  
  # select a row (=gene)
  other_gene_sel <- all_other_genes_expression_sel_mat[idx,]
  
  # convert index to numeric vectors
  one_gene_sel_num    <- as.numeric(one_gene_sel)
  other_gene_sel_num  <- as.numeric(other_gene_sel)
  
  # remove zero columns from data
  idx_both<-which(one_gene_sel_num>0.1 & other_gene_sel_num>0.1)
  
  idx_both
  if (length(idx_both)>1){
    # scatter plot 
    plot_df=data_frame(x=one_gene_sel_num[idx_both],y=other_gene_sel_num[idx_both])
    p<-p+geom_point(data=plot_df,aes(x=x,y=y),color=col_vector[idx],shape=idx,size=5)
    
    # perform correlation
    out<-cor.test(one_gene_sel_num[idx_both],other_gene_sel_num[idx_both])
    
    correlations[length(correlations)+1]<-out$estimate
  }else{
    print("Not enough data")
    failed<-failed+1
  }
}

correlations
failed
p



