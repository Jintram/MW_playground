
get_idx_for_gene_name<-function(gene_names,name_str) {
  
  # find it
  gene_idx<-which(grepl(name_str,gene_names))
  
  # do some checks
  if (length(gene_idx)==0){
    print(paste(name_str, ": No gene found with that name"))
    gene_idx=NaN
    next
  } else if (length(gene_idx)>1) {
    print(paste(name_str, ": Multiple genes found with that name")) 
    gene_idx=NaN
    next
  } else {
    print(paste(gene_names[gene_idx], ' found gene with that name; index= ', gene_idx, '.'),sep="")
  }
  
  # return index
  return(gene_idx)

}

# all_gene_expression needs to have rownames that give the gene names
get_expression_gene <- function(all_gene_expression, name_str) {
  
  # get index for gene of interest based search string
  gene_idx<-get_idx_for_gene_name(gene_names=rownames(all_gene_expression),name_str)
  
  # if found
  if (!is.nan(gene_idx)){
    
    # select the expression of the gene from the table
    the_gene_expression <- all_gene_expression[gene_idx,]
    
  } else {
   
    # throw error if gene not found
    stop('Error, couldn\'t find gene.')
     
  }
    
  return(the_gene_expression)
  
}

## get_differential_gene_expression()
# calculates fold-change as set1/set2
# indices_set1 : indices of columns (cells) that should be selected for set 1
# indices_set2 : indices of columns (cells) that should be selected for set 2
# all_gene_expression : matrix with all gene expression, normalized as grun/oudenaarden (raceID2)
# all_gene_expression_raw : same as previous, but raw transcript counts
# method='min' : pertains to rescaling of expression values; see comments in function
# pcutoff=0.01 : cutoff value for p values, entries above will be filtered out
# -- written by MW, 03-2019
get_differential_gene_expression <- function(indices_set1,indices_set2,all_gene_expression,all_gene_expression_raw,method='min',pcutoff=0.01) {
  
  # Get the gene expression of the two subsets
  gene_expression_set1  <- all_gene_expression[,indices_set1]
  gene_expression_set2  <- all_gene_expression[,indices_set2]
  
  # Calculate mean and median expression values
  mean_gene_expression_set1   <- rowMeans(as.matrix(gene_expression_set1))
  mean_gene_expression_set2   <- rowMeans(as.matrix(gene_expression_set2))
  median_gene_expression_set1 <- rowMedians(as.matrix(gene_expression_set1))
  median_gene_expression_set2 <- rowMedians(as.matrix(gene_expression_set2))
  
  # Calculate standard deviations
  sd_gene_expression_set1 <- rowSds(as.matrix(gene_expression_set1))
  sd_gene_expression_set2 <- rowSds(as.matrix(gene_expression_set2))
  
  # Now also calculate counts of positive in cells (similar to done before)
  detectioncount_gene_expression_set1<-rowSums(1*(gene_expression_set1>0.1))
  detectioncount_gene_expression_set2<-rowSums(1*(gene_expression_set2>0.1))
  
  # Calculate rescaling factor to rescale data to get cell total transcript counts to be rescaled
  # to the set median or minimum.
  # Calculate rescaling factors as done Grun & Van Oudenaarden (no pro'lly stands for normalization)
  # IMPORTANT NOTE: this value critically depends on whether "min" or "median" is used for normalization;
  # this is done in the raceid code, where ndata is calculated
  #
  # chose rescaling factor to continue with
  if (identical(method,'min')) {
    no_set1 <- (median(apply(all_gene_expression_raw[,indices_set1],2,sum))) / (min(apply(all_gene_expression_raw,2,sum)))
    no_set2 <- (median(apply(all_gene_expression_raw[,indices_set2],2,sum))) / (min(apply(all_gene_expression_raw,2,sum)))
  } else if (identical(method,'min_same')) {
      no_set1 <- (median(apply(all_gene_expression_raw[,indices_set1],2,sum))) / (min(apply(all_gene_expression_raw,2,sum)))
      no_set2 <- no_set1
  } else if (identical(method,'median')) {
    no_set1 <- (median(apply(all_gene_expression_raw[,indices_set1],2,sum))) / (median(apply(all_gene_expression_raw,2,sum)))
    no_set2 <- (median(apply(all_gene_expression_raw[,indices_set2],2,sum))) / (median(apply(all_gene_expression_raw,2,sum)))
  } else if (identical(method,'bug')){
    # Do not use this; I think there is a bug here; but this code was used for testing purposes
    no_set1<-(median(apply(all_gene_expression_raw[,indices_set1],2,sum))) / (min(apply(all_gene_expression_raw+.1,2,sum)))
    no_set2<-(median(apply(all_gene_expression_raw[,indices_set2],2,sum))) / (min(apply(all_gene_expression_raw+.1,2,sum)))
  } else if (identical(method,'bug_same')){
    # Do not use this; I think this method was mistakenly used in original raceID2 code
    no_set1<-(median(apply(all_gene_expression_raw[,indices_set1],2,sum))) / (min(apply(all_gene_expression_raw+.1,2,sum)))
    no_set2<-no_set1
  } else if (identical(method,'none')) {
    no_set1<-1
    no_set2<-1
  } else {
    stop('Invalid method supplied.')
  }
  ratio_rescaling_factors <- no_set1/no_set2 # interesting to see whole-set expression difference (not used)
  
  # Now rescale them accordingly
  mean_gene_expression_set1_rescaled <- no_set1*mean_gene_expression_set1
  mean_gene_expression_set2_rescaled <- no_set2*mean_gene_expression_set2
  
  # Now count the differential expression
  differential_gene_expression <- mean_gene_expression_set1_rescaled / mean_gene_expression_set2_rescaled
  
  # Now calculate standard deviations
  differential_gene_expression_stdev <- differential_gene_expression*
    sqrt((sd_gene_expression_set1*abs(no_set1)/mean_gene_expression_set1_rescaled)^2+
           (sd_gene_expression_set2*abs(no_set2)/mean_gene_expression_set2_rescaled)^2)
  
  # now calculate p-values
  pv <- binompval(mean_gene_expression_set2_rescaled/sum(mean_gene_expression_set2_rescaled),
                  sum(mean_gene_expression_set1_rescaled),mean_gene_expression_set1_rescaled)
  
  # create dataframe similar grun & van oudenaarden
  diff_expr_df <- data.frame(mean.set2=mean_gene_expression_set2_rescaled,mean.set1=mean_gene_expression_set1_rescaled,
                                 fc=differential_gene_expression,
                                 fc_inv=1/differential_gene_expression,
                                 pv=pv,
                                 stdev=differential_gene_expression_stdev,
                                 cellcount.set1=detectioncount_gene_expression_set1,
                                 cellcount.set2=detectioncount_gene_expression_set2,
                                 row.names = gene_names,
                                 gene_name = gene_names,
                                 stringsAsFactors = FALSE)
  
  # filter based on p values, order by fold-change, add row line number (useful bar plot)
  diff_expr_df_filterpv <- diff_expr_df[diff_expr_df$pv<pcutoff,]
  diff_expr_df_filterpv <- diff_expr_df_filterpv[order(diff_expr_df_filterpv$fc,decreasing=T),]
  diff_expr_df_filterpv <- mutate(diff_expr_df_filterpv,n123=factor(1:nrow(diff_expr_df_filterpv)))

  return(list(diff_expr_df,diff_expr_df_filterpv))  

}


















