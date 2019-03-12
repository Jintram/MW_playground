
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







