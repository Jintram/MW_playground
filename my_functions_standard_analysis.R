
get_idx_for_gene_name<-function(gene_names,name_str) {
  
  # find it
  gene_idx<-which(grepl(name_str,gene_names))
  
  # do some checks
  if (length(gene_idx)==0){
    print(paste(name_str, ": None gene found with that name"))
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
