


# Now let's try to use the DEseq package on Arwa's dataset.

# Obtain data ================================================================================================

# define sets
indices_cluster5_mutant    <- which(dataframe_cells$cluster==5 & dataframe_cells$condition==2)
indices_cluster5_wildtype  <- which(dataframe_cells$cluster==5 & dataframe_cells$condition==1)

# Get the gene expression of the two subsets
gene_expression_set1  <- all_gene_expression_raw[,indices_set1]
gene_expression_set2  <- all_gene_expression_raw[,indices_set2]

# Define experimental design (required DEseq input) =========================================================

# Actually perform the analysis ==============================================================================
dds<-DESeq(dds)
results<-results(dds)