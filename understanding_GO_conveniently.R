
# Generated using Git co-pilot
# This uses the clusterProfiler package to perform Gene Ontology (GO) enrichment analysis
# See additional information:
# https://bioconductor.org/packages/release/bioc/html/clusterProfiler.html
# https://www-liebertpub-com.proxy.library.uu.nl/doi/full/10.1089/omi.2011.0118
#
# The clusterprofiler code that co-pilot generated works fine,
# But the GSEABase code co-pilot generated does not work -- didn't extensively look at this yet.

# background_genes = shorthand_splitnames_v2( rownames(data_container$groups$flib30$dt$rdata) ) # from the mice data
# genes_of_interest = c('Nppa','Nppb')

# Load required packages
library(clusterProfiler)

# Define the function for GO enrichment analysis
# Currently, it performs a GO enrichment analysis, but the clusterProfiler package
# can also carry out GSEA analysis, e.g., or other analyses.
# See also https://bioconductor.org/packages/devel/bioc/manuals/clusterProfiler/man/clusterProfiler.pdf,
# allthough the precise statistical procedure is not shown here.
run_GO_enrichment <- function(genes_of_interest, background_genes) {
    # Perform GO enrichment analysis
    go_result <- enrichGO(gene          = genes_of_interest,
                                                OrgDb         = org.Hs.eg.db,
                                                keyType       = "SYMBOL",
                                                ont           = "BP",
                                                pvalueCutoff  = 0.05,
                                                qvalueCutoff  = 0.05,
                                                readable      = FALSE,
                                                universe      = background_genes)
    
    # Return the enriched GO terms
    return(go_result)
}

# Example usage
genes_of_interest <- c("GeneA", "GeneB", "GeneC")
background_genes <- c("GeneA", "GeneB", "GeneC", "GeneD", "GeneE", "GeneF")

go_enrichment_result <- run_GO_enrichment(genes_of_interest, background_genes)
print(go_enrichment_result)



# Define the function for GO enrichment analysis using GSEABase package
run_GO_enrichment_GSEABase <- function(genes_of_interest, background_genes) {
    # Load required packages
    library(GSEABase)
    
    # Create the gene set collection
    gene_set_collection <- GeneSetCollection(geneIds = background_genes)
    
    # Create the gene set
    gene_set <- GeneSet(geneIds = genes_of_interest)
    
    # Add the gene set to the collection
    gene_set_collection <- addGeneSet(gene_set_collection, gene_set)
    
    # Perform the enrichment analysis
    enrichment_result <- enricher(gene_set_collection, universe = background_genes)
    
    # Return the enriched GO terms
    return(enrichment_result)
}

# Example usage
genes_of_interest <- c("GeneA", "GeneB", "GeneC")
background_genes <- c("GeneA", "GeneB", "GeneC", "GeneD", "GeneE", "GeneF")

go_enrichment_result_GSEABase <- run_GO_enrichment_GSEABase(genes_of_interest, background_genes)
print(go_enrichment_result_GSEABase)