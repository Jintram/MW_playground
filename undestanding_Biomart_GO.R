# can we get GO terms for these genes?

library(biomaRt)

mart_ensembl = useMart("ensembl",dataset="hsapiens_gene_ensembl") #uses human ensembl annotations

#gets gene symbol, transcript_id and go_id for all genes annotated with GO:0007507
gene.data <- getBM(attributes=c('hgnc_symbol', 'ensembl_transcript_id', 'go_id'),
                   filters = 'go_id', values = 'GO:0007507', mart = mart_ensembl)

gene.data <- getBM(attributes=c('hgnc_symbol', 'ensembl_transcript_id', 'go_id'),
                   filters = 'with_go', values = 'GO:0007507', mart = mart_ensembl)

gene.data=getBM(attributes = c('ensembl_gene_id','external_gene_name'), 
      filters = 'go', 
      values = 'GO:0030098', 
      mart = mart_ensembl)

gene.data=getBM(attributes = c('ensembl_gene_id','external_gene_name','go_id','name_1006','namespace_1003'), 
      filters = c('external_gene_name'), 
      values = c('GAPDH'), 
      mart = mart_ensembl)

attr = listAttributes(mart_ensembl)
attr$name[grepl(pattern = 'go_', x = attr$name)]

filt = listFilters(mart_ensembl)
filt$name[grepl(pattern = 'name', x = filt$name)]

# Note that the below is "transcript centered", so it's highly
# inefficient
go_list <- getBM(attributes=c("go_id", "name_1006", "namespace_1003"),
                 filters = "go",
                 values = c("GO:0055114"),
                 mart=mart_ensembl, 
                 uniqueRows = FALSE)
go_list <- getBM(attributes=c("go_id", "name_1006", "namespace_1003"),
                 filters = "go",
                 values = c("GO:0055114"),
                 mart=mart_ensembl, 
                 uniqueRows = T)
