
# Example on how to downsample gene counts for a single cell

gene_counts <- c(g1=1,g2=2,g3=1,g4=10)

my_vase <- rep(1:length(gene_counts), times=gene_counts)

sample_of_vase <- sample(my_vase, 5)

counts_of_sample <- aggregate(sample_of_vase, list(sample_of_vase), FUN=length)

gene_counts_downsampled<-gene_counts
gene_counts_downsampled[]<-0
gene_counts_downsampled[counts_of_sample$Group.1]<-counts_of_sample$x



# Now for an expression matrix
gene_counts_matrix <- matrix(c(g1=1,g2=2,g3=1,g4=10,
                               g1=2,g2=1,g3=3,g4=8),nrow=4,dimnames=list(c('g1','g2','g3','g4'),c('c1','c2')))


downsample_1cell<-function(gene_counts,target_count=5){
    my_vase <- rep(1:length(gene_counts), times=gene_counts)

    sample_of_vase <- sample(my_vase, target_count)
    
    counts_of_sample <- aggregate(sample_of_vase, list(sample_of_vase), FUN=length)
    
    gene_counts_downsampled<-gene_counts
    gene_counts_downsampled[]<-0
    gene_counts_downsampled[counts_of_sample$Group.1]<-counts_of_sample$x
    
    return(gene_counts_downsampled)
}

apply(gene_counts_matrix,2,downsample_1cell)

# easier way, but isn't applicable since we don't want replacement
sample(1:length(gene_counts), 5, prob = gene_counts/sum(gene_counts))